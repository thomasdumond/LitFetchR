# Internal helpers for scheduling and unscheduling tasks.
#
# These helpers wrap the system calls (`schtasks` on Windows, `cronR` on
# Mac/Linux) so that every call returns its exit status and output. Callers
# must check the result before reporting success. They are kept small so they
# can be replaced by mocks when testing.

# Valid weekday codes, in the order used by `POSIXlt$wday` (0 = Sunday).
sched_weekdays <- c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT")
sched_weekday_names <- c("Sunday", "Monday", "Tuesday", "Wednesday",
                         "Thursday", "Friday", "Saturday")

is_windows <- function() .Platform$OS.type == "windows"

cron_available <- function() requireNamespace("cronR", quietly = TRUE)

# Runs a system command and returns its exit status and combined output.
# Never throws: a command that cannot be run gives status 127.
sched_run <- function(command, args = character()) {
  out <- tryCatch(
    suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE)),
    error = function(e) structure(conditionMessage(e), status = 127L)
  )
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else as.integer(status),
       output = as.character(out))
}

# Validates and normalises `when`, `time` and `day`.
# Returns a list with the cleaned values and `day_defaulted` (TRUE when the
# day was filled in from `today`).
check_schedule_args <- function(when, time, day = NULL, today = Sys.Date()) {
  if (!is.character(when) || length(when) != 1L || is.na(when)) {
    stop("`when` must be one of DAILY, WEEKLY or MONTHLY.")
  }
  when <- toupper(trimws(when))
  if (!when %in% c("DAILY", "WEEKLY", "MONTHLY")) {
    stop("`when` must be one of DAILY, WEEKLY or MONTHLY (not '", when, "').")
  }

  if (!is.character(time) || length(time) != 1L || is.na(time) ||
      !grepl("^([0-9]|[01][0-9]|2[0-3]):[0-5][0-9]$", trimws(time))) {
    stop("`time` must be in HH:MM 24-hour clock format (for example '08:00').")
  }
  time <- sprintf("%02d:%02d",
                  as.integer(sub(":.*$", "", trimws(time))),
                  as.integer(sub("^.*:", "", trimws(time))))

  day_defaulted <- FALSE
  lt <- as.POSIXlt(today)

  if (when == "DAILY") {
    if (!is.null(day)) warning("`day` is ignored when `when` is DAILY.")
    day <- NULL
  } else if (when == "WEEKLY") {
    if (is.null(day)) {
      day <- sched_weekdays[lt$wday + 1L]
      day_defaulted <- TRUE
    } else {
      if (!is.character(day) || length(day) != 1L || is.na(day)) {
        stop("`day` must be one of MON, TUE, WED, THU, FRI, SAT, SUN ",
             "when `when` is WEEKLY.")
      }
      day <- toupper(trimws(day))
      # Also accept full English names such as "Monday"
      full <- match(day, toupper(sched_weekday_names))
      if (!is.na(full)) day <- sched_weekdays[full]
      if (!day %in% sched_weekdays) {
        stop("`day` must be one of MON, TUE, WED, THU, FRI, SAT, SUN ",
             "when `when` is WEEKLY (not '", day, "').")
      }
    }
  } else {
    # MONTHLY: day of the month, limited to 1-28 so every month has that day
    if (is.null(day)) {
      day <- min(lt$mday, 28L)
      day_defaulted <- TRUE
    } else {
      num <- suppressWarnings(as.numeric(day))
      if (length(num) != 1L || is.na(num) || num != round(num) ||
          num < 1 || num > 28) {
        stop("`day` must be a day of the month between 1 and 28 ",
             "when `when` is MONTHLY.")
      }
      day <- as.integer(num)
    }
  }

  list(when = when, time = time, day = day, day_defaulted = day_defaulted)
}

# Task names cannot hold spaces (they are replaced) or path characters.
normalize_task_id <- function(task_id) {
  if (!is.character(task_id) || length(task_id) != 1L || is.na(task_id) ||
      !nzchar(trimws(task_id))) {
    stop("`task_id` must be a single non-empty character string.")
  }
  if (grepl("[\\\\/:*?\"<>|]", task_id)) {
    stop("`task_id` cannot contain any of these characters: \\ / : * ? \" < > |")
  }
  clean <- gsub("\\s+", "-", trimws(task_id))
  if (!identical(clean, task_id)) {
    message("No spaces are allowed in `task_id`, using '", clean, "' instead.")
  }
  clean
}

# Text describing a schedule, for example "weekly on Friday at 14:00".
describe_schedule <- function(when, day, time) {
  switch(when,
         DAILY = sprintf("daily at %s", time),
         WEEKLY = sprintf("weekly on %s at %s",
                          sched_weekday_names[match(day, sched_weekdays)],
                          time),
         MONTHLY = sprintf("monthly on day %s at %s", day, time),
         sprintf("%s at %s", tolower(when), time))
}

# Formats a failed scheduler call for an error message.
format_sched_failure <- function(res) {
  out <- trimws(res$output)
  out <- out[nzchar(out)]
  paste0("Exit status: ", res$status, ".",
         if (length(out)) paste0("\nScheduler output:\n", paste(out, collapse = "\n")))
}

# WINDOWS (Task Scheduler through schtasks)
############################################

# Arguments of `schtasks /Create`. No start date (/SD) is passed, so Windows
# uses today's date. The start date format depends on the regional settings
# and is a known source of failures. No days argument is passed for DAILY.
win_task_args <- function(task_id, script, when, time, day = NULL,
                          rexe = NULL) {
  if (is.null(rexe)) {
    rexe <- file.path(R.home("bin"), "Rscript.exe")
    rexe <- tryCatch(get_short_path(rexe), error = function(e) rexe)
    if (length(rexe) != 1L || !nzchar(rexe)) {
      rexe <- file.path(R.home("bin"), "Rscript.exe")
    }
  }
  log_file <- paste0(tools::file_path_sans_ext(script), ".log")
  # Same command line as taskscheduleR uses (output is appended to a log)
  task <- sprintf("cmd /c %s %s >> %s 2>&1",
                  rexe,
                  shQuote(script, type = "cmd"),
                  shQuote(log_file, type = "cmd"))
  args <- c("/Create",
            "/TN", shQuote(task_id, type = "cmd"),
            "/TR", shQuote(task, type = "cmd"),
            "/SC", when,
            "/ST", time)
  if (when %in% c("WEEKLY", "MONTHLY")) args <- c(args, "/D", day)
  args
}

win_task_create <- function(task_id, script, when, time, day = NULL) {
  sched_run("schtasks", win_task_args(task_id, script, when, time, day))
}

win_task_delete <- function(task_id) {
  sched_run("schtasks", c("/Delete", "/TN", shQuote(task_id, type = "cmd"),
                          "/F"))
}

# Reads the registered task. The XML is language independent and gives the
# recurrence. The CSV gives the next run time and status (shown as is, because
# the wording depends on the Windows language).
# Returns list(exists, output, info) where info is NULL when not found.
win_task_query <- function(task_id) {
  tn <- shQuote(task_id, type = "cmd")
  csv <- sched_run("schtasks", c("/Query", "/TN", tn, "/V", "/FO", "CSV",
                                 "/NH"))
  if (csv$status != 0L) {
    return(list(exists = FALSE, output = csv$output, info = NULL))
  }
  xml <- sched_run("schtasks", c("/Query", "/TN", tn, "/XML"))
  info <- parse_task_csv(csv$output)
  if (xml$status == 0L) {
    parsed <- tryCatch(parse_task_xml(xml$output), error = function(e) NULL)
    if (!is.null(parsed)) info <- c(info, parsed)
  }
  list(exists = TRUE, output = csv$output, info = info)
}

# Next run time and status from `schtasks /Query /V /FO CSV /NH`
parse_task_csv <- function(lines) {
  lines <- lines[grepl("^\"", lines)]
  res <- list(next_run = NA_character_, status = NA_character_)
  if (!length(lines)) return(res)
  df <- tryCatch(
    utils::read.csv(text = lines[1], header = FALSE, colClasses = "character",
                    fill = TRUE),
    error = function(e) NULL
  )
  if (is.null(df) || ncol(df) < 4L) return(res)
  res$next_run <- df[[3]]
  res$status <- df[[4]]
  res
}

# Recurrence, start and enabled flag from the task definition XML
parse_task_xml <- function(lines) {
  txt <- paste(lines, collapse = "\n")
  txt <- sub("^\\s*<\\?xml[^>]*\\?>", "", txt)
  doc <- xml2::read_xml(txt)
  xml2::xml_ns_strip(doc)
  trig <- xml2::xml_find_first(doc, ".//Triggers/CalendarTrigger")
  if (inherits(trig, "xml_missing")) return(NULL)

  start <- xml2::xml_text(xml2::xml_find_first(trig, "./StartBoundary"))
  has <- function(path) !inherits(xml2::xml_find_first(trig, path), "xml_missing")
  type <- if (has("./ScheduleByWeek")) {
    "WEEKLY"
  } else if (has("./ScheduleByMonth")) {
    "MONTHLY"
  } else if (has("./ScheduleByDay")) {
    "DAILY"
  } else {
    NA_character_
  }
  days <- character()
  if (identical(type, "WEEKLY")) {
    days <- xml2::xml_name(xml2::xml_children(
      xml2::xml_find_first(trig, "./ScheduleByWeek/DaysOfWeek")))
  } else if (identical(type, "MONTHLY")) {
    days <- xml2::xml_text(xml2::xml_children(
      xml2::xml_find_first(trig, "./ScheduleByMonth/DaysOfMonth")))
  }
  enabled_txt <- c(
    xml2::xml_text(xml2::xml_find_first(doc, ".//Settings/Enabled")),
    xml2::xml_text(xml2::xml_find_first(trig, "./Enabled"))
  )
  list(
    type = type,
    days = days,
    start_date = if (is.na(start)) NA_character_ else substr(start, 1, 10),
    start_time = if (is.na(start)) NA_character_ else substr(start, 12, 16),
    enabled = !any(tolower(enabled_txt) == "false", na.rm = TRUE)
  )
}

# Compares what is registered with what was requested.
# Returns a character vector of differences (empty when all matches).
compare_win_task <- function(info, when, day, time) {
  diffs <- character()
  if (is.null(info$type) || is.na(info$type)) {
    return("the recurrence could not be read from the registered task")
  }
  if (!identical(info$type, when)) {
    diffs <- c(diffs, sprintf("recurrence is %s instead of %s",
                              info$type, when))
  }
  if (identical(when, "WEEKLY") && identical(info$type, "WEEKLY")) {
    want <- sched_weekday_names[match(day, sched_weekdays)]
    if (!identical(info$days, want)) {
      diffs <- c(diffs, sprintf("weekday is %s instead of %s",
                                paste(info$days, collapse = ","), want))
    }
  }
  if (identical(when, "MONTHLY") && identical(info$type, "MONTHLY")) {
    if (!identical(info$days, as.character(day))) {
      diffs <- c(diffs, sprintf("day of month is %s instead of %s",
                                paste(info$days, collapse = ","), day))
    }
  }
  if (!identical(info$start_time, time)) {
    diffs <- c(diffs, sprintf("start time is %s instead of %s",
                              info$start_time, time))
  }
  if (isFALSE(info$enabled)) diffs <- c(diffs, "the task is disabled")
  diffs
}

# MAC / LINUX (cronR)
######################

# Lines of the user's crontab (empty when there is none)
cron_lines <- function() {
  x <- tryCatch(suppressWarnings(cronR::cron_ls()), error = function(e) NULL)
  if (is.null(x)) return(character())
  as.character(x)
}

# Adds the entry. Returns NULL on success or the error message.
cron_create <- function(task_id, script, when, time, day = NULL) {
  tryCatch({
    cmd <- cronR::cron_rscript(script)
    if (when == "DAILY") {
      cronR::cron_add(command = cmd, frequency = "daily", at = time,
                      id = task_id)
    } else if (when == "WEEKLY") {
      # cron counts days of the week from 0 (Sunday)
      cronR::cron_add(command = cmd, frequency = "daily", at = time,
                      days_of_week = match(day, sched_weekdays) - 1L,
                      id = task_id)
    } else {
      cronR::cron_add(command = cmd, frequency = "monthly", at = time,
                      days_of_month = day, id = task_id)
    }
    NULL
  }, error = function(e) conditionMessage(e))
}

# Removes the entry. Returns NULL on success or the error message.
cron_delete <- function(task_id) {
  tryCatch({
    cronR::cron_rm(task_id)
    NULL
  }, error = function(e) conditionMessage(e))
}

# Finds the crontab lines of a cronR job. cronR writes a comment line holding
# the id ("## id: <id>"), then other comment lines (tags, description) and
# finally the schedule line. Only that comment line is matched, so the id is
# not confused with a path or command of another job.
cron_find <- function(task_id, lines = cron_lines()) {
  esc <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", task_id)
  pat <- paste0("^\\s*#+\\s*id:\\s*", esc, "\\s*$")
  hit <- grep(pat, lines)
  if (!length(hit)) return(NULL)
  i <- hit[length(hit)]
  later <- lines[seq_along(lines) > i]
  later <- later[nzchar(trimws(later)) & !grepl("^\\s*#", later)]
  sched <- if (length(later)) later[1L] else NA_character_
  list(id_line = lines[i], schedule_line = sched)
}
