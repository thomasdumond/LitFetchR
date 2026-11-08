#' Columns of the run log.
#'
#' @return Character vector with the column names of "run_log.csv".
#' @keywords internal
#' @noRd
run_log_columns <- function() {
  c("run_id", "timestamp", "litfetchr_version", "r_version", "platform",
    "search_name", "search_string", "submitted_query",
    "pubmed_query_translation", "total_results", "new_records",
    "status", "message", "wos_database_counts")
}

#' Creates a run identifier shared by all rows of one retrieval.
#'
#' @return Character scalar (timestamp string).
#' @keywords internal
#' @noRd
make_run_id <- function() {
  format(Sys.time(), "%Y-%m-%d-%H%M%S")
}

#' Builds rows for the run log.
#'
#' All vector arguments are recycled to a common length. Empty or missing
#' text values are stored as empty strings so that the CSV stays simple.
#'
#' @param run_id Run identifier.
#' @param platform One of "wos", "scp" or "pmd".
#' @param search_name Name of the saved search.
#' @param search_string Search string as read from "search_list.txt".
#' @param submitted_query Exact query submitted to the platform.
#' @param pubmed_query_translation PubMed query translation (PubMed only).
#' @param total_results Number of results reported by the platform.
#' @param new_records Number of records not seen in previous runs.
#' @param status "success", "partial", "no_results" or "error".
#' @param message Error text or note.
#' @param wos_database_counts Web of Science records per source database.
#'
#' @return A data.frame with the columns of `run_log_columns()`.
#' @keywords internal
#' @noRd
make_log_rows <- function(run_id, platform, search_name, search_string,
                          submitted_query = "", pubmed_query_translation = "",
                          total_results = NA_real_, new_records = NA_real_,
                          status, message = "", wos_database_counts = "") {
  pkg_version <- tryCatch(as.character(utils::packageVersion("LitFetchR")),
                          error = function(e) "unknown")
  clean_text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    gsub("[\r\n]+\\s*", " ", x)
  }
  data.frame(
    run_id = run_id,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    litfetchr_version = pkg_version,
    r_version = as.character(getRversion()),
    platform = platform,
    search_name = clean_text(search_name),
    search_string = clean_text(search_string),
    submitted_query = clean_text(submitted_query),
    pubmed_query_translation = clean_text(pubmed_query_translation),
    total_results = as.numeric(total_results),
    new_records = as.numeric(new_records),
    status = status,
    message = trimws(clean_text(message)),
    wos_database_counts = clean_text(wos_database_counts),
    stringsAsFactors = FALSE
  )
}

#' Counts Web of Science records per source database.
#'
#' The source database is the prefix of the UID before the colon
#' (e.g. "WOS:000123" gives "WOS"). A UID without prefix is counted
#' as "unknown".
#'
#' @param uids Character vector of Web of Science UIDs.
#' @return Character scalar such as "WOS:120|MEDLINE:30" (empty if no UID).
#' @keywords internal
#' @noRd
wos_database_counts <- function(uids) {
  uids <- unique(as.character(uids))
  uids <- uids[!is.na(uids)]
  if (length(uids) == 0) return("")
  prefix <- ifelse(grepl(":", uids, fixed = TRUE), sub(":.*$", "", uids), "unknown")
  prefix[!nzchar(prefix)] <- "unknown"
  counts <- table(prefix)
  counts <- counts[order(-as.integer(counts), names(counts))]
  paste0(names(counts), ":", as.integer(counts), collapse = "|")
}

#' Appends rows to the persistent run log "run_log.csv".
#'
#' Creates the file with a header if missing and appends otherwise. If the
#' existing file has a different header (older version or edited file), its
#' rows are kept and re-written with the current columns first. If the
#' existing file cannot be read, it is copied to a backup file and a new
#' log is started.
#'
#' @param directory Project directory in which "run_log.csv" is saved.
#' @param rows Data frame created with `make_log_rows()`.
#'
#' @return The path of the run log (invisibly).
#' @keywords internal
#' @noRd
write_run_log <- function(directory, rows) {
  if (is.null(rows) || nrow(rows) == 0) return(invisible(NULL))
  path <- file.path(directory, "run_log.csv")
  cols <- run_log_columns()
  for (col in setdiff(cols, names(rows))) rows[[col]] <- ""
  rows <- rows[, cols, drop = FALSE]

  if (!file.exists(path) || file.size(path) == 0) {
    utils::write.csv(rows, file = path, row.names = FALSE, na = "",
                     fileEncoding = "UTF-8")
    return(invisible(path))
  }

  # Reads the header only, to decide between appending and rewriting.
  header <- tryCatch(
    names(suppressWarnings(utils::read.csv(path, nrows = 0, check.names = FALSE,
                                           fileEncoding = "UTF-8"))),
    error = function(e) NULL
  )
  old <- NULL
  if (!identical(header, cols)) {
    old <- tryCatch(
      suppressWarnings(utils::read.csv(path, colClasses = "character",
                                       check.names = FALSE,
                                       na.strings = character(0),
                                       fileEncoding = "UTF-8",
                                       stringsAsFactors = FALSE)),
      error = function(e) NULL
    )
  }

  if (!identical(header, cols) && is.null(old)) {
    backup <- file.path(directory, paste0("run_log_unreadable_",
                                          make_run_id(), ".csv"))
    file.copy(path, backup)
    warning("The existing run_log.csv could not be read. A copy was saved as ",
            basename(backup), " and a new run_log.csv was started.",
            call. = FALSE)
    utils::write.csv(rows, file = path, row.names = FALSE, na = "",
                     fileEncoding = "UTF-8")
    return(invisible(path))
  }

  if (identical(header, cols)) {
    # Same header: append without rewriting the file.
    size <- file.size(path)
    con <- file(path, "rb")
    seek(con, size - 1)
    last_byte <- readBin(con, "raw", 1)
    close(con)
    if (!identical(last_byte, as.raw(10L))) cat("\n", file = path, append = TRUE)
    utils::write.table(rows, file = path, append = TRUE, sep = ",",
                       row.names = FALSE, col.names = FALSE, qmethod = "double",
                       na = "", fileEncoding = "UTF-8")
  } else {
    # Different header: keep the old rows and rewrite the file.
    rows_chr <- as.data.frame(lapply(rows, as.character), stringsAsFactors = FALSE)
    combined <- dplyr::bind_rows(old, rows_chr)
    combined <- combined[, c(cols, setdiff(names(combined), cols)), drop = FALSE]
    tmp <- tempfile(pattern = "run_log_", tmpdir = directory, fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    utils::write.csv(combined, file = tmp, row.names = FALSE, na = "",
                     fileEncoding = "UTF-8")
    file.copy(tmp, path, overwrite = TRUE)
  }
  invisible(path)
}

#' Writes the run log without ever stopping the retrieval.
#'
#' A failure to write the log (for example the file is open in Excel)
#' is reported as a warning only.
#'
#' @inheritParams write_run_log
#' @return \code{NULL} (invisibly).
#' @keywords internal
#' @noRd
flush_run_log <- function(directory, rows) {
  tryCatch(
    write_run_log(directory, rows),
    error = function(e) {
      warning("Could not write run_log.csv: ", conditionMessage(e),
              call. = FALSE)
    }
  )
  invisible(NULL)
}
