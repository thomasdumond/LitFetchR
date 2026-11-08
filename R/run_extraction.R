#' Runs the retrieval of one platform and records what happened.
#'
#' Shared engine of [extract_wos_list()], [extract_scp_list()] and
#' [extract_pmd_list()]. It calls the platform specific search function,
#' compares the IDs found with the history, fetches the records that are new
#' and builds the rows of the run log.
#'
#' When \code{commit_history} is \code{TRUE} (standalone use), the history
#' and the run log are written before returning. When it is \code{FALSE}
#' (used by [manual_fetch()]), nothing is written: the pending history update
#' and the run log rows are attached to the returned data frame as the
#' attributes \code{pending_history} and \code{run_log}, and written by the
#' caller once the output file exists.
#'
#' If anything fails, nothing is committed for the platform. An error of
#' class \code{litfetchr_platform_error} is raised, carrying the run log rows
#' (all searches of the platform are reported as \code{error}).
#'
#' @param platform One of "wos", "scp" or "pmd".
#' @param search_list Named character vector of search strings.
#' @param directory Project directory.
#' @param run_id Run identifier (created if \code{NULL}).
#' @param commit_history Whether to write the history and the log directly.
#' @param search_fn Function \code{(search_list, state)} that runs the
#'  searches and stores, for each search \code{i}, a list with elements
#'  \code{total}, \code{ids} and optionally \code{translation} and
#'  \code{note} in \code{state$queries[[i]]}. It sets \code{state$current} to
#'  \code{i} while working on search \code{i}.
#' @param fetch_fn Function \code{(new_ids, state)} returning the data frame
#'  of the new records. It may add IDs to \code{state$failed_ids} (metadata
#'  could not be fetched, the IDs are kept out of the history) and to
#'  \code{state$unparsed_ids} (metadata could not be parsed).
#'
#' @return The data frame of the new records (see [extract_pmd_list()]).
#' @keywords internal
#' @noRd
run_extraction <- function(platform, search_list, directory, run_id,
                           commit_history, search_fn, fetch_fn) {

  short_name <- c(wos = "WoS", scp = "Scopus", pmd = "PubMed")[[platform]]
  label <- c(wos = "Web of Science", scp = "Scopus", pmd = "PubMed")[[platform]]
  if (is.null(run_id)) run_id <- make_run_id()
  search_names <- names(search_list)
  if (is.null(search_names)) search_names <- rep("", length(search_list))
  n_search <- length(search_list)

  state <- new.env(parent = emptyenv())
  state$queries <- vector("list", n_search)
  state$current <- NULL
  state$failed_ids <- character(0)
  state$unparsed_ids <- character(0)

  # Builds one log row per search of the platform.
  build_rows <- function(new_counts = NULL, error_message = NULL) {
    submitted <- vapply(search_list, function(s) {
      tryCatch(build_platform_query(s, platform), error = function(e) "")
    }, character(1), USE.NAMES = FALSE)
    done <- !vapply(state$queries, is.null, logical(1))
    status <- msg <- character(n_search)
    total <- new <- rep(NA_real_, n_search)
    translation <- counts <- character(n_search)

    for (i in seq_len(n_search)) {
      q <- state$queries[[i]]
      if (!is.null(q)) {
        total[i] <- q$total
        if (!is.null(q$translation)) translation[i] <- q$translation
        if (platform == "wos") counts[i] <- wos_database_counts(q$ids)
      }
      if (!is.null(error_message)) {
        status[i] <- "error"
        if (!is.null(state$current) && i == state$current) {
          msg[i] <- error_message
        } else if (done[i]) {
          msg[i] <- paste0("Results discarded and not added to the history ",
                           "because the platform failed: ", error_message)
        } else if (!is.null(state$current)) {
          msg[i] <- paste0("Not run because an earlier search failed: ",
                           error_message)
        } else {
          msg[i] <- error_message
        }
      } else {
        new[i] <- new_counts[i]
        notes <- character(0)
        if (!is.null(q$note) && nzchar(q$note)) notes <- q$note
        if (q$total == 0) {
          status[i] <- "no_results"
        } else {
          status[i] <- "success"
          n_found <- length(q$ids)
          n_failed <- sum(q$ids %in% state$failed_ids)
          n_unparsed <- sum(q$ids %in% state$unparsed_ids)
          if (n_found < q$total) {
            status[i] <- "partial"
            notes <- c(notes, paste0("Only ", n_found, " of ", q$total,
                                     " results reported by the platform were ",
                                     "retrieved. The missing records are not ",
                                     "marked as seen."))
          }
          if (n_failed > 0) {
            status[i] <- "partial"
            notes <- c(notes, paste0(n_failed, " record(s) had no metadata ",
                                     "because a request failed. They are not ",
                                     "marked as seen and will be fetched again ",
                                     "on the next run."))
          }
          if (n_unparsed > 0) {
            status[i] <- "partial"
            notes <- c(notes, paste0(n_unparsed, " record(s) could not be ",
                                     "parsed and have empty metadata."))
          }
        }
        msg[i] <- paste(notes, collapse = " ")
      }
    }
    make_log_rows(run_id = run_id, platform = platform,
                  search_name = search_names, search_string = unname(search_list),
                  submitted_query = submitted,
                  pubmed_query_translation = translation,
                  total_results = total, new_records = new,
                  status = status, message = msg, wos_database_counts = counts)
  }

  tryCatch({
    search_fn(search_list, state)
    state$current <- NULL

    # Creates the search history sheet and/or
    # document and saves the unique IDs.
    # If already exists, just extract the data from it.
    sh <- create_id_history(directory)
    history_id <- sh$history_id

    query_ids <- lapply(state$queries, function(q) {
      if (is.null(q)) character(0) else as.character(q$ids)
    })
    all_ids <- unlist(query_ids, use.names = FALSE)
    new_counts <- rep(0, n_search)
    pending <- NULL

    if (length(all_ids) == 0) {
      message("No new record from ", short_name, " retrieved.")
      records <- empty_records()
    } else {
      total_results <- sum(vapply(state$queries, function(q) q$total, numeric(1)))
      # Extract the IDs retrieved previously from the in-memory workbook.
      last_list <- openxlsx::readWorkbook(history_id, sheet = "updated_id_list")
      seen <- as.character(last_list$id)
      # Gets only the new IDs (also counted per search).
      for (i in seq_len(n_search)) {
        q_new <- setdiff(unique(query_ids[[i]]), seen)
        new_counts[i] <- length(q_new)
        seen <- c(seen, q_new)
      }
      new_ids <- setdiff(unique(all_ids), as.character(last_list$id))
      message(length(new_ids), " new records found among ", total_results,
              " total results.")

      records <- fetch_fn(new_ids, state)
      rownames(records) <- NULL

      pending <- list(
        label = label,
        timestamp = format(Sys.time(), "%Y-%m-%d-%H%M%S"),
        ids = data.frame(id = all_ids,
                         search_name = rep(search_names, lengths(query_ids)),
                         stringsAsFactors = FALSE),
        new_ids = setdiff(new_ids, state$failed_ids)
      )
    }

    rows <- build_rows(new_counts = new_counts)

    if (isTRUE(commit_history)) {
      if (!is.null(pending)) commit_id_history(directory, pending)
      flush_run_log(directory, rows)
    } else {
      attr(records, "pending_history") <- pending
      attr(records, "run_log") <- rows
    }
    records

  }, error = function(e) {
    rows <- build_rows(error_message = conditionMessage(e))
    if (isTRUE(commit_history)) flush_run_log(directory, rows)
    stop(structure(
      class = c("litfetchr_platform_error", "error", "condition"),
      list(message = conditionMessage(e), call = NULL,
           platform = platform, run_log = rows, logged = isTRUE(commit_history))
    ))
  })
}

# Data frame with the 12 standard columns and no row.
empty_records <- function() {
  data.frame(author = character(), year = character(),
             title = character(), journal = character(),
             volume = character(), number = character(),
             abstract = character(), doi = character(),
             pages = character(), isbn = character(),
             source = character(), platform_id = character(),
             stringsAsFactors = FALSE)
}
