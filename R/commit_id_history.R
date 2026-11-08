#' Commits the references identification history of one platform.
#'
#' Appends the raw IDs of a retrieval to the \code{id_log} sheet and the new
#' IDs to the \code{updated_id_list} sheet of "history_id.xlsx". This is the
#' only place where the extractors' results are marked as seen. It is called
#' by [manual_fetch()] once the output file exists, so that a failed
#' retrieval or a failed output never marks records as seen.
#'
#' Files created by earlier versions of the package (3 columns in
#' \code{id_log}) are upgraded in place: the \code{search_name} column is
#' added and left empty for the old rows.
#'
#' @param directory Directory of the project (where "history_id.xlsx" is).
#' @param pending A list with elements \code{label} (platform label stored in
#'  \code{id_log}), \code{timestamp}, \code{ids} (data frame with columns
#'  \code{id} and \code{search_name}) and \code{new_ids} (IDs to add to
#'  \code{updated_id_list}).
#'
#' @return \code{NULL} (invisibly).
#' @keywords internal
#' @noRd
commit_id_history <- function(directory, pending) {
  history_id_path <- file.path(directory, "history_id.xlsx")
  if (!file.exists(history_id_path)) create_id_history(directory)
  history_id <- openxlsx::loadWorkbook(history_id_path)
  if (!"id_log" %in% names(history_id)) {
    openxlsx::addWorksheet(history_id, "id_log")
  }

  # Appends raw IDs to the id_log sheet with platform, timestamp and search.
  log_cols <- c("id", "platform", "timestamp", "search_name")
  existing_log <- openxlsx::readWorkbook(history_id, sheet = "id_log")
  for (col in setdiff(log_cols, names(existing_log))) {
    existing_log[[col]] <- rep("", nrow(existing_log))
  }
  existing_log <- existing_log[, log_cols, drop = FALSE]
  existing_log[] <- lapply(existing_log, as.character)
  new_log_rows <- data.frame(id = as.character(pending$ids$id),
                             platform = pending$label,
                             timestamp = pending$timestamp,
                             search_name = as.character(pending$ids$search_name),
                             stringsAsFactors = FALSE)
  openxlsx::writeData(history_id, "id_log", rbind(existing_log, new_log_rows))

  # Adds the new IDs to the list of IDs seen (only those not already in it).
  last_list <- openxlsx::readWorkbook(history_id, sheet = "updated_id_list")
  new_ids <- setdiff(as.character(pending$new_ids), as.character(last_list$id))
  updated_list <- rbind(last_list, data.frame(id = new_ids))
  openxlsx::writeData(history_id, sheet = "updated_id_list", x = updated_list)

  openxlsx::saveWorkbook(history_id, file = history_id_path, overwrite = TRUE)
  invisible(NULL)
}
