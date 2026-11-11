#' Opens a history workbook with a readable message when that fails.
#'
#' openxlsx fails with internal messages (for example "object 'sheetrId' not
#' found") when the file is open in another program or damaged.
#'
#' @param path Path of the Excel file.
#' @return A Workbook object.
#' @keywords internal
#' @noRd
load_history_workbook <- function(path) {
  tryCatch(
    suppressWarnings(openxlsx::loadWorkbook(path)),
    error = function(e) {
      stop("The history file '", basename(path), "' cannot be read. ",
           "If it is open in another program (for example Excel), close it ",
           "and run again. If the file is damaged, restore a copy or move it ",
           "away: a new history is then started and all the records are ",
           "retrieved again.\nFile: ", path, call. = FALSE)
    }
  )
}

#' Saves a history workbook and fails clearly if the file cannot be written.
#'
#' openxlsx only gives a warning (and does not save) when the file is locked,
#' for example open in Excel. That is turned into an error here so that the
#' run is reported as a problem.
#'
#' @param wb Workbook object.
#' @param path Path of the Excel file.
#' @return \code{NULL} (invisibly).
#' @keywords internal
#' @noRd
save_history_workbook <- function(wb, path) {
  fail <- function(reason) {
    stop("The history file '", basename(path), "' could not be written (",
         reason, "). If it is open in another program (for example Excel), ",
         "close it.", call. = FALSE)
  }
  tryCatch(
    openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE),
    warning = function(w) fail(conditionMessage(w)),
    error = function(e) fail(conditionMessage(e))
  )
  invisible(NULL)
}

#' Checks the `directory` argument of the exported functions.
#'
#' @param directory Value given by the user.
#' @return The normalised path of an existing directory.
#' @keywords internal
#' @noRd
check_directory <- function(directory) {
  if (is.null(directory) ||
      (is.character(directory) && length(directory) == 1 && !is.na(directory) &&
       !nzchar(directory))) {
    stop("`directory` must be provided (path to your project folder).",
         call. = FALSE)
  }
  if (!is.character(directory) || length(directory) != 1 || is.na(directory)) {
    stop("`directory` must be a single character string ",
         "(path to your project folder).", call. = FALSE)
  }
  directory <- normalizePath(directory, mustWork = FALSE)
  if (!dir.exists(directory)) stop("Directory does not exist: ", directory,
                                   call. = FALSE)
  directory
}

#' Percent-encodes a search query for use inside a URL.
#'
#' `utils::URLencode()` leaves reserved characters such as `&`, `#` and `+`
#' unchanged, so a query containing them was cut or rejected by the platform.
#'
#' @param x Query text.
#' @return Encoded text.
#' @keywords internal
#' @noRd
encode_query <- function(x) {
  utils::URLencode(x, reserved = TRUE)
}
