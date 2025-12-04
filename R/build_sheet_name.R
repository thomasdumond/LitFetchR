#' Create a unique name for any document
#'
#' @param time System time at the time the function is run
#'
#' @returns Return a unique name based on the system time
#'          with the format YYYY-MM-DD-HHMMSS.
#'
#' @keywords internal

build_sheet_name <- function(time = Sys.time()) {
  paste0("search", format(time, "%Y-%m-%d-%H%M%S"))
}
