build_sheet_name <- function(time = Sys.time()) {
  paste0("search", format(time, "%Y-%m-%d-%H%M%S"))
}
