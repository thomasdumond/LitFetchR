# Set default options when package is loaded
.onLoad <- function(libname, pkgname) {
  op.asysd <- list(
    shiny.launch.browser = TRUE  # always open Shiny in browser
  )
  toset <- !(names(op.asysd) %in% names(options()))
  if (any(toset)) options(op.asysd[toset])
  invisible()
}
