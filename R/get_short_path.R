#' Transforms a long computer path into a shorter.
#' @param path Path to the document.
#' @return Character scalar, a shorter path to use in Windows OS.
#' @keywords internal

get_short_path <- function(path) {
  shell(sprintf('for %%I in ("%s") do @echo %%~sI', path), intern = TRUE)
}
