#' Transforms a long computer path into a shorter.
#' @param path Path to the document.
#' @return A shorter path.
#' @keywords internal

get_short_path <- function(path) {
  shell(sprintf('for %%I in ("%s") do @echo %%~sI', path), intern = TRUE)
}

