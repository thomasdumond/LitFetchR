#' transforms a long computer path into a shorter
#' @param path path to the document
#' @return a shorter path

get_short_path <- function(path) {
  shell(sprintf('for %%I in ("%s") do @echo %%~sI', path), intern = TRUE)
}

