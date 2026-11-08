#' Reads and validates the saved search strings.
#'
#' Reads "search_list.txt" and returns the saved searches as a named
#' character vector. The file is parsed in the same way as in the
#' extraction functions: blank lines are dropped, the name is the text before
#' the first `=` and the query is everything after the first `=` (so a query
#' may itself contain `=`). The function stops with an informative message
#' when the file is missing, has no search, or is malformed (a line without
#' `=`, an empty name or query, or duplicated names).
#'
#' @param path Path to "search_list.txt".
#' @param allow_empty If `TRUE`, a file without any search returns an empty
#'  named character vector instead of an error. Defaults to `FALSE`.
#'
#' @return A named character vector. The names are the search names and the
#'  values are the query strings.
#'
#' @keywords internal

read_search_list <- function(path, allow_empty = FALSE) {

  if (!is.character(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single character string.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Search list file not found: ", path, "\n",
         "Run `create_save_search()` to create and save a search string first.",
         call. = FALSE)
  }

  lines <- readLines(path, warn = FALSE)
  # Keep the original line numbers for the error messages.
  line_no <- seq_along(lines)
  keep <- nzchar(trimws(lines))
  lines <- lines[keep]
  line_no <- line_no[keep]

  if (length(lines) == 0) {
    if (allow_empty) return(stats::setNames(character(), character()))
    stop("No search found in: ", path, "\n",
         "The file is empty. Run `create_save_search()` to save a search string.",
         call. = FALSE)
  }

  problems <- character()

  # Lines without "=" cannot be split into a name and a query.
  has_equal <- grepl("=", lines, fixed = TRUE)
  if (!all(has_equal)) {
    problems <- c(problems, paste0(
      "Missing '=' (expected name=query) on line(s): ",
      paste(line_no[!has_equal], collapse = ", ")))
  }

  # Same split as the extraction functions: first "=" separates name and query.
  pos <- regexpr("=", lines, fixed = TRUE)
  nm <- ifelse(has_equal, substr(lines, 1, pos - 1), lines)
  query <- ifelse(has_equal, substr(lines, pos + 1, nchar(lines)), "")

  empty_name <- has_equal & !nzchar(trimws(nm))
  if (any(empty_name)) {
    problems <- c(problems, paste0(
      "Empty search name on line(s): ",
      paste(line_no[empty_name], collapse = ", ")))
  }

  empty_query <- has_equal & !nzchar(trimws(query))
  if (any(empty_query)) {
    problems <- c(problems, paste0(
      "Empty search query on line(s): ",
      paste(line_no[empty_query], collapse = ", ")))
  }

  dup <- has_equal & nm %in% nm[has_equal][duplicated(nm[has_equal])]
  if (any(dup)) {
    dup_names <- unique(nm[dup])
    dup_detail <- vapply(dup_names, function(n) {
      paste0("'", n, "' (lines ", paste(line_no[dup & nm == n], collapse = ", "), ")")
    }, character(1))
    problems <- c(problems, paste0(
      "Duplicated search name(s): ", paste(dup_detail, collapse = ", ")))
  }

  if (length(problems) > 0) {
    stop("Invalid search list file: ", path, "\n",
         paste0("- ", problems, collapse = "\n"), "\n",
         "Correct the file (one 'name=query' per line, unique names) ",
         "or run `create_save_search()` to rebuild it.",
         call. = FALSE)
  }

  stats::setNames(query, nm)
}
