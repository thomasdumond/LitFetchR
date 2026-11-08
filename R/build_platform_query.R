#' Builds the query submitted to a literature platform.
#'
#' Takes the search string entered by the user and returns the exact query
#' text sent to the API of the selected platform, before URL encoding.
#' The field tag of the platform is added (PubMed: none, Scopus:
#' `TITLE-ABS-KEY`, Web of Science: `TS`) and, for Scopus, the Boolean
#' operator `NOT` is replaced by `AND NOT` outside quoted phrases.
#' This is the only place where the search string is modified, so that
#' the counts shown by [create_save_search()] and the records retrieved by
#' [manual_fetch()] always use the same query.
#'
#' @param search Search string entered by the user (single character string).
#' @param platform One of `"wos"`, `"scp"` or `"pmd"`.
#'
#' @return A single character string, not URL encoded.
#'
#' @keywords internal

build_platform_query <- function(search, platform = c("wos", "scp", "pmd")) {
  platform <- match.arg(platform)
  if (!is.character(search) || length(search) != 1 || is.na(search)) {
    stop("`search` must be a single character string.", call. = FALSE)
  }
  switch(platform,
         pmd = search,
         wos = paste0("TS=(", search, ")"),
         scp = paste0("TITLE-ABS-KEY(", scopus_not(search), ")")
  )
}

# Scopus requires "AND NOT" instead of "NOT". Text between double quotes
# (exact phrases) is left untouched, and an "AND NOT" typed by the user is
# not duplicated.
scopus_not <- function(x) {
  quoted_pos <- gregexpr('"[^"]*"', x)
  quoted <- regmatches(x, quoted_pos)[[1]]
  outside <- regmatches(x, quoted_pos, invert = TRUE)[[1]]
  outside <- gsub("\\bAND\\s+NOT\\b", "NOT", outside, ignore.case = TRUE)
  outside <- gsub("\\bNOT\\b", "AND NOT", outside, ignore.case = TRUE)
  paste0(c(rbind(outside, c(quoted, ""))), collapse = "")
}
