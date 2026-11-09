#' Builds the query submitted to a literature platform.
#'
#' Takes the search string entered by the user and returns the exact query
#' text sent to the API of the selected platform, before URL encoding.
#' The field tag of the platform is added (PubMed: none, Scopus:
#' `TITLE-ABS-KEY`, Web of Science: `TS`) and the exclusion operator is
#' written the way each platform needs it, outside quoted phrases:
#' `AND NOT` for Scopus and `NOT` for PubMed and Web of Science (PubMed
#' ignores the `NOT` of an `AND NOT` and returns the records to be excluded,
#' and Web of Science rejects it).
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
         pmd = and_not_to_not(search),
         wos = paste0("TS=(", and_not_to_not(search), ")"),
         scp = paste0("TITLE-ABS-KEY(", scopus_not(search), ")")
  )
}

# Applies a text transformation to the parts of a search string that are
# outside double quotes, so that exact phrases are never changed.
transform_outside_quotes <- function(x, fn) {
  quoted_pos <- gregexpr('"[^"]*"', x)
  quoted <- regmatches(x, quoted_pos)[[1]]
  outside <- fn(regmatches(x, quoted_pos, invert = TRUE)[[1]])
  paste0(c(rbind(outside, c(quoted, ""))), collapse = "")
}

# PubMed and Web of Science use "NOT". A typed "AND NOT" becomes "NOT".
and_not_to_not <- function(x) {
  transform_outside_quotes(x, function(s) {
    gsub("\\bAND\\s+NOT\\b", "NOT", s, ignore.case = TRUE)
  })
}

# Scopus requires "AND NOT" instead of "NOT". An "AND NOT" typed by the user
# is not duplicated.
scopus_not <- function(x) {
  transform_outside_quotes(x, function(s) {
    s <- gsub("\\bAND\\s+NOT\\b", "NOT", s, ignore.case = TRUE)
    gsub("\\bNOT\\b", "AND NOT", s, ignore.case = TRUE)
  })
}
