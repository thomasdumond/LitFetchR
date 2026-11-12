#' Writes references to a RIS file.
#'
#' RIS is read by most reference managers and screening tools, for example
#' Covidence, which does not accept CSV files. One record is written for each
#' row, always as a journal article (`TY - JOUR`), with these tags: `AU` (one
#' line per author), `PY`, `TI`, `JO` (journal), `VL`, `IS` (issue), `SP` and
#' `EP` (first and last page), `AB`, `DO`, `SN` (ISBN), `AN` (identifier on the
#' platform) and `DB` (platform or platforms). Empty values are left out. The
#' platforms give the authors as surnames separated by commas, so the text is
#' split on commas to write one `AU` line per author (a single line holding
#' all the names would be read as one author by other tools).
#'
#' The file is written in UTF-8 with Windows line endings, as RIS files usually
#' are, and line breaks inside values are replaced by spaces.
#'
#' @param x Data frame with the standard columns of the retrieved references
#'  (`author`, `year`, `title`, `journal`, `volume`, `number`, `pages`,
#'  `abstract`, `doi`, `isbn`, `source`, `platform_id`). Other columns are
#'  ignored and missing ones are treated as empty.
#' @param path Path of the file to write.
#'
#' @return The path (invisibly).
#' @keywords internal
#' @noRd
write_ris <- function(x, path) {
  std <- c("author", "year", "title", "journal", "volume", "number", "pages",
           "abstract", "doi", "isbn", "source", "platform_id")
  for (col in setdiff(std, names(x))) x[[col]] <- NA_character_

  clean <- function(v) {
    v <- as.character(v)
    v[is.na(v)] <- ""
    v <- gsub("[\r\n]+", " ", v)
    trimws(gsub("[ \t]+", " ", v))
  }
  tag <- function(t, v) {
    v <- v[nzchar(v)]
    if (length(v)) paste0(t, "  - ", v) else character()
  }

  records <- vapply(seq_len(nrow(x)), function(i) {
    r <- x[i, , drop = FALSE]
    authors <- trimws(strsplit(clean(r$author), ",", fixed = TRUE)[[1]])

    # First and last page. A last page that is not a page number
    # (for example the "+" of "1161-+") is dropped.
    pages <- gsub("\u2013|\u2014", "-", clean(r$pages))
    first <- last <- ""
    if (nzchar(pages)) {
      parts <- trimws(strsplit(pages, "-", fixed = TRUE)[[1]])
      first <- parts[1]
      if (length(parts) > 1 && grepl("^[A-Za-z0-9]+$", parts[2])) last <- parts[2]
    }

    lines <- c(
      "TY  - JOUR",
      tag("AU", authors),
      tag("PY", sub("^([0-9]{4}).*$", "\\1", clean(r$year))),
      tag("TI", clean(r$title)),
      tag("JO", clean(r$journal)),
      tag("VL", clean(r$volume)),
      tag("IS", clean(r$number)),
      tag("SP", first),
      tag("EP", last),
      tag("AB", clean(r$abstract)),
      tag("DO", clean(r$doi)),
      tag("SN", clean(r$isbn)),
      tag("AN", clean(r$platform_id)),
      tag("DB", clean(r$source)),
      "ER  - "
    )
    paste(lines, collapse = "\r\n")
  }, character(1))

  text <- paste0(paste(records, collapse = "\r\n\r\n"), "\r\n")
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), con)
  invisible(path)
}
