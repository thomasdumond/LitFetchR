#' Save Web of Science and/or Scopus API keys in .Renviron
#'
#' You can set WOS_API_KEY, SCP_API_KEY, or both in a single call.
#'
#' @param WOS_API_KEY The API key value for Web of Science (optional).
#' @param SCP_API_KEY The API key value for Scopus (optional).
#' @return TRUE if at least one value was written, FALSE if left unchanged.
#' @export
save_api_keys <- function(WOS_API_KEY = NULL, SCP_API_KEY = NULL) {
  # Collect provided keys -----------------------------------------------------
  keys <- list()
  if (!missing(WOS_API_KEY) && !is.null(WOS_API_KEY)) keys$WOS_API_KEY <- WOS_API_KEY
  if (!missing(SCP_API_KEY) && !is.null(SCP_API_KEY)) keys$SCP_API_KEY <- SCP_API_KEY

  if (!length(keys)) {
    stop("Please provide at least one of WOS_API_KEY or SCP_API_KEY.")
  }

  # Locate .Renviron ---------------------------------------------------------
  renviron_path <- Sys.getenv("R_ENVIRON_USER")
  if (!nzchar(renviron_path)) {
    renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
  }

  if (!file.exists(renviron_path)) {
    file.create(renviron_path)
  }

  # Read existing lines ------------------------------------------------------
  lines <- readLines(renviron_path, warn = FALSE)

  saved_names <- character(0)

  # Process each provided key -------------------------------------------------
  for (name in names(keys)) {
    key <- keys[[name]]

    pattern <- paste0("^\\s*", name, "\\s*=")
    exists  <- grepl(pattern, lines)

    if (any(exists)) {
      ans <- utils::askYesNo(
        sprintf("An entry for '%s' already exists in %s. Replace it?",
                name, renviron_path)
      )
      if (!isTRUE(ans)) {
        message("Keeping existing value for '", name, "'.")
        next
      }
      # Remove existing entry for this name
      lines <- lines[!exists]
    }

    # Append / replace with new value
    new_line <- sprintf("%s=%s", name, encodeString(key, quote = '"'))
    lines <- c(lines, new_line)
    saved_names <- c(saved_names, name)
  }

  # Write back if anything changed -------------------------------------------
  if (!length(saved_names)) {
    message("No changes made to ", renviron_path, ".")
    return(invisible(FALSE))
  }

  writeLines(lines, renviron_path)

  message(
    "Saved key(s) ", paste(saved_names, collapse = ", "),
    " to ", renviron_path, ".\n",
    "Restart R for the new environment variable(s) to be available."
  )

  invisible(TRUE)
}
