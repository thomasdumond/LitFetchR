#' Saves Web of Science and/or Scopus API keys in .Renviron.
#'
#' You can set WOS_API_KEY, SCP_API_KEY, or both at the same time.
#' Remember to restart the R session after saving your API keys.
#'
#' @param WOS_API_KEY The API key value for Web of Science (use quotation marks).
#' @param SCP_API_KEY The API key value for Scopus (use quotation marks).
#' @param dry_run Simulation run option.
#'
#' @return TRUE if at least one value was written, FALSE if left unchanged.
#'
#' @examples
#' save_api_keys(WOS_API_KEY = "abcd01234",
#'                SCP_API_KEY = "efgh5678",
#'                dry_run = TRUE
#'                )
#'
#' @export

save_api_keys <- function(WOS_API_KEY = NULL,
                          SCP_API_KEY = NULL,
                          dry_run = FALSE
                          ) {

  if (dry_run) {
    message('Saved key(s) WOS_API_KEY, SCP_API_KEY to -path-to-your-renvironment/.Renviron.
            Restart R for the new environment variable(s) to be available.')
    return(invisible(NULL))
  }

  # Collects keys in a list.
  keys <- list()
  if (!missing(WOS_API_KEY) && !is.null(WOS_API_KEY)) keys$WOS_API_KEY <- WOS_API_KEY
  if (!missing(SCP_API_KEY) && !is.null(SCP_API_KEY)) keys$SCP_API_KEY <- SCP_API_KEY

  # Informs the user if there is no API keys.
  if (!length(keys)) {
    stop("Please provide at least one of WOS_API_KEY or SCP_API_KEY.")
  }

  # Locates .Renviron.
  renviron_path <- Sys.getenv("R_ENVIRON_USER")
  if (!nzchar(renviron_path)) {
    renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
  }

  if (!file.exists(renviron_path)) {
    file.create(renviron_path)
  }

  # Read existing lines in Renviron.
  lines <- readLines(renviron_path, warn = FALSE)

  saved_names <- character(0)

  # Processes each provided key.
  for (name in names(keys)) {
    key <- keys[[name]]

    pattern <- paste0("^\\s*", name, "\\s*=")
    exists  <- grepl(pattern, lines)

    # If a key was already saved, asks the user if they want to replace it.
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

    # Replace with new API key.
    new_line <- sprintf("%s=%s", name, encodeString(key, quote = '"'))
    lines <- c(lines, new_line)
    saved_names <- c(saved_names, name)
  }

  # Write back if anything changed.
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
