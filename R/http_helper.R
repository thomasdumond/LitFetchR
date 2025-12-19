#' Internal HTTP GET helper with retries when extracting data using WOS/SCP or PMD APIs
#'
#' @param url API call for the platform
#' @param headers Named character vector of headers (optional).
#' @return Character scalar. Response body (UTF-8).
#' @keywords internal
#' @noRd

get_text_retry <- function(url, headers = NULL) {
  hdrs <- if (is.null(headers)) httr::add_headers()
  else do.call(httr::add_headers, as.list(headers))

  resp <- httr::RETRY(
    "GET", url, hdrs,
    times = 6,
    pause_base = 1,
    pause_cap  = 30
  )

  if (httr::status_code(resp) == 429) {
    ra <- httr::headers(resp)[["retry-after"]]
    if (!is.null(ra) && !is.na(ra)) Sys.sleep(as.numeric(ra))
  }

  httr::stop_for_status(resp)
  httr::content(resp, as = "text", encoding = "UTF-8")
}
