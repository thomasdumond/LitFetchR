#' Internal HTTP GET helper with retries when
#'  extracting data using WOS/SCP or PMD APIs
#'
#' @param url API call for the platform
#' @param headers Named character vector of headers (optional).
#' @return Character scalar. Response body (UTF-8).
#' @keywords internal
#' @noRd

get_text_retry <- function(url, headers = NULL) {
  hdrs <- if (is.null(headers)) httr::add_headers()
  else do.call(httr::add_headers, as.list(headers))

  resp <- NULL
  for (attempt in seq_len(3)) {
    resp <- httr::RETRY(
      "GET", url, hdrs,
      httr::timeout(60),
      times = 6,
      pause_base = 1,
      pause_cap  = 30,
      terminate_on = c(400L, 401L, 403L)
    )

    status <- httr::status_code(resp)

    if (status %in% c(401L, 403L)) {
      stop("Authentication failed (HTTP ", status, "). ",
           "Check your API key with save_api_keys().", call. = FALSE)
    }

    if (status != 429) break

    ra <- httr::headers(resp)[["retry-after"]]
    wait <- if (!is.null(ra) && !is.na(as.numeric(ra))) as.numeric(ra) else 60
    if (attempt < 3) {
      message("Rate limited (429). Waiting ", wait, "s before retry ", attempt + 1, " of 3...")
      Sys.sleep(wait)
    }
  }

  httr::stop_for_status(resp)
  httr::content(resp, as = "text", encoding = "UTF-8")
}
