setwd("/home/runner/work/LitFetchR/LitFetchR/docs/reference")
search_list_path <- '/home/runner/work/LitFetchR/LitFetchR/docs/reference/search_list.txt'
LitFetchR::manual_fetch(WOS = TRUE, SCP = TRUE, PMD = TRUE, dedup = FALSE, open_file = FALSE)
