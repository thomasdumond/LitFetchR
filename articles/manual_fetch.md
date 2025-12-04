# Manual reference retrieval

If you are here, this is because you want to manually retrieve a list of
reference, outside of the automated task that you might have set up
after reading [LitFetchR
(Tutorial)](https://thomasdumond.github.io/LitFetchR/articles/LitFetchr.html).

## Setup

From your individual review R directory, load `LitFetchR`:

``` r
library(LitFetchR)
```

## Manual reference retrieval

Make sure to have save your search string using
[`create_save_search()`](https://thomasdumond.github.io/LitFetchR/reference/create_save_search.md)
as the following function need to access the file *search_list.txt*, see
[LitFetchR
(Tutorial)](https://thomasdumond.github.io/LitFetchR/articles/LitFetchr.html).

``` r
manual_fetch(WOS = TRUE, SCP = TRUE, PMD = TRUE)
```

After running
[`manual_fetch()`](https://thomasdumond.github.io/LitFetchR/reference/manual_fetch.md),
new unique *history_dedup\_* and *citationCSV\_* files are created and
*history_id* is updated.
