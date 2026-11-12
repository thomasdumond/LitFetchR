# Extracts the metadata from the new references found on PubMed based on the search string(s) saved in "search_list.txt".

Extracts the metadata from the new references found on PubMed based on
the search string(s) saved in "search_list.txt".

## Usage

``` r
extract_pmd_list(
  search_list_path,
  directory,
  run_id = NULL,
  commit_history = TRUE
)
```

## Arguments

- search_list_path:

  Path to "search_list.txt".

- directory:

  Choose the directory in which the references identification history
  will be saved.

- run_id:

  Identifier shared by all the rows of one retrieval in "run_log.csv"
  (created if `NULL`).

- commit_history:

  If `TRUE` (default), the references identification history and the run
  log are written before returning. If `FALSE`, nothing is written: the
  pending history update and the run log rows are attached to the
  returned data frame (attributes `pending_history` and `run_log`) so
  that the caller can write them once the output file exists (used by
  [`manual_fetch()`](https://thomasdumond.github.io/LitFetchR/reference/manual_fetch.md)).

## Value

A data.frame with one row per retrieved PubMed record and columns:

- author:

  Character. Publication authors.

- year:

  Character. Publication year.

- title:

  Character. Publication title.

- journal:

  Character. Publication journal name.

- volume:

  Character. Publication journal volume.

- number:

  Character. Publication journal issue number.

- abstract:

  Character. Publication abstract.

- doi:

  Character. Publication Digital Object Identifier (DOI).

- pages:

  Character. Publication page range (e.g. "179-192").

- isbn:

  Character. ISBN for book chapters (NA for journal articles).

- source:

  Character. Data source.

- platform_id:

  Character. Publication unique identifier in data source.

If `search_list_path` does not exist, returns `NULL`. If any search
fails, an error of class `litfetchr_platform_error` is raised and
nothing is added to the history.
