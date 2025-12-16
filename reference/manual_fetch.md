# manual literature fetch

manual literature fetch

## Usage

``` r
manual_fetch(
  WOS = TRUE,
  SCP = TRUE,
  PMD = TRUE,
  dedup = FALSE,
  open_file = FALSE
)
```

## Arguments

- WOS:

  choose to search on Web of Science (TRUE or FALSE)

- SCP:

  choose to search on Scopus (TRUE or FALSE)

- PMD:

  choose to search on PubMed (TRUE or FALSE)

- dedup:

  choose to proceed to the deduplication of the references or not

- open_file:

  choose to automatically open the CSV file after reference retrieval

## Value

create a CSV file with the literature metadata, a history file of the
references retreived and a history file of the deduplication

## Examples
