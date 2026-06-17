# Manual literature retrieval.

Retrieves references corresponding to the saved search string(s) on up
to three platforms (e.g. [Web of
Science](https://clarivate.com/academia-government/scientific-and-academic-research/research-discovery-and-referencing/web-of-science/),
[Scopus](https://www.elsevier.com/en-au/products/scopus) and
[PubMed](https://pubmed.ncbi.nlm.nih.gov/)).

## Usage

``` r
manual_fetch(
  wos = FALSE,
  scp = FALSE,
  pmd = FALSE,
  directory,
  dedup = FALSE,
  open_file = FALSE,
  dry_run = FALSE
)
```

## Arguments

- wos:

  Runs the search on Web of Science (TRUE or FALSE).

- scp:

  Runs the search on Scopus (TRUE or FALSE).

- pmd:

  Runs the search on PubMed (TRUE or FALSE).

- directory:

  Choose the directory in which the search string is saved (Project's
  directory). That is also where the references metadata will be saved.

- dedup:

  Deduplicates the retrieved references (TRUE or FALSE).

- open_file:

  Automatically opens the CSV file after reference retrieval.

- dry_run:

  Simulation run option.

## Value

`NULL` (invisibly). Called for its side effects: Create a CSV file with
the references metadata, a history file of the references retrieved and
a history file of the deduplication (if the option is selected).

## Examples

``` r
# This is a "dry run" example.
# No references will actually be scheduled, it only shows how the function should react.
manual_fetch(wos = TRUE,
             scp = TRUE,
             pmd = TRUE,
             directory,
             dedup = TRUE,
             open_file = FALSE,
             dry_run = TRUE
             )
#> This is the message from the dry run showing what you should be
#>             seeing when the function will be used:
#>               154 total results found on Web of Science for: fish AND "vibrio harveyi" AND diagnostic
#>               Finished batch 1 of 2
#>               Finished batch 2 of 2
#>               File already exists
#>               149 new records found among 154 total results.
#>               24 total results found on Scopus for: fish AND "vibrio harveyi" AND diagnostic
#>               Finished batch 1 of 1
#>               File already exists
#>               22 new records found among 24 total results.
#>               110 total results found on PubMed for: fish AND "vibrio harveyi" AND diagnostic
#>               Finished batch 1 of 1
#>               File already exists
#>               104 new records found among 110 total results.
#>               Finished batch 1 of 1 (104 / 104 records retrieved)
#>               Warning: The following columns are missing: record_id
#>               formatting data...
#>               identifying potential duplicates...
#>               identified duplicates!
#>               275 citations loaded...
#>               27 duplicate citations removed...
#>               248 unique citations remaining!
#>               Deduplication script has been executed,
#>                         concatenated deduplicated references had been exported.
#> 
#>               Warning message:
#>               In add_missing_cols(raw_citations) :
#>                 Search contains missing values for the record_id column. A record_id will be created using row numbers

```
