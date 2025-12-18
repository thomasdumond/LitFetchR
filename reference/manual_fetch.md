# Manual literature retrieval.

Retrieves references corresponding to the saved search string(s) on up
to three platforms (e.g. [Web of
Science](https://clarivate.com/academia-government/scientific-and-academic-research/research-discovery-and-referencing/web-of-science/),
[Scopus](https://www.elsevier.com/en-au/products/scopus) and
[PubMed](https://pubmed.ncbi.nlm.nih.gov/)).

## Usage

``` r
manual_fetch(
  WOS = TRUE,
  SCP = TRUE,
  PMD = TRUE,
  dedup = FALSE,
  open_file = FALSE,
  dry_run = FALSE
)
```

## Arguments

- WOS:

  Runs the search on Web of Science (TRUE or FALSE).

- SCP:

  Runs the search on Scopus (TRUE or FALSE).

- PMD:

  Runs the search on PubMed (TRUE or FALSE).

- dedup:

  Deduplicates the retrieved references (TRUE or FALSE).

- open_file:

  Automatically opens the CSV file after reference retrieval.

- dry_run:

  Simulation run option.

## Value

Create a CSV file with the references metadata, a history file of the
references retrieved and a history file of the deduplication (if the
option is selected).

## Examples

``` r
# This is a "dry run" example.
# No references will actually be scheduled, it only shows how the function should react.
manual_fetch(WOS = TRUE,
             SCP = TRUE,
             PMD = TRUE,
             dedup = TRUE,
             open_file = FALSE,
             dry_run = TRUE
             )
#> [1] 126
#>               Finished batch number 1
#>               Finished batch number 2
#>               [1] "10.1016/j.aaf.2023.11.002 1 / 126"
#>               [1] "10.3390/fishes10090439 2 / 126"
#>               [FOR THE PURPOSE OF THIS EXAMPLE WE ARE NOT SHOWING EACH LINE OF THE CONSOLE]
#>               [THE REMOVED LINES CORRESPOND TO MOST OF THE REFERENCE RETRIEVAL COUNTING]
#>               [1] "NA 125 / 126"
#>               [1] "NA 126 / 126"
#>               [1] 22
#>               Finished batch number 1
#>               File already exists
#>               [1] "10.1007/s12602-023-10207-x 1 / 22"
#>               [1] "10.1016/j.fsi.2025.110189 2 / 22"
#>               [FOR THE PURPOSE OF THIS EXAMPLE WE ARE NOT SHOWING EACH LINE OF THE CONSOLE]
#>               [THE REMOVED LINES CORRESPOND TO MOST OF THE REFERENCE RETRIEVAL COUNTING]
#>               [1] "10.1111/j.1472-765X.2010.02894.x 21 / 22"
#>               [1] "NA 22 / 22"
#>               [1] 106
#>               Finished batch 1 for fish AND "vibrio harveyi" AND diagnostic
#>               File already exists
#>               [1] "10.1016/j.fsi.2025.110503 1 / 106"
#>               [1] "10.1016/j.fsi.2025.110501 2 / 106"
#>               [FOR THE PURPOSE OF THIS EXAMPLE WE ARE NOT SHOWING EACH LINE OF THE CONSOLE]
#>               [THE REMOVED LINES CORRESPOND TO MOST OF THE REFERENCE RETRIEVAL COUNTING]
#>               [1] "NA 105 / 106"
#>               [1] "NA 106 / 106"
#>               Warning: The following columns are missing: pages, number, record_id, isbn
#>               formatting data...
#>               identifying potential duplicates...
#>               identified duplicates!
#>               flagging potential pairs for manual dedup...
#>               Joining with `by = join_by(duplicate_id.x, duplicate_id.y)`
#>               254 citations loaded...
#>               14 duplicate citations removed...
#>               240 unique citations remaining!
#>               Deduplication script has been executed, concatenated deduplicated references had been exported.
#>               Warning message:
#>               In add_missing_cols(raw_citations) :
#>                Search contains missing values for the record_id column.
#>                A record_id will be created using row numbers

```
