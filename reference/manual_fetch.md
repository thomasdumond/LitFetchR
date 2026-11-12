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
  dry_run = FALSE,
  output_format = c("csv", "ris")
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

  Automatically opens the output file after reference retrieval.

- dry_run:

  Simulation run option.

- output_format:

  Format of the file of references: `"csv"` (default) or `"ris"`. RIS is
  read by most reference managers and screening tools, for example
  Covidence, which does not accept CSV files. A RIS file has one `AU`
  line per author and always describes journal articles. With
  `dedup = TRUE` the deduplicated references are written in the chosen
  format.

## Value

`NULL` (invisibly). Called for its side effects: Create a CSV or RIS
file with the references metadata, a history file of the references
retrieved, a history file of the deduplication (if the option is
selected) and a run log (`run_log.csv`).

## Details

Every retrieval is recorded in the file `run_log.csv` of `directory`
(one row per saved search and platform, or one row with platform "all"
if the run could not start, for example because `search_list.txt` is
invalid). It records whether the run was started by hand or by a
scheduled task (`run_mode`). It stores the exact query submitted to each
platform, the date, the package and R versions, the number of results
and new records, the status of the request and any error message. For
PubMed, it also stores the query translation reported by PubMed. For Web
of Science, it also stores the number of records retrieved from each
source database.

The history of the references already retrieved (`history_id.xlsx`) is
only updated once the output file has been written. If a platform fails
(for example an expired API key), the other platforms are still saved, a
warning names the failed platform(s), and the failed platform is not
added to the history, so its references are retrieved again by the next
run. If all the selected platforms fail, an error is raised. When there
is no new record, no output file is written.

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
#>               Updating existing history_id.xlsx.
#>               149 new records found among 154 total results.
#>               24 total results found on Scopus for: fish AND "vibrio harveyi" AND diagnostic
#>               Finished batch 1 of 1
#>               Updating existing history_id.xlsx.
#>               22 new records found among 24 total results.
#>               110 total results found on PubMed for: fish AND "vibrio harveyi" AND diagnostic
#>               Finished batch 1 of 1
#>               Updating existing history_id.xlsx.
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
#> 
#>               Run log saved in <directory>/run_log.csv

```
