# Automating the retrieval of references based on a saved search string(s).

Creates a read-only Rscript and a task to run the code automatically at
a specified frequency and time, to retrieve references corresponding to
the saved search string(s) on up to three platforms (e.g. [Web of
Science](https://clarivate.com/academia-government/scientific-and-academic-research/research-discovery-and-referencing/web-of-science/),
[Scopus](https://www.elsevier.com/en-au/products/scopus) and
[PubMed](https://pubmed.ncbi.nlm.nih.gov/)).

## Usage

``` r
auto_LitFetchR_setup(
  task_id = "task_id",
  when = "DAILY",
  time = "08:00",
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

- task_id:

  Name of the automated reference retrieval task (e.g. one keyword
  describing your review).

- when:

  Frequency of the automated reference retrieval task (DAILY, WEEKLY or
  MONTHLY).

- time:

  Time of the automated reference retrieval task (must be HH:MM 24-hour
  clock format).

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

`NULL` (invisibly). Called for its side effects: writes an R script and
schedules a task (Windows Task Scheduler or cron) to run the script
automatically.

## Examples

``` r
# This is a "dry run" example.
# No task will actually be scheduled,
# it only shows how the function should react.
auto_LitFetchR_setup(task_id = "fish_vibrio",
                       when = "WEEKLY",
                       time = "14:00",
                       wos = TRUE,
                       scp = TRUE,
                       pmd = TRUE,
                       directory,
                       dedup = FALSE,
                       open_file = FALSE,
                       dry_run = TRUE
                       )
#> Dry run: no task scheduled,
#>             the message "Task scheduled!" will appear when the function will
#>             run successfully.
```
