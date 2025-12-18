# Automating the retrieval of references based on a saved search string

`auto_LitFetchR_setup` creates a read-only Rscript and a task to run the
code automatically at a specified frequency and time, on up to three
platforms (e.g. [Web of
Science](https://clarivate.com/academia-government/scientific-and-academic-research/research-discovery-and-referencing/web-of-science/),
[Scopus](https://www.elsevier.com/en-au/products/scopus) and
[PubMed](https://pubmed.ncbi.nlm.nih.gov/))

## Usage

``` r
auto_LitFetchR_setup(
  task_ID = "task_ID",
  when = "DAILY",
  time = "08:00",
  WOS = TRUE,
  SCP = TRUE,
  PMD = TRUE,
  dedup = FALSE,
  open_file = FALSE,
  dry_run = FALSE
)
```

## Arguments

- task_ID:

  Name of the automated task (e.g. one keyword describing your review)

- when:

  Frequency of the fetching, i.e. DAILY, WEEKLY, MONTHLY

- time:

  Time of the fetching, i.e. HH:MM 24-hour clock format

- WOS:

  Choose to search on Web of Science (TRUE or FALSE)

- SCP:

  Choose to search on Scopus (TRUE or FALSE)

- PMD:

  Choose to search on PubMed (TRUE or FALSE)

- dedup:

  Choose to deduplicate or not the references (TRUE or FALSE)

- open_file:

  choose to automatically open the CSV file after reference retrieval

- dry_run:

  simulation run option

## Value

Create a Rscript file (READ ONLY) and a task in Task Scheduler
(Windows), or in Cron (Mac/Linux)

## Examples

``` r
auto_LitFetchR_setup(task_ID = "fish_vibrio",
                       when = "WEEKLY",
                       time = "14:00",
                       WOS = TRUE,
                       SCP = TRUE,
                       PMD = TRUE,
                       dedup = FALSE,
                       open_file = FALSE,
                       dry_run = TRUE
                       )
#> Task scheduled!
```
