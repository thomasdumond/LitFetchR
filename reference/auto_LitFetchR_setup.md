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
  PMD = TRUE
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

## Value

Create a Rscript file (READ ONLY) and a task in Task Scheduler
(Windows), or in Cron (Mac/Linux)

## Examples
