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
  open_file = FALSE
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

## Value

Create a Rscript file (READ ONLY) and a task in Task Scheduler
(Windows), or in Cron (Mac/Linux)

## Examples

``` r
# \donttest{

# Example of what you should see:

auto_LitFetchR_setup(task_ID = "fish_vibrio",
                       when = "WEEKLY",
                       time = "14:00",
                       WOS = TRUE,
                       SCP = TRUE,
                       PMD = TRUE)
#> At your own risk: will set the cron schedule as is: 'weekly'
#> Are you sure you want to add the specified cron job: '/opt/R/4.5.2/lib/R/bin/Rscript '/home/runner/work/LitFetchR/LitFetchR/docs/reference/auto_LitFetchR_code(READ_ONLY).R'  >> '/home/runner/work/LitFetchR/LitFetchR/docs/reference/auto_LitFetchR_code(READ_ONLY).log' 2>&1'? [y/n]: 
#> Error in if (!input %in% "y") {    message("No action taken.")    return(invisible())}: argument is of length zero
#> Task scheduled!
# }
```
