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
  dry_run = FALSE,
  day = NULL,
  output_format = c("csv", "ris")
)
```

## Arguments

- task_id:

  Name of the automated reference retrieval task (e.g. one keyword
  describing your review). Spaces are replaced by "-". A task with the
  same name must not exist already: use
  [`remove_scheduled_task()`](https://thomasdumond.github.io/LitFetchR/reference/remove_scheduled_task.md)
  first to replace it.

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

  Automatically opens the output file after reference retrieval.

- dry_run:

  Simulation run option.

- day:

  Day on which a WEEKLY or MONTHLY task runs. For WEEKLY, one of MON,
  TUE, WED, THU, FRI, SAT or SUN. For MONTHLY, a day of the month
  between 1 and 28. If `NULL` (default), the weekday (WEEKLY) or day of
  the month (MONTHLY) of today's date is used. Ignored for DAILY.

- output_format:

  Format of the file of references written at each run: `"csv"`
  (default) or `"ris"` (see
  [`manual_fetch()`](https://thomasdumond.github.io/LitFetchR/reference/manual_fetch.md)).
  The choice is fixed in the script of the task. To change it, remove
  the task with
  [`remove_scheduled_task()`](https://thomasdumond.github.io/LitFetchR/reference/remove_scheduled_task.md)
  and create it again.

## Value

`NULL` (invisibly). Called for its side effects: writes an R script and
schedules a task (Windows Task Scheduler or cron) to run the script
automatically. The function stops with the scheduler's error message if
the task cannot be created, and gives a warning if the new task cannot
be confirmed.

## Details

The function reports "Task scheduled!" only after checking that the
scheduler accepted the task and that the task exists. On Windows the
registered task is read back from Task Scheduler and compared with the
requested schedule. A short summary (recurrence, next run time and
status) is then printed. If the scheduler returns an error, the function
stops and shows the scheduler's own message. If the task cannot be
confirmed, a warning is given instead of the success message. Check the
summary: it is the only evidence that automatic retrieval will run.

The scheduled task runs only while the computer is on and you are logged
in. Each run is recorded in `run_log.csv` (see
[`manual_fetch()`](https://thomasdumond.github.io/LitFetchR/reference/manual_fetch.md))
with the name of the task in the column `run_mode`. When a run fails, or
finishes with problems on one platform, it ends with a non-zero exit
status, which the Windows Task Scheduler shows as the Last Run Result of
the task.

## Examples

``` r
# This is a "dry run" example.
# No task will actually be scheduled,
# it only shows how the function should react.
auto_LitFetchR_setup(task_id = "fish_vibrio",
                       when = "WEEKLY",
                       time = "14:00",
                       day = "MON",
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
#> The task would run weekly on Monday at 14:00.
```
