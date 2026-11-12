## R CMD check results

0 errors | 0 warnings | up to 2 notes (environment-dependent)

Checked locally on Windows 11 with R 4.3.1 (`devtools::check()`), on
win-builder R-devel (2026-11-11 r90876) and R-release (4.6.2): 0 errors |
0 warnings | 1 NOTE (the one below) on both.
The package was also installed from source and run on macOS (Apple silicon,
R 4.6.2), including scheduling through cron.

* checking package dependencies ... NOTE
  Suggests or Enhances not in mainstream repositories: ASySD
  (On some platforms also reported as: 'ASySD', 'cronR' could not be checked.)

  'ASySD' is only available from GitHub (<https://github.com/camaradesuk/ASySD>).
  It is used solely by the optional dedup_refs() function and its use is guarded
  with requireNamespace(), so the package installs, loads, and passes its checks
  without it. 'cronR' is a Suggested package used only for task scheduling on
  macOS/Linux; it is likewise guarded behind a runtime availability check.

* checking for future file timestamps ... NOTE
  unable to verify current time

  This note relates to the check environment's inability to reach the time
  server and is not related to the package. (Not seen on win-builder.)

## Update

This is an update from the current CRAN version (1.0.0) to 1.1.0.

This release improves the reliability and the traceability of the retrieval
and of the scheduled retrieval:

* Every retrieval now appends a row per saved search and platform to a run log
  (`run_log.csv`, written in the user's project directory).
* Each platform is queried independently, and the history of retrieved
  identifiers is updated only after the output file is written, so a failure on
  one platform no longer affects the others or the history.
* `auto_LitFetchR_setup()` now checks the result of the operating system
  scheduler and reads the task back before reporting success. It calls the
  Windows `schtasks` command directly, so `taskscheduleR` is removed from
  Suggests. On macOS and Linux the task is created with the optional `cronR`
  package, which was tested on macOS (Apple silicon, R 4.6.2): installation
  from source, retrieval, and scheduled runs through cron.
* The query sent to each platform is built in one place, so that the counts
  shown by `create_save_search()` and the retrieval use the same query. The
  exclusion operator is written the way each platform requires.
* The saved search file is validated before retrieval, and
  `create_save_search()` offers to save a search when the user exits.
* Search strings containing `&`, `#` or `+` are now encoded correctly in the
  requests to the three platforms (they were previously cut or rejected).
* A history file that is open in another program or damaged gives a readable
  message, and a failed history update is reported as a problem.
* New optional argument `output_format` ("csv" by default, or "ris") in
  `manual_fetch()`, `auto_LitFetchR_setup()` and `dedup_refs()`, so that the
  references can be imported into tools that do not accept CSV files.
  Scripts created by earlier versions keep working unchanged.
* Inline formatting tags (such as `<i>`) are removed from titles and abstracts
  of Web of Science and Scopus records.
* Documentation (README, vignettes) updated, including installation from CRAN
  and the optional packages.

The exported function interface is unchanged apart from two new optional
arguments (`day` in `auto_LitFetchR_setup()` and `output_format`). Two behaviours differ from 1.0.0
and are described in NEWS.md: a `search_list.txt` with duplicated names now
stops the run, and searches containing exclusions (`NOT` / `AND NOT`) return
different, correct, results on some platforms. There are no reverse
dependencies on CRAN.

The package writes files only in the directory chosen by the user. It creates
a scheduled task only when `auto_LitFetchR_setup()` is called explicitly. All
examples use `dry_run = TRUE`, the vignette code is not evaluated, and the
tests do not call the scheduler or any API, so no task, file or API request is
created during the checks.
