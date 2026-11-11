# LitFetchR 1.1.0

Reliability and transparency release. Each retrieval is now recorded in a run log, scheduling is verified instead of assumed, and a failure on one platform no longer affects the others or the history of seen records. The exported function interface is unchanged apart from one new optional argument, `day`, in `auto_LitFetchR_setup()`.

##### Run log

* New file `run_log.csv` in the project directory. Every run, manual or scheduled, adds one row per saved search and platform with the date and time, the versions of LitFetchR and R, the run mode (`manual` or `scheduled:<task name>`), the search string as read from `search_list.txt`, the exact query submitted to the platform, the number of results and of new records, the status (`success`, `no_results`, `partial` or `error`) and any error message.
* For PubMed the query translation returned by the platform is stored. For Web of Science the number of retrieved records per source database is stored, derived from the prefix of the record identifiers.
* A problem found before any platform is queried (missing or invalid `search_list.txt`, no platform selected) is logged as a row with the platform `all` and the status `error`.
* The `id_log` sheet of `history_id.xlsx` has a new `search_name` column. Existing files are upgraded in place. A `run_log.csv` made by an earlier development version is upgraded in place as well.

##### Search strings

* One internal helper now builds the exact query sent to each platform, so that the counts shown by `create_save_search()` and the records retrieved by `manual_fetch()` always use the same query.
* The exclusion operator is written the way each platform needs it, outside quoted phrases: `AND NOT` for Scopus and `NOT` for PubMed and Web of Science. A typed `AND NOT` is no longer duplicated for Scopus. Previously retrieval sent the raw string to Scopus, and PubMed ignored the `NOT` of an `AND NOT` and returned the records to be excluded. Searches containing exclusions can therefore return different, correct, results than in 1.0.0.
* `create_save_search()` no longer ends the session when a platform rejects a search. The count for that platform is shown as unavailable, together with the platform's message and the query sent.

##### create_save_search() and search_list.txt

* Selecting `exit` or `summary` offers to save a search of the session when none has been saved.
* All questions of the saving flow behave the same way: an invalid answer is asked again, an empty answer cancels and returns to the search prompt, and the valid range is shown for the index question. Names are checked before saving (empty names, `=` and regular expression characters).
* `search_list.txt` is validated before any platform is queried: a missing or empty file, a line without `=`, an empty name or query and duplicate names stop the run with a clear message. Files with duplicate names were accepted by 1.0.0 and now stop the run.

##### Scheduling

* `auto_LitFetchR_setup()` reports "Task scheduled!" only after the scheduler accepted the task and the task was read back. A short summary (recurrence, next run time, status) is printed. If the scheduler returns an error, the function stops and shows the scheduler's message. If the task cannot be confirmed, a warning is given.
* New argument `day`: the weekday of a weekly task (MON to SUN) or the day of the month of a monthly task (1 to 28). The default is the weekday or day of the month of today. Weekly tasks previously ran every day, and no start date is passed any more, so regional date settings no longer matter.
* A task with the same name is never overwritten. Use `remove_scheduled_task()` first.
* The generated script wrote Windows paths unescaped and could not be parsed. This is fixed, and the script is checked before the task is created.
* The generated script records the name of the task in the run log and ends with a non-zero exit status when the run failed or finished with a problem.
* `remove_scheduled_task()` reports what actually happened.
* The package now calls the Windows `schtasks` command directly and no longer uses taskscheduleR, which is removed from the suggested packages. On macOS and Linux the task is still created with `cronR`.

##### Failure handling

* Each platform is queried independently. A failure on one platform is logged and warned about, and the other platforms continue. The run stops only when all selected platforms fail.
* The output file is written first and `history_id.xlsx` is updated afterwards, only with the identifiers of records actually retrieved. A failed platform or a failed output therefore never marks records as seen, and the records are retrieved at the next run.
* When no record is new, no CSV or deduplication file is written. With `dedup = TRUE` and no new record the deduplication step is skipped instead of failing.
* Messages about existing history files were reworded ("Updating existing history_id.xlsx.").

# LitFetchR 1.0.0

Major reworking of the retrieval pipeline, consolidating a series of performance, reliability, and data-quality improvements. The exported function interface is unchanged apart from two new optional arguments to `save_api_keys()` (`scp_insttoken` and `ncbi_api_key`), but the returned data frame has changed (see "Output columns" below).

##### Output columns

* The `issue` column has been renamed to `number`. Code that referred to the output column by the name `issue` must be updated.
* Added `pages` and `isbn` columns to the output of all three extractors.
* The returned data frame is now a consistent 12 columns across Web of Science, Scopus, and PubMed: `author, year, title, journal, volume, number, abstract, doi, pages, isbn, source, platform_id`.

##### Performance

* Web of Science and Scopus extractors no longer make individual per-record API calls. All metadata is now extracted directly from the batch search responses, greatly reducing the number of API requests and the time per run.
* PubMed record details are now fetched in batches of 200 PMIDs per call instead of one call per record.
* Replaced row-by-row `rbind` accumulation with list collection and a single combine in all three extractors.
* Removed a redundant save-and-reread of `history_id.xlsx` during extraction.

##### API keys and authentication

* The Scopus API key is now sent as the `X-ELS-APIKey` request header instead of a URL query parameter, matching Elsevier's current requirements.
* Added optional support for a Scopus institutional token. Set it with `save_api_keys(scp_insttoken = "...")`; when present it is sent as the `X-ELS-Insttoken` header on all Scopus requests. Some institutional subscriptions require it.
* `save_api_keys()` can now save an optional NCBI/PubMed API key with `save_api_keys(ncbi_api_key = "...")`, which enables a higher PubMed request rate. Previously this key had to be added to `.Renviron` by hand.

##### Reliability and error handling

* Rate-limit responses (HTTP 429) are now retried in a dedicated loop that respects the `Retry-After` header.
* Authentication failures (HTTP 401/403) now fail immediately with a clear diagnostic message instead of retrying.
* Every request now has a 60-second timeout.
* PubMed requests now respect NCBI rate limits, using a faster rate when the optional `ncbi_api_key` environment variable is set.
* `create_save_search()` now uses the same retrying HTTP helper as the extractors rather than bare requests.
* The list of already-seen record IDs is now saved only after extraction succeeds, so an interrupted run no longer marks records as fetched.
* Search strings containing `=` are now parsed correctly from `search_list.txt`.
* Fixed handling of searches that return zero results in all three extractors.

##### Data quality

* Superscript/subscript markup (`<sup>`/`<inf>` tags) is now stripped from Scopus and Web of Science titles and abstracts while preserving their content (e.g. `10<sup>2</sup>` becomes `102`), matching Scopus' own export format. Bare `<` and `>` used as mathematical operators (e.g. `p < 0.05`) are preserved.
* Web of Science extraction now handles more record types correctly: DIIDW patents (inventors, year, and Derwent abstract), multi-language abstracts from CSCD/KJD/SCIELO (preferring the English variant), and BCI/ZOOREC book-chapter journal titles. Italic terms in CABI titles and abstracts are reconstructed in place, and the article URL is used as a fallback when no DOI is available.

##### History files

* Per-run timestamped sheets in the history files have been replaced with append-only log sheets, keeping a single growing record instead of one sheet per run.

# LitFetchR 0.2.2
* Fixes bug in `manual_fetch()`: removed a chunk of code that was calling 'scp_api_key' before it was created in the internal function `extract_scp_list()`. (see github issue https://github.com/thomasdumond/LitFetchR/issues/2#issue-4259003613)

# LitFetchR 0.2.1
*CRAN release: 2026-02-10*
* Added 'directory' arguments to functions creating files so users can choose in which directory they are created.

* Added option to choose which literature platform to access when creating the search string using `create_save_search()` function.

* Information messages can now be suppressed, if needed, using the function `suppressMessages()`.

* This was the first version 

# LitFetchR 0.2.0

* New data extraction strategy improving quality and consistency.

# LitFetchR 0.1.1

* Deduplication of references using ASySD made optional.

* Added an option to choose or not the automatic opening of the references after each retrieval.

# LitFetchR 0.1.0

* Initial GitHub upload.
