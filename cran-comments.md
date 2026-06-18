## R CMD check results

0 errors | 0 warnings | up to 2 notes (environment-dependent)

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

This is an update from the current CRAN version (0.2.2) to 1.0.0.

This release reworks the Web of Science and Scopus retrieval pipeline to
extract all metadata from batch search responses, removing the previous
per-record API calls. It also updates Scopus authentication to use the
`X-ELS-APIKey` header with optional institutional-token support, adds
reliability improvements (retry/timeout handling), and strips
superscript/subscript markup from titles and abstracts.

The exported function interface is unchanged apart from one new optional
argument (`scp_insttoken` in `save_api_keys()`). The returned data frame has
changed: the `issue` column is renamed to `number`, and `pages` and `isbn`
columns are added. There are no reverse dependencies on CRAN.
