## R CMD check results

0 errors | 0 warnings | 2 notes

* checking package dependencies ... NOTE
  Package suggested but not available for checking: 'cronR'

  'cronR' is a suggested package used only for task scheduling on macOS/Linux.
  It is not available on the check platform, and its use is guarded behind a
  runtime availability check, so it is not required for the package to work.

* checking for future file timestamps ... NOTE
  unable to verify current time

  This note relates to the check environment's inability to reach the time
  server and is not related to the package.

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
