# Changelog

## LitFetchR 1.0.0

Major reworking of the retrieval pipeline, consolidating a series of
performance, reliability, and data-quality improvements. The exported
function interface is unchanged apart from two new optional arguments to
[`save_api_keys()`](https://thomasdumond.github.io/LitFetchR/reference/save_api_keys.md)
(`scp_insttoken` and `ncbi_api_key`), but the returned data frame has
changed (see “Output columns” below).

###### Output columns

- The `issue` column has been renamed to `number`. Code that referred to
  the output column by the name `issue` must be updated.
- Added `pages` and `isbn` columns to the output of all three
  extractors.
- The returned data frame is now a consistent 12 columns across Web of
  Science, Scopus, and PubMed:
  `author, year, title, journal, volume, number, abstract, doi, pages, isbn, source, platform_id`.

###### Performance

- Web of Science and Scopus extractors no longer make individual
  per-record API calls. All metadata is now extracted directly from the
  batch search responses, greatly reducing the number of API requests
  and the time per run.
- PubMed record details are now fetched in batches of 200 PMIDs per call
  instead of one call per record.
- Replaced row-by-row `rbind` accumulation with list collection and a
  single combine in all three extractors.
- Removed a redundant save-and-reread of `history_id.xlsx` during
  extraction.

###### API keys and authentication

- The Scopus API key is now sent as the `X-ELS-APIKey` request header
  instead of a URL query parameter, matching Elsevier’s current
  requirements.
- Added optional support for a Scopus institutional token. Set it with
  `save_api_keys(scp_insttoken = "...")`; when present it is sent as the
  `X-ELS-Insttoken` header on all Scopus requests. Some institutional
  subscriptions require it.
- [`save_api_keys()`](https://thomasdumond.github.io/LitFetchR/reference/save_api_keys.md)
  can now save an optional NCBI/PubMed API key with
  `save_api_keys(ncbi_api_key = "...")`, which enables a higher PubMed
  request rate. Previously this key had to be added to `.Renviron` by
  hand.

###### Reliability and error handling

- Rate-limit responses (HTTP 429) are now retried in a dedicated loop
  that respects the `Retry-After` header.
- Authentication failures (HTTP 401/403) now fail immediately with a
  clear diagnostic message instead of retrying.
- Every request now has a 60-second timeout.
- PubMed requests now respect NCBI rate limits, using a faster rate when
  the optional `ncbi_api_key` environment variable is set.
- [`create_save_search()`](https://thomasdumond.github.io/LitFetchR/reference/create_save_search.md)
  now uses the same retrying HTTP helper as the extractors rather than
  bare requests.
- The list of already-seen record IDs is now saved only after extraction
  succeeds, so an interrupted run no longer marks records as fetched.
- Search strings containing `=` are now parsed correctly from
  `search_list.txt`.
- Fixed handling of searches that return zero results in all three
  extractors.

###### Data quality

- Superscript/subscript markup (`<sup>`/`<inf>` tags) is now stripped
  from Scopus and Web of Science titles and abstracts while preserving
  their content (e.g. `10<sup>2</sup>` becomes `102`), matching Scopus’
  own export format. Bare `<` and `>` used as mathematical operators
  (e.g. `p < 0.05`) are preserved.
- Web of Science extraction now handles more record types correctly:
  DIIDW patents (inventors, year, and Derwent abstract), multi-language
  abstracts from CSCD/KJD/SCIELO (preferring the English variant), and
  BCI/ZOOREC book-chapter journal titles. Italic terms in CABI titles
  and abstracts are reconstructed in place, and the article URL is used
  as a fallback when no DOI is available.

###### History files

- Per-run timestamped sheets in the history files have been replaced
  with append-only log sheets, keeping a single growing record instead
  of one sheet per run.

## LitFetchR 0.2.2

CRAN release: 2026-04-14

- Fixes bug in
  [`manual_fetch()`](https://thomasdumond.github.io/LitFetchR/reference/manual_fetch.md):
  removed a chunk of code that was calling ‘scp_api_key’ before it was
  created in the internal function
  [`extract_scp_list()`](https://thomasdumond.github.io/LitFetchR/reference/extract_scp_list.md).
  (see github issue
  <https://github.com/thomasdumond/LitFetchR/issues/2#issue-4259003613>)

## LitFetchR 0.2.1

CRAN release: 2026-02-10

*CRAN release: 2026-02-10* \* Added ‘directory’ arguments to functions
creating files so users can choose in which directory they are created.

- Added option to choose which literature platform to access when
  creating the search string using
  [`create_save_search()`](https://thomasdumond.github.io/LitFetchR/reference/create_save_search.md)
  function.

- Information messages can now be suppressed, if needed, using the
  function [`suppressMessages()`](https://rdrr.io/r/base/message.html).

- This was the first version

## LitFetchR 0.2.0

- New data extraction strategy improving quality and consistency.

## LitFetchR 0.1.1

- Deduplication of references using ASySD made optional.

- Added an option to choose or not the automatic opening of the
  references after each retrieval.

## LitFetchR 0.1.0

- Initial GitHub upload.
