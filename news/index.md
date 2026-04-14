# Changelog

## LitFetchR 0.2.2

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
