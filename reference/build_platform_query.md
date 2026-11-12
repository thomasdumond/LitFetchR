# Builds the query submitted to a literature platform.

Takes the search string entered by the user and returns the exact query
text sent to the API of the selected platform, before URL encoding. The
field tag of the platform is added (PubMed: none, Scopus:
`TITLE-ABS-KEY`, Web of Science: `TS`) and the exclusion operator is
written the way each platform needs it, outside quoted phrases:
`AND NOT` for Scopus and `NOT` for PubMed and Web of Science (PubMed
ignores the `NOT` of an `AND NOT` and returns the records to be
excluded, and Web of Science rejects it). This is the only place where
the search string is modified, so that the counts shown by
[`create_save_search()`](https://thomasdumond.github.io/LitFetchR/reference/create_save_search.md)
and the records retrieved by
[`manual_fetch()`](https://thomasdumond.github.io/LitFetchR/reference/manual_fetch.md)
always use the same query.

## Usage

``` r
build_platform_query(search, platform = c("wos", "scp", "pmd"))
```

## Arguments

- search:

  Search string entered by the user (single character string).

- platform:

  One of `"wos"`, `"scp"` or `"pmd"`.

## Value

A single character string, not URL encoded.
