# Reads and validates the saved search strings.

Reads "search_list.txt" and returns the saved searches as a named
character vector. The file is parsed in the same way as in the
extraction functions: blank lines are dropped, the name is the text
before the first `=` and the query is everything after the first `=` (so
a query may itself contain `=`). The function stops with an informative
message when the file is missing, has no search, or is malformed (a line
without `=`, an empty name or query, or duplicated names).

## Usage

``` r
read_search_list(path, allow_empty = FALSE)
```

## Arguments

- path:

  Path to "search_list.txt".

- allow_empty:

  If `TRUE`, a file without any search returns an empty named character
  vector instead of an error. Defaults to `FALSE`.

## Value

A named character vector. The names are the search names and the values
are the query strings.
