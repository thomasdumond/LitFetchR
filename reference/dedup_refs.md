# deduplicate the references from three dataframes

deduplicate the references from three dataframes

## Usage

``` r
dedup_refs(df1 = NULL, df2 = NULL, df3 = NULL, open_file = FALSE)
```

## Arguments

- df1:

  dataframe 1, can be null

- df2:

  dataframe 2, can be null

- df3:

  dataframe 3, can be null

- open_file:

  choose to automatically open the CSV file after reference
  deduplication

## Value

A CSV file containing all the new references deduplicated and the
history of the deduplication.

## Examples
