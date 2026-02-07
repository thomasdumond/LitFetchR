# Creates an excel file to store the deduplication history.

Creates an excel file to store the deduplication history.

## Usage

``` r
create_dedup_history(directory)
```

## Arguments

- directory:

  Choose the directory in which the references deduplication history
  will be saved.

## Value

A list with elements:

- history_dedup:

  A Workbook object (from openxlsx).

- hist_dedup_path:

  Character. Path to the created .xlsx file.
