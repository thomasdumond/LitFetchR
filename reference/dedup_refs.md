# Deduplicates the references from up to three dataframes.

Deduplicates the references from up to three dataframes.

## Usage

``` r
dedup_refs(
  df1 = NULL,
  df2 = NULL,
  df3 = NULL,
  open_file = FALSE,
  dry_run = FALSE
)
```

## Arguments

- df1:

  Dataframe 1 (can be NULL)

- df2:

  Dataframe 2 (can be NULL)

- df3:

  Dataframe 3 (can be NULL)

- open_file:

  Automatically opens the CSV file after reference retrieval.

- dry_run:

  Simulation run option.

## Value

A CSV file containing all the new references deduplicated and the
history of the deduplication in an excel file.

## Examples

``` r
# This is a "dry run" example.
# No deduplication will happen.
# It only shows how the function should react.
dedup_refs(df_vibrio_wos,
           df_vibrio_scp,
           df_vibrio_pmd,
           dry_run = TRUE
           )
#> Warning: The following columns are missing: pages, number, record_id, isbn
#>     formatting data...
#>     identifying potential duplicates...
#>     identified duplicates!
#>     flagging potential pairs for manual dedup...
#>     Joining with `by = join_by(duplicate_id.x, duplicate_id.y)`
#>     254 citations loaded...
#>     14 duplicate citations removed...
#>     240 unique citations remaining!
#>     Deduplication script has been executed, concatenated deduplicated references had been exported.
#>     Warning message:
#>     In add_missing_cols(raw_citations) :
#>     Search contains missing values for the record_id column.
#>     A record_id will be created using row numbers
```
