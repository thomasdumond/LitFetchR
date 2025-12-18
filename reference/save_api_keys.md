# Saves Web of Science and/or Scopus API keys in .Renviron.

You can set WOS_API_KEY, SCP_API_KEY, or both at the same time. Remember
to restart the R session after saving your API keys.

## Usage

``` r
save_api_keys(WOS_API_KEY = NULL, SCP_API_KEY = NULL, dry_run = FALSE)
```

## Arguments

- WOS_API_KEY:

  The API key value for Web of Science (use quotation marks).

- SCP_API_KEY:

  The API key value for Scopus (use quotation marks).

- dry_run:

  Simulation run option.

## Value

TRUE if at least one value was written, FALSE if left unchanged.

## Examples

``` r
save_api_keys(WOS_API_KEY = "abcd01234",
               SCP_API_KEY = "efgh5678",
               dry_run = TRUE
               )
#> Saved key(s) WOS_API_KEY, SCP_API_KEY to -path-to-your-renvironment/.Renviron.
#>             Restart R for the new environment variable(s) to be available.
```
