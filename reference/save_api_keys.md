# Saves Web of Science and/or Scopus API keys in .Renviron.

You can set wos_api_key, scp_api_key, or both at the same time. Remember
to restart the R session after saving your API keys.

## Usage

``` r
save_api_keys(wos_api_key = NULL, scp_api_key = NULL, dry_run = FALSE)
```

## Arguments

- wos_api_key:

  The API key value for Web of Science (use quotation marks).

- scp_api_key:

  The API key value for Scopus (use quotation marks).

- dry_run:

  Simulation run option.

## Value

Logical. TRUE if at least one value was written, FALSE if left
unchanged.

## Examples

``` r
save_api_keys(wos_api_key = "abcd01234",
               scp_api_key = "efgh5678",
               dry_run = TRUE
               )
#> This is the message from the dry run showing what you should be seeing when the function will be used:
#>             Saved key(s) wos_api_key, scp_api_key to -path-to-your-renvironment/.Renviron.
#>             Restart R for the new environment variable(s) to be available.
```
