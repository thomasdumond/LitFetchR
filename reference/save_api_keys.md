# Save Web of Science and/or Scopus API keys in .Renviron

You can set WOS_API_KEY, SCP_API_KEY, or both in a single call.

## Usage

``` r
save_api_keys(WOS_API_KEY = NULL, SCP_API_KEY = NULL)
```

## Arguments

- WOS_API_KEY:

  The API key value for Web of Science (optional).

- SCP_API_KEY:

  The API key value for Scopus (optional).

## Value

TRUE if at least one value was written, FALSE if left unchanged.

## Examples
