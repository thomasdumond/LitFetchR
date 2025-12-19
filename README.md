
# LitFetchR <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
<!-- badges: end -->

The purpose of `LitFetchR` is to automatically retrieve and deduplicate
references based on saved search string(s). Access to Web of Science and
Scopus requires personal API keys, while PubMed can be queried without
one.

## Installation

You can install the development version of `LitFetchR` from
[GitHub](https://github.com/) using `devtools` or `remotes`:

``` r
#install via devtools
install.packages("devtools")
devtools::install_github("thomasdumond/LitFetchR")

#install via remotes
install.packages("remotes")
remotes::install_github("thomasdumond/LitFetchR")
```

## Tutorial

You can find all the tutorials in the
[Articles](https://thomasdumond.github.io/LitFetchR/articles/) tab.

## Requirements

This package uses APIs to access Web of Science and Scopus platforms. To
enjoy the full performance of `LitFetchR`, request personal API keys
from the platforms. For more information on API keys, you can see our
tutorial [Get API
keys](https://thomasdumond.github.io/LitFetchR/articles/Get_API_keys.html).
If you are affiliated with a university we suggest contacting your local
librarian to discuss the use of API keys (i.e. quota access, details of
databases accessed through Web of Science).

PubMed does not require API key access, so the package can still be used
to access PubMed only.

*More scientific platforms and databases will be considered for
inclusion in future versions of the package.*
