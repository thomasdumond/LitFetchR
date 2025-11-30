# LitFetchR

The goal of LitFetchR is to automatically retrieve and deduplicate
reference metadata based on saved search strings. Access to Web of
Science and Scopus requires personal API keys, while PubMed can be
queried without one.

## Installation

You can install the development version of LitFetchR from
[GitHub](https://github.com/) with:

``` R
install.packages("devtools")
devtools::install_github("thomasdumond/LitFetchR")
```

## Tutorial

Please see the [written
tutorial](https://thomasdumond.github.io/LitFetchR/) as well as the
[youtube tutorial](https://thomasdumond.github.io/LitFetchR/).

## Requirements

This package uses API to access Web of Science and Scopus platforms. To
enjoy the full performance of `LitFetchR`, request personal API keys to
the platforms. For more information on API keys, you can see our [API
tutorial](https://thomasdumond.github.io/LitFetchR/). If you are
affiliated to a University we suggest contacting your local librarian to
discuss the use of API keys (i.e. quota access, detail of databases
accessed through Web of Science).

PubMed does not require API key access, so the package can still be used
to access PubMed only.

More databases will be considered for inclusion in future versions of
the package.
