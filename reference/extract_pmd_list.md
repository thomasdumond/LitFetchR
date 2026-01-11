# Extracts the metadata from the new references found on PubMed based on the search string(s) saved in "search_list.txt".

Extracts the metadata from the new references found on PubMed based on
the search string(s) saved in "search_list.txt".

## Usage

``` r
extract_pmd_list(search_list_path)
```

## Arguments

- search_list_path:

  Path to "search_list.txt".

## Value

A data.frame with one row per retrieved PubMed record and columns:

- author:

  Character. Publication authors.

- year:

  Character. Publication year.

- title:

  Character. Publication title.

- journal:

  Character. Publication journal name.

- volume:

  Character. Publication journal volume.

- issue:

  Character. Publication journal issue.

- abstract:

  Character. Publication abstract.

- doi:

  Character. Publication Digital Object Identifier (DOI).

- source:

  Character. Data source.

- platform_id:

  Character. Publication unique identifier in data source.

If `search_list_path` does not exist, returns `NULL`.
