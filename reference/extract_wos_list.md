# extract the metadata from the new references from Web of Science based on the search strings found in search_list.txt

extract the metadata from the new references from Web of Science based
on the search strings found in search_list.txt

## Usage

``` r
extract_wos_list(search_list_path, directory)
```

## Arguments

- search_list_path:

  path to search_list

- directory:

  Choose the directory in which the references identification history
  will be saved.

## Value

A data.frame with one row per retrieved Web of Science record and
columns:

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
