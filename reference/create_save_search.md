# Creates and saves search string(s).

An interactive function that ask the user to enter a search string and
provide the number of results from 3 platforms: Web of Science, Scopus
and PubMed. You can then save one or more search strings to retrieve the
references later.

## Usage

``` r
create_save_search(
  wos = FALSE,
  scp = FALSE,
  pmd = FALSE,
  directory,
  dry_run = FALSE
)
```

## Arguments

- wos:

  Runs the search on Web of Science (TRUE or FALSE).

- scp:

  Runs the search on Scopus (TRUE or FALSE).

- pmd:

  Runs the search on PubMed (TRUE or FALSE).

- directory:

  Choose the directory in which the search string and the search history
  will be saved.

- dry_run:

  Simulation run option.

## Value

`NULL` (invisibly). Called for its side effects: interactive querying
and writing search history files.

## Examples

``` r
# This is a "dry run" example.
# No search will be created and no database will be accessed.
# It only shows how the function should react.
create_save_search(wos = TRUE,
                   scp = TRUE,
                   pmd = TRUE,
                   directory,
                   dry_run = TRUE)
#> This is the message from the dry run showing
#>   what you should be seeing when the function will be used:
#>   History had been created.
#>   Enter your search string (or "summary" or "exit"): fish
#>   [1] "fish"
#>   [1] "Web of Science: 1793296 results"
#>   [1] "Scopus: 718644 results"
#>   [1] "PubMed: 384742 results"
#>   Enter your search string (or "summary" or "exit"):
#>   fish AND "vibrio harveyi"
#>   [1] "fish AND "vibrio harveyi""
#>   [1] "Web of Science: 2084 results"
#>   [1] "Scopus: 1080 results"
#>   [1] "PubMed: 727 results"
#>   Enter your search string (or "summary" or "exit"):
#>   fish AND "vibrio harveyi" AND diagnostic
#>   [1] "fish AND "vibrio harveyi" AND diagnostic"
#>   [1] "Web of Science: 126 results"
#>   [1] "Scopus: 22 results"
#>   [1] "PubMed: 106 results"
#>   Enter your search string (or "summary" or "exit"): summary
#>                                  Search_Term Results_WOS Results_SCP Results_PMD
#>   1                                     fish     1793296      718644      384742
#>   2                fish AND "vibrio harveyi"        2084        1080         727
#>   3 fish AND "vibrio harveyi" AND diagnostic         126          22         106
#>   Select the index number for the search string to use in automated retrieval: 3
#> 
#>   Selected search string: fish AND "vibrio harveyi" AND diagnostic
#>   Do you want to save the search string for future use? (yes/no): yes
#>   Enter a name for the search identification: fish_vibrio
#>   Search string saved successfully.
#>   Enter your search string (or "summary" or "exit"): exit
#>   Exiting search tool.
```
