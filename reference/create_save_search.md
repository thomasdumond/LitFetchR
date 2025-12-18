# Create and save search string(s).

An interactive function that ask the user to enter a search string and
provide the number of results from 3 platforms: Web of Science, Scopus
and PubMed. You can then save one or more search strings to retrieve the
references later.

## Usage

``` r
create_save_search(dry_run = FALSE)
```

## Value

One or more search string(s) in the file "search_list.txt" and the
history of all searches in "history_search.xlsx".

## Examples

``` r
# This is a "dry run" example.
# No search will be created and no database will be accessed.
# It only shows how the function should react.
create_save_search(dry_run = TRUE)
#> History had been created.
#>             Enter your search string (or "summary" or "exit"): fish
#>             [1] "fish"
#>             [1] "Web of Science: 1793296 results"
#>             [1] "Scopus: 718644 results"
#>             [1] "PubMed: 384742 results"
#>             Enter your search string (or "summary" or "exit"): fish AND "vibrio harveyi"
#>             [1] "fish AND "vibrio harveyi""
#>             [1] "Web of Science: 2084 results"
#>             [1] "Scopus: 1080 results"
#>             [1] "PubMed: 727 results"
#>             Enter your search string (or "summary" or "exit"): fish AND "vibrio harveyi" AND diagnostic
#>             [1] "fish AND "vibrio harveyi" AND diagnostic"
#>             [1] "Web of Science: 126 results"
#>             [1] "Scopus: 22 results"
#>             [1] "PubMed: 106 results"
#>             Enter your search string (or "summary" or "exit"): summary
#>                                            Search_Term Results_WOS Results_SCP Results_PMD
#>             1                                     fish     1793296      718644      384742
#>             2                fish AND "vibrio harveyi"        2084        1080         727
#>             #3 fish AND "vibrio harveyi" AND diagnostic         126          22         106
#>             Select the index number for the search string to use in automated retrieval: 3
#> 
#>             Selected search string: fish AND "vibrio harveyi" AND diagnostic
#>             Do you want to save the search string for future use? (yes/no): yes
#>             Enter a name for the search identification: fish_vibrio
#>             Search string saved successfully.
#>             Enter your search string (or "summary" or "exit"): exit
#>             Exiting search tool.
#>             
```
