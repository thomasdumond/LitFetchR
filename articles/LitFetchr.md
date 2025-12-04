# LitFetchR (Tutorial)

Before continuing to the general package tutorial, make sure to have
received your Scopus and Web of Science API keys. If not, the package
will only access PubMed. If you don’t know how to get API keys, have a
look at the article [Get API
keys](https://thomasdumond.github.io/LitFetchR/articles/Get_API_keys.html).

## Setup

We strongly recommend to create a new R directory for each individual
review project. To do so in RStudio:

`File` \> `New Project…` \> `New Directory`

From your new directory, install the package in R from GitHub:

``` r
#This step can be skipped if `LitFetchR` is already installed
devtools::install_github("thomasdumond/LitFetchR")
```

Then load the package:

``` r
library(LitFetchR)
```

## Save API keys

Save your API keys in your personal R environment:

``` r
#This step can be skipped if your API keys have already been saved. Repeat this step if your API keys changed.
save_api_keys(WOS_API_KEY = "your-wos-api-key", SCP_API_KEY = "your-scp-api-key")
```

This will allow `LitFetchR` to locally access your personal API keys
while keeping them confidential if you need to share your code with
collaborators.

## Save the review search string

The interactive
[`create_save_search()`](https://thomasdumond.github.io/LitFetchR/reference/create_save_search.md)
function will guide you through its workflow. Here is an example of what
you should see:

``` r
create_save_search()

# Example
```

After using
[`create_save_search()`](https://thomasdumond.github.io/LitFetchR/reference/create_save_search.md),
two new files will appear in your directory:

- search_list.txt

  *This file contains the name and search string that you saved using
  [`create_save_search()`](https://thomasdumond.github.io/LitFetchR/reference/create_save_search.md).
  It is an essential file to retrieve the reference later. If you modify
  the search string in this file, the modified search string will be
  used for future reference retrieval.*

- history_search.xlsx

  *This file contains all the search that you run when using the
  function
  [`create_save_search()`](https://thomasdumond.github.io/LitFetchR/reference/create_save_search.md).
  Each time you run the function, it creates a new sheet named with the
  current date and saves both the search string sent to the literature
  platforms, and the number of results that they returned.*

If you already validated your review search string, use
[`create_save_search()`](https://thomasdumond.github.io/LitFetchR/reference/create_save_search.md)
to save the search string and setup the file required to continue toward
the automation of reference retrieval.

## Setup the automatic reference retrieval

The automated reference retrieval uses either Windows Task Scheduler or
Cron (Mac/Linux). You do not have to specify it, `LitFetchR` will detect
the system and setup the task for you using the following:

``` r
#We recommend using a single word for the *task_ID* or to use underscores "_" to separate words.
#The retrieval frequency is currently available "DAILY", "WEEKLY" or "MONTHLY".
#You need to use a 24H format for the time of reference retrieval.
#If you do not have an API key for WOS and/or SCP or want to exclude any database,
#change "TRUE" to "FALSE" in front of the corresponding database (e.g.`WOS = FALSE`).

auto_LitFetchR_setup(task_ID = "name_of_your_task", when = "DAILY", time = "14:00", WOS = TRUE, SCP = TRUE, PMD = TRUE)
```

After running
[`auto_LitFetchR_setup()`](https://thomasdumond.github.io/LitFetchR/reference/auto_LitFetchR_setup.md)
for the first time, four new files will be created:

- history_id.xlsx

  *This file contains all the databases unique identifiers (e.g. PMID or
  SCOPUS_ID). These are saved and used at each reference retrieval to
  only extract the references that were not retrieved before.*

- history_dedup_YYYY_MM_HHMMSS.xlsx

  *This file contains the history of each steps of the deduplication,
  using the package ASySD.*

- citationsCSV_YYYY_MM_HHMMSS.CSV

  *This file contains the unique list of references retrieved from the
  search conducted at the date referenced in the name of the file. It is
  a CSV file, ready to be imported in the screening tool of your choice
  or in your reference manager.*

- auto_LitFetchR_code(READ_ONLY).R

  *This file contains the code that is automatically run by the
  scheduled task. It is set as “READ ONLY” to avoid accidental
  modification that would impair the scheduled task action. If you
  deleted this file by accident, use the
  [`auto_LitFetchR_setup()`](https://thomasdumond.github.io/LitFetchR/reference/auto_LitFetchR_setup.md)
  to create a new task and a new code file.*

Each time
[`auto_LitFetchR_setup()`](https://thomasdumond.github.io/LitFetchR/reference/auto_LitFetchR_setup.md)
runs, new unique *history_dedup\_* and *citationCSV\_* files are created
and *history_id* is updated.

**Congratulations ! Your automated reference retrieval is ready to work
!**

See also:

- [Remove or change a scheduled
  task](https://thomasdumond.github.io/LitFetchR/articles/)

- [Manual reference
  retrieval](https://thomasdumond.github.io/LitFetchR/articles/)
