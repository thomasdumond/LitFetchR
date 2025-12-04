# Remove or change a scheduled task

If you are here, this is because you want to remove or modify a
reference retrieval task that you set up earlier. Otherwise, check the
article [LitFetchR
(Tutorial)](https://thomasdumond.github.io/LitFetchR/articles/LitFetchr.html)
to learn how to set up a task.

## Setup

From your individual review R directory, load `LitFetchR`:

``` r
library(LitFetchR)
```

## Remove a scheduled task

Just use the following function with the name of the scheduled task:

``` r
remove_scheduled_task("taskID")
```

## Modify a scheduled task

There is no function to directly modify a scheduled task at the moment.
You can simply:

1.  Remove the scheduled task (see above)
2.  Setup a new task (see [LitFetchR
    (Tutorial)](https://thomasdumond.github.io/LitFetchR/articles/LitFetchr.html))
