# Remove or change a scheduled task

If you are here, this is because you want to remove or modify a
reference retrieval task that you set up earlier. Otherwise, check the
article [LitFetchR
(Tutorial)](https://thomasdumond.github.io/LitFetchR/articles/LitFetchr.html)
to learn how to set up a task.

## Setup

From your individual review R directory, load `LitFetchR`:

``` r

#Load LitFetchR
library(LitFetchR)
```

## Remove a scheduled task

Just use the following function with the name of the scheduled task:

``` r

#Load LitFetchR
library(LitFetchR)

#remove a scheduled task using its name/identification

#Example of what you should see:
# > remove_scheduled_task("fish_vibrio")
# Windows task 'fish_vibrio' removed.

#If no task has this name, nothing is removed and you are told so:
# Windows task 'fish_vibrio' not found, nothing was removed.
```

The function reports what actually happened. It checks that the task
exists before removing it and that it is gone afterwards.

## Modify a scheduled task

There is no function to directly modify a scheduled task at the moment.
You can simply:

1.  Remove the scheduled task (see above)
2.  Setup a new task → see [LitFetchR
    (Tutorial)](https://thomasdumond.github.io/LitFetchR/articles/LitFetchr.html)

A task with the same name is never overwritten by
[`auto_LitFetchR_setup()`](https://thomasdumond.github.io/LitFetchR/reference/auto_LitFetchR_setup.md),
which is why the old task has to be removed first. Your history files
and *run_log.csv* are kept, so the new task continues from the
references already retrieved.
