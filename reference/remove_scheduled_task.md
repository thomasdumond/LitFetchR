# Removes a scheduled task.

Removes a scheduled task using the "task_id" from Task Scheduler
(Windows) or Cron (Mac/Linux).

## Usage

``` r
remove_scheduled_task(task_id, dry_run = FALSE)
```

## Arguments

- task_id:

  Name/ID of the scheduled task (Windows Task Scheduler or Cron).

- dry_run:

  Simulation run option.

## Value

`NULL` (invisibly). Called for its side effects: removes a scheduled
task saved using the function 'auto_LitFetchR_setup'.

## Details

The function checks whether the task exists first, removes it, then
checks that it is gone. It reports which of these happened. If the task
does not exist, nothing is removed and a message says so. If the
scheduler returns an error, or the task is still there afterwards, the
function stops and shows the scheduler's own message.

## Examples

``` r
# This is a "dry run" example.
# No task will actually be removed, it only shows how the function should react.
remove_scheduled_task("fish_vibrio",
                      dry_run = TRUE
                      )
#> This is the message from the dry run showing what you should be seeing when the function will be used:
#>             Windows task "fish_vibrio" removed.
#>             (or: Windows task "fish_vibrio" not found, nothing was removed.)
```
