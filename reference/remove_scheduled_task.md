# Removes a scheduled task using the "task_ID" from Task Scheduler (Windows) or Cron (Mac/Linux).

Removes a scheduled task using the "task_ID" from Task Scheduler
(Windows) or Cron (Mac/Linux).

## Usage

``` r
remove_scheduled_task(taskname, dry_run = FALSE)
```

## Arguments

- taskname:

  Name/ID of the scheduled task (Windows Task Scheduler or Cron).

- dry_run:

  Simulation run option.

## Examples

``` r
# This is a "dry run" example.
# No task will actually be removed, it only shows how the function should react.
remove_scheduled_task("fish_vibrio",
                      dry_run = TRUE
                      )
#> SUCCESS: The scheduled task "fish_vibrio" was successfully deleted.
#>             Windows task "fish_vibrio" removed (or did not exist).
```
