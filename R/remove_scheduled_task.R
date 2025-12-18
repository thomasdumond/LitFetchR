#' Removes a scheduled task using the "task_ID" from Task Scheduler (Windows) or Cron (Mac/Linux).
#' @param taskname Name/ID of the scheduled task (Windows Task Scheduler or Cron).
#' @param dry_run Simulation run option.
#'
#' @examples
#' # This is a "dry run" example.
#' # No task will actually be removed, it only shows how the function should react.
#' remove_scheduled_task("fish_vibrio",
#'                       dry_run = TRUE
#'                       )
#'
#' @export

remove_scheduled_task <- function(taskname,
                                  dry_run = FALSE
                                  ) {

  if (dry_run) {
    message('SUCCESS: The scheduled task "fish_vibrio" was successfully deleted.
            Windows task "fish_vibrio" removed (or did not exist).')
    return(invisible(NULL))
  }

  # Identifies the OS of the computer before removing the task.
  if (.Platform$OS.type == "windows") {
    # Windows: uses taskscheduleR.
    if (!requireNamespace("taskscheduleR", quietly = TRUE)) {
      stop("Package 'taskscheduleR' is required on Windows.")
    }

    ok <- tryCatch(
      {
        taskscheduleR::taskscheduler_delete(taskname)
        TRUE
      },
      error = function(e) FALSE
    )

    if (ok) {
      message("Windows task '", taskname, "' removed (or did not exist).")
    } else {
      message("Could not remove Windows task '", taskname, "'.")
    }

  } else {
    # macOS / Linux: uses cronR.
    if (!requireNamespace("cronR", quietly = TRUE)) {
      stop("Package 'cronR' is required on this OS.")
    }

    cronR::cron_rm(taskname)

  }

  invisible(NULL)
}
