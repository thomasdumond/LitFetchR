#' Removes a scheduled task.
#'
#' Removes a scheduled task using the "task_id"
#'  from Task Scheduler (Windows) or Cron (Mac/Linux).
#'
#' @param task_id Name/ID of the scheduled task (Windows Task Scheduler or Cron).
#' @param dry_run Simulation run option.
#'
#' @return \code{NULL} (invisibly). Called for its side effects: removes a scheduled task saved using the function 'auto_LitFetchR_setup'.
#'
#' @examples
#' # This is a "dry run" example.
#' # No task will actually be removed, it only shows how the function should react.
#' remove_scheduled_task("fish_vibrio",
#'                       dry_run = TRUE
#'                       )
#'
#' @export

remove_scheduled_task <- function(task_id,
                                  dry_run = FALSE
                                  ) {

  if (dry_run) {
    message('This is the message from the dry run showing what you should be seeing when the function will be used:
            SUCCESS: The scheduled task "fish_vibrio" was successfully deleted.
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
        taskscheduleR::taskscheduler_delete(task_id)
        TRUE
      },
      error = function(e) FALSE
    )

    if (ok) {
      message("Windows task '", task_id, "' removed (or did not exist).")
    } else {
      message("Could not remove Windows task '", task_id, "'.")
    }

  } else {
    # macOS / Linux: uses cronR.
    if (!requireNamespace("cronR", quietly = TRUE)) {
      stop("Package 'cronR' is required on this OS.")
    }
    ok <- tryCatch(
      {
        cronR::cron_rm(task_id)
        TRUE
      },
      error = function(e) FALSE
    )
    if (ok) {
      message("Cron task '", task_id, "' removed (or did not exist).")
    } else {
      message("Could not remove Cron task '", task_id, "'.")
    }

  }

  invisible(NULL)
}
