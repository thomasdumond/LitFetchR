#' Remove a scheduled task by name from Task Scheduler (Windows) or Cron (Mac/Linux)
#' @param taskname Name/ID of the scheduled task (Windows Task Scheduler or cronR id)
#'
#' @examples
#' \dontrun{
#'
#' #Example of what you should see:
#' > remove_scheduled_task("fish_vibrio")
#' SUCCESS: The scheduled task "fish_vibrio" was successfully deleted.
#' Windows task 'fish_vibrio' removed (or did not exist).
#' }
#'
#' @export
remove_scheduled_task <- function(taskname) {

  if (.Platform$OS.type == "windows") {
    # Windows: use taskscheduleR
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
    # macOS / Linux: use cronR
    if (!requireNamespace("cronR", quietly = TRUE)) {
      stop("Package 'cronR' is required on this OS.")
    }

    cronR::cron_rm(taskname)

  }

  invisible(NULL)
}
