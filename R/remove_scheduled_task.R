#' Removes a scheduled task.
#'
#' Removes a scheduled task using the "task_id"
#'  from Task Scheduler (Windows) or Cron (Mac/Linux).
#'
#' The function checks whether the task exists first, removes it, then checks
#' that it is gone. It reports which of these happened. If the task does not
#' exist, nothing is removed and a message says so. If the scheduler returns
#' an error, or the task is still there afterwards, the function stops and
#' shows the scheduler's own message.
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
            Windows task "fish_vibrio" removed.
            (or: Windows task "fish_vibrio" not found, nothing was removed.)')
    return(invisible(NULL))
  }

  task_id <- normalize_task_id(task_id)

  # Identifies the OS of the computer before removing the task.
  if (is_windows()) {
    # Windows: uses schtasks. Each result is checked before reporting.
    found <- win_task_query(task_id)
    if (!found$exists) {
      message("Windows task '", task_id,
              "' not found, nothing was removed.\n",
              "Task Scheduler said: ", paste(trimws(found$output), collapse = " "))
      return(invisible(NULL))
    }

    deleted <- win_task_delete(task_id)
    if (deleted$status != 0L) {
      stop("Windows Task Scheduler could not remove the task '", task_id,
           "'. The task may still be scheduled.\n",
           format_sched_failure(deleted), call. = FALSE)
    }
    if (win_task_query(task_id)$exists) {
      stop("Windows Task Scheduler reported success but the task '", task_id,
           "' is still there. The task may still be scheduled.",
           call. = FALSE)
    }
    message("Windows task '", task_id, "' removed.")

  } else {
    # macOS / Linux: uses cronR.
    if (!cron_available()) {
      stop("Package 'cronR' is required on this OS.")
    }
    if (is.null(cron_find(task_id))) {
      message("Cron task '", task_id, "' not found, nothing was removed.")
      return(invisible(NULL))
    }

    err <- cron_delete(task_id)
    if (!is.null(err)) {
      stop("cronR could not remove the job '", task_id,
           "'. The task may still be scheduled.\n", err, call. = FALSE)
    }
    if (!is.null(cron_find(task_id))) {
      stop("cronR reported success but the job '", task_id,
           "' is still in the crontab. The task may still be scheduled.",
           call. = FALSE)
    }
    message("Cron task '", task_id, "' removed.")
  }

  invisible(NULL)
}
