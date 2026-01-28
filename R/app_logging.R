#' Setup per-session logging to file
#'
#' Captures input changes, stdout, and stderr for a Shiny session.
#' Log directory controlled by LOG_DIR env var (default: ./logs).
#'
#' @param session Shiny session object
#' @param input Shiny input object
#' @param excluded_inputs_extra Character vector of additional input IDs to exclude from logging
#' @export
setUpAppLogging <- function(session, input, excluded_inputs_extra = NULL) {
  session_id <- session$token
  log_dir <- Sys.getenv("LOG_DIR", unset = "./logs")
  timestamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
  log_file <- file.path(log_dir, paste0(timestamp, "_", session_id, ".log"))

  message("Setting up logging for session: ", session_id)
  message("Log directory: ", log_dir)
  message("Log file: ", log_file)

  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

  # Setup logger for this session's namespace
  logger::log_appender(logger::appender_file(log_file), namespace = session_id)
  logger::log_threshold(logger::TRACE, namespace = session_id)

  # Auto-capture inputs (excluding sensitive fields)
  # Always exclude library-defined password fields, plus any user-specified extras
  excluded_inputs <- c("bfabric8-webservicepassword", excluded_inputs_extra)
  logger::log_shiny_input_changes(
    input,
    namespace = session_id,
    excluded_inputs = excluded_inputs
  )

  # Capture stdout/stderr (print, cat, etc.)
  sink_file <- file(log_file, open = "at")
  sink(sink_file, append = TRUE, type = "output")
  sink(sink_file, append = TRUE, type = "message")

  session$onSessionEnded(function() {
    sink(type = "output")
    sink(type = "message")
    close(sink_file)
  })

  logger::log_info("Session started: {session_id}", namespace = session_id)
  message("Logging setup complete")

  invisible(NULL)
}
