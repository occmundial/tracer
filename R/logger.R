#' @title Simple logging utility
#'
#' @description
#' Creates an Logger class
#'
#' @export
#'
Logger = R6::R6Class(
  classname = "Logger",
  public = list(
    initialize = function(
    level = c("info", "fatal", "error", "warn", "debug", "trace", "off", "all"),
    name = "logger", printer = NULL) {
      self$set_log_level(level)
      self$set_name(name)
      self$set_printer(printer)
    },
    set_name = function(name = "logger") {
      private$name = as.character(x = name)
      invisible(self)
    },
    set_log_level = function(level = c("info", "fatal", "error", "warn", "debug", "trace", "off", "all")) {
      level = match.arg(level)
      level = logging_constants[[level]]
      private$level = level
      invisible(self)
    },
    set_printer = function(FUN = NULL) {
      if (is.null(FUN)) {
        FUN = function(timestamp, level, logger_name, pid, message, ...) {
          log_msg = list(
            timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"),
            level = as.character(level),
            name = as.character(logger_name),
            pid = as.integer(pid),
            msg = message
          )
          extra = list(...)
          if (length(extra) > 0) {
            log_msg = c(log_msg, extra)
          }
          x = yyjsonr::write_json_str(log_msg, opts = list(dataframe = "columns", auto_unbox = TRUE))
          writeLines(x)
          flush(stdout())
        }
      }
      if (!is.function(FUN))
        stop("'FUN' should function or NULL")
      if (length(formals(FUN)) != 6L)
        stop("FUN should be a function with 6 formal arguments - (timestamp, level, logger_name, pid, message, ...)")
      private$printer = FUN
      return(invisible(self))
    },
    trace = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$trace, log_level_tag = "TRACE")
    },
    debug = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$debug, log_level_tag = "DEBUG")
    },
    info = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$info, log_level_tag = "INFO")
    },
    warn = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$warn, log_level_tag = "WARN")
    },
    error = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$error, log_level_tag = "ERROR")
    },
    fatal = function(msg, ...) {
      private$log_base(msg, ..., log_level = logging_constants$fatal, log_level_tag = "FATAL")
    }
  ),
  private = list(
    printer = NULL,
    level = NULL,
    name = NULL,
    log_base = function(msg, ..., log_level, log_level_tag) {
      if (isTRUE(private$level >= log_level) || is.na(private$level)) {
        private$printer(Sys.time(), log_level_tag, private$name, Sys.getpid(), msg, ...)
      }
      invisible(msg)
    }
  )
)

logging_constants = list(
  "fatal" = 100,
  "error" = 200,
  "warn" = 300,
  "info" = 400,
  "debug" = 500,
  "trace" = 600,
  "off" = 0,
  "all" = NA_real_
)
