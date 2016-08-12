#' @name tidy.summary.survfit
#' @title Tidy a Survfit Summary
#' 
#' @description Tidier for \code{summary.survfit} objects.  This isn't greatly different
#'   from \code{tidy.survfit}, except that it allows for the use of the \code{times} 
#'   argument.  It also doesn't contain any information with regard to censoring.
#'   
#' @param x a \code{summary.survfit} object.
#' @param ... Additional arguments to pass to other methods
#' @param detail A logical value indicating if the detailed output should be tidied, 
#'   or just the table of survival metrics (median survival, etc)
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item accept a \code{summary.survfit} object
#'   \item return a tidy \code{data.frame}
#' }
#' 
#' @author Benjamin Nutter
#' @export

tidy.summary.survfit <- function(x, detail = TRUE, ...)
{
  if (detail)
  {
    x[c("time", "n.risk", "n.event", "surv", "std.err", 
             "upper", "lower", "strata")] %>%
      as.data.frame() %>%
      stats::setNames(c("time", "n.risk", "n.event", "estimate", "std.error", 
                        "conf.high", "conf.low", "strata")) 
  }
  else
  {
    broom::tidy(x$table) %>%
      stats::setNames(c("strata", "records", "n.max", "n.start", "events", "rmean", "se.rmean",
                        "median", "conf.low", "conf.high"))
  }
}

utils::globalVariables(c("strata", "time"))