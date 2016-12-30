#' @name with_warning
#' @title Evaluate an expression and capture any warning messages
#' 
#' @description Running a funtion like \code{univariable_table} has the 
#'   potential to produce several warnings, but can do so while making it 
#'   difficult to determine which of the variable tests generated the 
#'   warning.  This captures the warning without casting it so the warnings 
#'   can all be gathered and printed at the same time.
#'   
#' @param expr An expression.
#' 

with_warning <- function(expr) {
  expr_warn <- NULL
  warn_handler <- function(w) {
    expr_warn <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = warn_handler)
  list(value = val, warnings = expr_warn)
}
