#' @name month_regex
#' @title Regular Expression for Extracting Month Names from a String
#' 
#' @description Generates the regular expression string that will
#'   match any month name in a character string.  This can 
#'   be helpful for identifying month names in non standard date
#'   patterns.
#' 
#' @param abbreviated \code{logical(1)}. Should the abbreviated month names be 
#'   used?
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return a character vector that will match the month names.
#'   \item Permit the user to request abbreviated month names.
#' }
#' 
#' @export

month_regex <- function(abbreviated = FALSE)
{
  checkmate::assert_logical(x = abbreviated,
                            len = 1)
  (
    if (abbreviated) 
      month.abb
    else
      month.name
  ) %>%
    paste0(collapse = "|") %>%
    paste0("(", ., ")")
}
