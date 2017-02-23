#' @name toggle_text
#' @title Toggle Text Output Based on a Condition
#' 
#' @description Toggle between two text strings based on the evaluation 
#'   of a logical.  This is helpful for writing dynamic reports where 
#'   the interpretation may differ depending on the result of a 
#'   condition.
#' 
#' @param condition An expression that resolves to a \code{logical(1)}
#' @param true \code{character(1)}, the string to print when 
#'   \code{condition} resolves to \code{TRUE}.
#' @param false \code{character(1)}, the string to print when 
#'   \code{condition} resolves to \code{FALSE}.
#' 
#' @author Benjamin Nutter
#' 
#' @seealso \code{toggle_plural}
#' 
#' @return \code{character(1)} object.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return an error if \code{condition} does not resolve to a logical.
#'   \item Return an error if either \code{true} or \code{false} are not 
#'     character strings.
#'   \item Return \code{true} when condition resolves to \code{TRUE}, else
#'     return \code{false}
#' }
#' 
#' @examples
#' p_value <- 0.01
#' toggle_text(p_value <= 0.05, 
#'             true = "is statistically significant",
#'             false = "is not statistically significant")
#' 
#' @export

toggle_text <- function(condition, true, false)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_logical(x = condition,
                            len = 1,
                            add = coll)
  
  checkmate::assert_character(x = true,
                              len = 1,
                              add = coll)
  
  checkmate::assert_character(x = false,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (condition) true
  else false
}
