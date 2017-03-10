#' @name is_even
#' @title Test whether numeric values are odd or even
#' 
#' @description A number \code{x} is even if there exists some integer
#'   \code{k} such that \code{2 * k = x}. \code{is_odd(x)} is equivalent to 
#'   \code{!is_even(x)}
#'   
#' @param x A vector of numeric values.
#' @param require_int \code{logical(1)}. When \code{TRUE}, an error is 
#'   returned when any of the values is \code{x} is not an integer-like
#'   value.  Otherwise, \code{FALSE} is returned for non-integer-like values.
#'   
#' @seealso \code{\link{Arithmetic}}
#' 
#' @return A logical vector the length of \code{x}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return a logical vector the same length as x.
#'   \item Cast an error when given a non-numeric vector.
#'   \item Cast an error when \code{x} is not a vector.
#'   \item An option is available to return an error if any values in \code{x}
#'         are not integer-like values.
#' }
#' 
#' @examples 
#' is_even(1:10)
#' 
#' # Non integer-like values
#' is_even(1:10 + .15)
#' 
#' is_even(1:10, require_int = TRUE)
#' 
#' \dontrun{
#' # Return an error if x contains non-integer-like values
#' is_even(1:10 + 0.15, require_int = TRUE)
#' }
#' 
#' is_odd(1:10)
#' 
#' @export

is_even <- function(x, require_int = FALSE)
{
  checkmate::assert_logical(x = require_int,
                            len = 1)
  
  if (require_int)
  {
    checkmate::assert_integerish(x = x)
  }
  else
  {
    checkmate::assert_numeric(x = x)
  }
  
  x %% 2 == 0
}

#' @rdname is_even
#' @export

is_odd <- function(x, require_int = FALSE)
{
  !is_even(x, require_int = require_int)
}
