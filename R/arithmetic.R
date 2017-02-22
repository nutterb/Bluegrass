#' @name arithmetic
#' @title Pipe Adapted Arithmetic Operators
#' 
#' @description Arithmetic operators adapted to the pipe. These are merely wrappers 
#'   for the \code{base} binary operators. The names used are slightly more informative
#'   than using the \code{base} operators in a pipe.  See Examples.
#' 
#' @param x,y  numeric or complex vectors or objects which can be coerced to such, 
#'   or other objects for which methods have been written.
#' 
#' @details See \code{\link{Arithmetic}} for details on usage.
#' 
#' \code{add(x, y)} maps to \code{x + y}\cr
#' \code{subtract(x, y)} maps to \code{x - y}\cr
#' \code{multiply(x, y)} maps to \code{x * y}\cr
#' \code{divide(x, y)} maps to \code{x / y}\cr
#' \code{raise(x, y)} maps to \code{x ^ y}\cr
#' \code{modulo(x, y)} maps to \code{x %% y}\cr
#' \code{remainder(x, y)} maps to \code{x %% y}\cr
#' \code{integer_division(x, y)} maps to \code{x %/% y}\cr
#' \code{int_div(x, y)} maps to \code{x %/% y}
#' 
#' @seealso \code{\link{Arithmetic}}
#' 
#' @examples
#' require(magrittr)
#' 1 + 1
#' 
#' # Pipe with base syntax (somewhat confusing)
#' 1 %>% `+`(1)
#' 
#' # Pipe with better syntax 
#' 1 %>% add(1)
#' 
#' 2 %>% raise(3)
#' 
#' @export

add <- function(x, y)
{
  x + y
}

#' @rdname arithmetic
#' @export

subtract <- function(x, y)
{
  x - y
}

#' @rdname arithmetic
#' @export

multiply <- function(x, y)
{
  x * y
}

#' @rdname arithmetic
#' @export

divide <- function(x, y)
{
  x / y
}

#' @rdname arithmetic
#' @export

raise <- function(x, y)
{
  x ^ y
}

#' @rdname arithmetic
#' @export

modulo <- function(x, y)
{
  x %% y
}

#' @rdname arithmetic
#' @export

remainder <- function(x, y)
{
  x %% y
}

#' @rdname arithmetic
#' @export

integer_division <- function(x, y)
{
  x %/% y
}

#' @rdname arithmetic
#' @export

int_div <- function(x, y)
{
  x %/% y
}
