#' @name is_equal
#' @title Vectorized Equality Comparison
#' 
#' @description A vectorized wrapper for \code{isTRUE(all.equal(x, y))} for
#'   comparing pairwise elements of two vectors for equality.
#'   
#' @param x A vector. must have the same length as \code{y}
#' @param y A vector. Must have the same length as \code{x}
#' @param compare_na \code{logical(1)}. When \code{FALSE}, if either 
#'   \code{x} or \code{y} is \code{NA}, the value returned is \code{NA}. If 
#'   \code{TRUE}, a logical value will be returned indicating if both 
#'   \code{x} and \code{y} are \code{NA}.
#' @param use_names \code{logical(1)}. If \code{x} and/or \code{y} have names,
#'   the return value may be named. if \code{TRUE} names are preserved when
#'   available, otherwise they are suppressed.  See the \code{USE.NAMES} 
#'   argument in \code{\link{mapply}}.
#'   
#' @return A logical vector the same length as \code{x}
#' 
#' @seealso \code{\link{all.equal}}, \code{\link{isTRUE}}, \code{\link{mapply}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Cast an error if either \code{x} or \code{y} are not vectors.
#'   \item Cast an error if \code{x} and \code{y} do not have the same length.
#'   \item Return a logical vector the same length of \code{x}
#'   \item Give control over how \code{NA} are handled.
#'   \item Cast a warning if \code{x} and \code{y} do not share a class
#' }
#' 
#' @examples 
#' is_equal(1:3, 1:3)
#' 
#' is_equal(c(1, 1, 2), 
#'          c(1, 2, 3))
#'          
#' is_equal(c(1, NA, 2),
#'          c(1, 3, 2))
#'          
#' is_equal(c(1, NA, 2),
#'          c(1, 3, 2),
#'          compare_na = TRUE)
#'          
#' is_equal(c(1, NA, 2),
#'          c(1, NA, 2))
#'          
#' is_equal(c(1, NA, 2),
#'          c(1, NA, 2),
#'          compare_na = TRUE)
#'          
#' @export

is_equal <- function(x, y, compare_na = FALSE, use_names = TRUE)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_atomic_vector(x = x,
                                  add = coll)
  
  checkmate::assert_atomic_vector(x = y,
                                  add = coll)
  
  if (length(x) != length(y))
  {
    coll$push("`x` and `y` must have the same length")
  }
  
  checkmate::assert_logical(x = compare_na,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = use_names,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (!inherits(x, class(y)))
  {
    warning("`x` and `y` are not of a similar class. All comparisons will be FALSE")
  }
  
  out <- 
    mapply(FUN = function(x, y) isTRUE(all.equal(x, y)),
           x = x,
           y = y,
           USE.NAMES = use_names, 
           SIMPLIFY = TRUE)
  
  out <- unlist(out)
  
  if (!compare_na)
  {
    out[which(is.na(x) | is.na(y))] <- NA
  }
  
  if (!is.logical(out))
  {
    return(as.logical(out))
  }
  
  out
}