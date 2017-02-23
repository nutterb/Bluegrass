#' @name group_levels
#' @title Consolidate Levels in a Factor
#' 
#' @description Simplifies the consolidation of multiple factor levels into 
#'   a smaller number of levels.
#'
#' @param x A vector. 
#' @param ... Additional named arguments.  The name should reflect the desired group 
#'   name, and should be accompanied by a character vector naming the values to 
#'   map to that group. A name associated with \code{NULL} will map any values 
#'   not declared in other arguments.  See Examples.
#'   
#' @return A factor variable with the same length of \code{x}.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return an object of class \code{factor} with the same length as \code{x}.
#'   \item All arguments passed to \code{...} must be vectors or \code{NULL}.
#'   \item No more than one argument passed to \code{...} may be \code{NULL}.
#' }
#' 
#' @seealso \code{\link{levels}}
#' 
#' @author Benjamin Nutter
#' 
#' @source
#' A Stack Overflow question prompted me to write this function:
#' \url{http://stackoverflow.com/questions/42423022/function-to-determine-proper-group/42423605}
#' @examples
#' # From the Stack Overflow Question that inspired this function:
#' DF <- data.frame(Members = c("Eva",  "Charlie1", "Fred",     "Charlie2", 
#'                              "Adam", "Eva",      "Charlie2", "David", 
#'                              "Adam", "David",    "Charlie1"))
#'                   
#' # Using `Group3 = NULL` means that anything that isn't defined in 
#' # `Group1` or `Group2` will be assigned to `Group3`
#' 
#' group_levels(DF$Members, 
#'              Group1 = c("Eva", "Adam"), 
#'              Group2 = c("Charlie1", "Charlie2"),
#'              Group3 = NULL)
#' 
#' # Excluding `Group3 = NULL` means that anything that isn't defined in
#' # `Group1` or `Group2` will be assigned `NA`
#' 
#' group_levels(DF$Members, 
#'              Group1 = c("Eva", "Adam"), 
#'              Group2 = c("Charlie1", "Charlie2"))
#' @export

group_levels <- function(x, ...)
{
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertAtomic(x = x,
                          add = coll)
  
  lev <- list(...)

  null_lev <- vapply(X = lev,
                     FUN = is.null,
                     FUN.VALUE = logical(1))
  
  if (! all(vapply(X = lev,
                   FUN = checkmate::test_atomic,
                   FUN.VALUE = logical(1))))
  {
    coll$push("Each element to `...` must be a vector or `NULL`")
  }
  
  if (sum(null_lev) > 1)
  {
    coll$push("No more than one element to `...` may be `NULL`")
  }
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  
  if (is.factor(x))
  {
    x <- as.character(x)
  }
  
  lev[null_lev] <- 
    list(unique(x[! x %in% unlist(lev[!null_lev])]))
  
  x <- factor(x)
  levels(x) <- lev
  x
}
