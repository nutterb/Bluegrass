#' @name toggle_plural
#' @title Toggle Between Plural and Singular Word Choice
#'
#' @description When writing dynamic documents, it is not always known if a 
#'  value will be singular or plural.  Writing a document that accommodates
#'  both conditions can be challenging.  \code{toggle_plural} assists
#'  by giving the user a choice of words, and inserts the appropriate
#'  word based on the value being printed.
#' 
#' @param x \code{numeric(1)}
#' @param singular \code{character(1)}, defines the word choice to use if 
#'  \code{x} is equal to 1.
#' @param plural \code{character(1)}, defines the word choice to use if
#'   \code{x} is not equal to 1.
#' @param ... Additional arguments to pass to \code{\link{format}}
#' 
#' @author Benjamin Nutter
#' 
#' @return Returns a \code{character(1)} object.
#'
#' @seealso \code{\link{toggle_text}}, \code{\link{format}}, \code{\link{all.equal}}, 
#'   \code{\link{isTRUE}}
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return an error if \code{x} is not numeric.
#'   \item Return an error if either \code{singular} or plural is not
#'     a character string.
#'   \item Return \code{singular} if \code{x} is equal to 1, else 
#'     return \code{plural}.
#' }
#' 
#' @examples
#' toggle_plural(1, "was", "were")
#'
#' toggle_plural(1, "is", "are")
#' 
#' @export

toggle_plural <- function(x, singular, plural, ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_numeric(x = x,
                            len = 1,
                            add = coll)
  
  checkmate::assert_character(x = singular,
                              len = 1,
                              add = coll)
  
  checkmate::assert_character(x = plural,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  sprintf("%s %s",
          if(isTRUE(all.equal(x, 1))) singular else plural,
          format(x, ...))
}
