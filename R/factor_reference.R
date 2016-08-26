#' @name factor_reference
#' @title Generate a list of reference tables for factors in a data set
#' 
#' @description Creates a list consisting of one data frame for each factor in a 
#'   data set.  Each table has two columns; \code{level} and \code{label}.
#'   
#' @param data A data frame-like object.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accept an object that inherits the \code{data.frame} class
#'   \item Return a list of data frames, one for each factor in \code{data}
#'   \item Return a message indicating when no factors are found.
#' }
#' 
#' @export

factor_reference <- function(data)
{
  checkmate::assert_class(x = data, 
                          classes = "data.frame")
  
  factor_vars <- vapply(X = data, 
                        FUN = is.factor, 
                        FUN.VALUE = logical(1))
  
  if (!sum(factor_vars))
  {
    message("No factors were found in `data`")
  }
  else
  {
    lapply(data[factor_vars],
             function(x)
             {
               if (is.factor(x)) data.frame(level = seq_along(levels(x)),
                                            label = levels(x))
               else NULL
             }
      )
  }
}
