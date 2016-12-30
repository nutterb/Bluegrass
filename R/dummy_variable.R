#' @name dummy_variable
#' @title Create Columns of Dummy Variables
#' 
#' @description Create a data frame of dummy variables.  There will be 
#'   n-1 variables in the data frame, where n is the number of levels in 
#'   the factor.
#'   
#' @param data An object that inherits data frame.
#' @param x A \code{character(1)} naming a variable in \code{data}
#' @param lev A vector of length 1. The value in \code{x} to use as the 
#'   reference value.
#' @param var_name \code{logical(1)}. When \code{TRUE}, the value of 
#'   \code{x} is prepended to the level, separated by an underscore.
#' @param as_logical \code{logical(1)}. When \code{TRUE}, the dummy variables
#'   are returned as logical values.  By default, they are returned as 0/1 values.
#' 
#' @section Functional Requirements:
#' 
#' In the requirements, \code{x} is used as shorthand for \code{data[["x"]]}
#' 
#' \enumerate{
#'   \item Accepts an object that inherits \code{data.frame}
#'   \item Accepts a \code{character(1)} naming a variable in \code{data}
#'   \item Casts an error when \code{x} is not in \code{data}
#'   \item Accepts a length 1 vector that names a value in \code{x} to use as 
#'     the reference value.  
#'   \item Casts and error if \code{lev} is not a value in \code{x}
#'   \item Provide an option to prepend the variable name to the front of 
#'     the level name.
#'   \item Provide an option to return dummy values as logical values.
#' }
#' 
#' @examples 
#' # Dummy Variables as 0/1
#' dummy_variable(iris, "Species")
#' 
#' # Dummy Variables as 0/1 with prefix
#' dummy_variable(iris, "Species", var_name = TRUE)
#' 
#' # Dummy Variables as TRUE/FALSE
#' dummy_variable(iris, "Species", as_logical = TRUE)
#' 
#' # Dummy Variables as TRUE/FALSE with prefix
#' dummy_variable(iris, "Species", var_name = TRUE, as_logical = TRUE)
#' 
#' # Dummy Variables as 0/1 with alternate reference
#' dummy_variable(iris, "Species", lev = "virginica")
#' 
#' @export

dummy_variable <- function(data, x, lev = NULL, var_name = FALSE, as_logical = FALSE)
{
# Argument Validations ----------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = data,
                          classes = "data.frame",
                          add = coll)
  
  checkmate::assert_character(x = x,
                              len = 1,
                              add = coll)
  
  checkmate::assert_logical(x = var_name,
                            len = 1,
                            add = coll)
  
  # lev validations
  
  if (!is.null(lev))
  {
    checkmate::assert_vector(x = lev,
                             len = 1,
                             add = coll)
    lev <- as.character(lev)
  }
  
  checkmate::reportAssertions(coll)
  
  # Second round of validations
  # x is in names(data)
  
  checkmate::assert_subset(x = x,
                           choices = names(data),
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Third round of validations
  # lev is in levels(x)
  
  x_fac <- factor(data[[x]])
  
  if (!is.null(lev))
  {
    checkmate::assert_subset(x = lev,
                             choices = levels(x_fac),
                             add = coll)
  }
  else
  {
    lev <- levels(x_fac)[1]
  }
  
  checkmate::reportAssertions(coll)
  
# Functional Code ---------------------------------------------------
  
  dummy_level <- levels(x_fac)[!levels(x_fac) %in% lev]
  dummy_name <- 
    if (var_name)
    {
      sprintf("%s_%s",
              x, 
              dummy_level)
    }
    else
    {
      dummy_level
    }
  
  dummy <- 
    lapply(dummy_level,
           make_dummy,
           x = x_fac,
           as_logical = as_logical) %>%
    stats::setNames(dummy_name) %>%
    as.data.frame()
  
  dplyr::bind_cols(data, dummy)
}

# Unexported Methods ------------------------------------------------

make_dummy <- function(lev, x, as_logical)
{
  if (as_logical)
  {
    x %in% lev
  }
  else 
  {
    x %in% lev + 0L
  }
}