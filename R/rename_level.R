#' @name rename_level
#' @title Rename Factor Levels
#' 
#' @description Tool for renaming an arbitrary number of levels in a factor
#'   variable. NOTE: This may be done using the \code{forcats} package.
#'   
#' @param x a vector of class \code{factor}
#' @param ... Additional, named arguments where the name is an existing
#'   level in \code{x} and the argument value is a \code{character(1)} 
#'   (or \code{NA}) giving the replacement value.
#' @param na_val \code{character(1)} (or \code{NA}) giving the value to 
#'   assign as the level of any missing values.  This level will be placed
#'   at the end of the \code{levels} attribute.
#'   
#' @details This is a wrapper for \code{\link[plyr]{revalue}}. It performs 
#'   some additional aregument validations, and then calls \code{revalue}.
#'   This permits \code{revalue} to be used without having to load the
#'   \code{plyr}, which can cause masking problems if not done in the 
#'   right order around \code{dplyr}.
#'   
#' @seealso \code{\link[plyr]{revalue}}
#' 
#' @source Motivated by a question on stack overflow. 
#' \url{http://stackoverflow.com/questions/42436552/cleaning-replacing-nas-in-data-for-factorial-variables}
#' 
#' @section: Functional Requirements
#' \enumerate{
#'   \item Cast an error if \code{x} is not a factor variable
#'   \item Cast an error if any element in \code{...} is not named.
#'   \item Cast a warning if any name in \code{...} is not an 
#'     existing level of \code{x}.
#'   \item Cast an error if any element in \code{...} is not a character(1)
#'   \item Permit missing values to be assigned a character level label.
#' }
#' 
#' @examples 
#' val <- factor(LETTERS[1:5])
#' rename_level(val, "F" = "f")
#' 
#' rename_level(val,
#'              A = "Q",
#'              D = "Dinosaur")
#'              
#' val_with_na <- factor(c(LETTERS[1:3], NA, LETTERS[4:6]))
#' rename_level(val_with_na, A = "hello", na_val = "sweet", D = NA)
#' 
#' @export

rename_level <- function(x, ..., na_val = NA)
{
# Argument Validation -----------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_factor(x = x, 
                           add = coll)
  
  lev_arg <- list(...)
  
  if (!length(lev_arg))
  {
    coll$push("`...` must have at least one value")
  }
  
  if (!checkmate::test_named(lev_arg))
  {
    coll$push("All arguments in `...` must be named")
  }
  
  if (!all(vapply(lev_arg,
                  FUN = checkmate::test_character,
                  FUN.VALUE = logical(1),
                  len = 1)))
  {
    coll$push("All arguments in `...` must be `character(1)` objects")
  }
  
  checkmate::assert_character(x = na_val,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  not_in_x <- 
    lev_arg[which(!names(lev_arg) %in% levels(x))]
  
  if (length(not_in_x))
  {
    warning(sprintf("The following levels were not found in `x`: %s",
                    paste0(not_in_x, collapse = ", ")))
  }
    
# Functional Code ---------------------------------------------------

  if (!is.na(na_val))
  {
    lev <- levels(x)
    x <- as.character(x)
    x[is.na(x)] <- na_val
    x <- factor(x,
                levels = c(lev, na_val))
  }
    
  plyr::revalue(x = x, 
                replace = unlist(lev_arg), 
                warn_missing = FALSE)
}