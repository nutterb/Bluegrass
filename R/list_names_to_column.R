#' @name list_names_to_column
#' @title Make list names a column name
#' 
#' @description Operations on a list of data frames, such as reading in
#'   multiple files, do not always place an identifier for the list in the
#'   data frame itself.  The function provided here takes the name and places
#'   it in the data frame as a column. This may be accomplished with an 
#'   \code{mapply} call, but who wants to write those eight lines of code?
#'   
#' @param list_data a named list of data frames.
#' @param new_col \code{character(1)}, the name of the new column in the data 
#'   frame.
#' 
#' @return 
#' A list of data frames. Each data frame has an additional column given by 
#' \code{new_col}.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return a list of data frames.
#'  \item Each data frame in the list has a new column named by \code{new_col}
#'  \item Cast an error if \code{list_data} is not a named list of data frames.
#'  \item Cast an error if \code{new_col} is not a \code{character(1)}
#'  \item Cast an error if \code{new_col} exists in any element of 
#'    \code{list_data}.
#' }
#' 
#' @export

list_names_to_column <- function(list_data, new_col)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_list(x = list_data,
                         names = "named",
                         types = "data.frame",
                         add = coll)
  
  checkmate::assert_character(x = new_col,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  new_col_exist <- 
    vapply(list_data,
           FUN = function(x) new_col %in% names(x),
           logical(1))
  
  if (any(new_col_exist)){
    has_new_col <- names(list_data)[new_col_exist]
    
    coll$push(paste0("The following list elements already have a ",
                     "column named ", new_col, ": ",
                     paste0(has_new_col, collapse = ", ")))
  }
  
  checkmate::reportAssertions(coll)
  
  mapply(
    FUN = function(x, n){
      x[[new_col]] <- n
      x
    },
    x = list_data,
    n = names(list_data),
    SIMPLIFY = FALSE
  )
}