#' @name show_duplicated
#' @title Subset a Data Frame to Duplicated Subjects
#' 
#' @description Reduces a data frame to those subjects that are duplicated,
#'   but also includes the index record.
#'   
#' @param data A data frame
#' @param key A character vector naming the fields that identify unique
#'   subjects (experimental units) in \code{data}.
#'   
#' @details Using the \code{duplicated} function returns a logical vector
#'   indicating which records are duplicates, but will show \code{FALSE}
#'   for the first record of the subject being duplicated.  
#'   \code{show_duplicated} returns all of the rows for the subject
#'   for inspection.
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accept an object that inherits the \code{data.frame} class.
#'   \item Returns an object of the same class given.
#'   \item Gives all of the records for an experimental unit found to 
#'         have at least one duplication on \code{key} columns.
#' }
#'   
#' @author Benjamin Nutter
#' 
#' @examples 
#' DF <- data.frame(
#'   id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   week = c(1, 1, 2, 1, 2, 2, 1, 2, 3),
#'   obs = rep(letters[1:3], 3)
#' )
#' 
#' show_duplicated(DF, c("id", "week"))
#' 
#' show_duplicated(DF, c("id"))
#' 
#' show_duplicated(DF, c("id", "week", "obs"))
#' 
#' @export

show_duplicated <- function(data, key)
{
  orig_class <- class(data)
  
  #* arrange the data frame
  data <- dplyr::arrange_(data, 
                          .dots = key)
  
  #* Identify duplicates
  dup <- duplicated(data[, key])
  
  #* The first record with duplicates is always given a FALSE value in duplicated()
  #* Subtracting 1 from the vector of rownames returned by duplicated()
  #* includes that index value.
  dup <- sort(unique(c(which(dup), which(dup)-1)))
  dup_data <- data[dup, ]
  
  if ("data.table" %in% orig_class) 
  {
    return(data.table::as.data.table(dup_data))
  }
  else 
  {
    return(dup_data)
  }
}