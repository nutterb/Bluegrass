#' @name FirstLastRecord
#' @export firstRecord
#' @importFrom plyr arrange
#' @importFrom plyr desc
#' 
#' @title First, Last, and nth Records
#' @description Extracts specific records from within an id in a repeated
#'   measures data set.
#' 
#' @param data A data frame to be reduced to the first row per identifiers
#' @param id a vector of identifiers
#' @param ... additional variables by which the data should be sorted prior
#'   to selecting the first per identifier set.
#'   
#' @details The data frame will first be sorted by the variables in \code{id} 
#'   and then by the variables in \code{...}. 
#'   
#'   Sorting is performed by the \code{plyr::arrange} function, and accepts 
#'   use of the \code{desc} to sort a variable in descending order.  In both
#'   \code{firstRecord} and \code{lastRecord}, sorting should be done in the
#'   natural sort order of the variable; the last record is identified using
#'   the \code{duplicated} function with \code{fromLast=TRUE}.
#'   
#'   For \code{nthRecord}, using \code{desc} around each of the variables in 
#'   \code{...} will provide the nth-from-last record for each id. 
#'   
#' @author Benjamin Nutter
#' @seealso \code{order}, \code{duplicated}

firstRecord <- function(data, id, ...){
  #* Order the data frame
  if (as.character(substitute(id))[1] == "c")
    id2 <- paste(as.character(substitute(id))[-1], collapse=", ")
  else id2 <- as.character(substitute(id))
  
  expr <- gsub("desc[(]", "plyr::desc(",
               as.character(substitute(plyr::arrange(data, id2, ...))))
  expr <- paste0(expr[1], "(", paste(expr[-1], collapse=","), ")")
  data <- eval(parse(text=expr))
  
  #* Identify duplicates
  expr <- paste0("duplicated(data[, c('", 
                 gsub(", ", "', '", id2), 
                 "'), drop=FALSE])")
  dup <- eval(parse(text=expr))
  return(data[!dup, , drop=FALSE])
}

