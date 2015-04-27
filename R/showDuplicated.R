#' @name showDuplicated
#' @export showDuplicated
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' 
#' @title Subset a Data Frame to Duplicated Subjects
#' @description Reduces a data frame to those subjects that are duplicated,
#'   but also includes the index record.
#'   
#' @param data A data frame
#' @param ... the variable names in the data frame that indicate distinct records.
#'   These should be unquoted and may include any number of fields.
#'   
#' @details The data frame is first sorted by the fields in \code{...}.  The user
#'   may also specify fields to be sorted in descencing order via the \code{desc}
#'   function in \code{plyr}, but it is not strictly necessary.
#'   
#'   It is assumed that the variable names do not include parentheses.
#'   
#' @author Benjamin Nutter
#' 

showDuplicated <- function(data, ...){
  vars <- as.character(substitute(list(...)))[-1]
  
  #* arrange the data frame
  arrange_vars <- gsub("desc[(]", "dplyr::desc(", vars)
  data <- dplyr::arrange_(data, arrange_vars)
  
  #* Identify duplicates
  vector_str <- gsub("desc[(]", "", vars)
  vector_str <- gsub("[)]", "", vector_str)
  dup <- duplicated(data[, vector_str])
  
  #* The first record with duplicates is always given a FALSE value in duplicated()
  #* Subtracting 1 from the vector of rownames returned by duplicated()
  #* includes that index value.
  dup <- sort(unique(c(which(dup), which(dup)-1)))
  data[dup, ]
}
