#' @name FirstLastRecord
#' @export firstRecord
#' @importFrom dplyr arrange_
#' @importFrom dplyr filter
#' @importFrom dplyr group_by_
#' @importFrom dplyr mutate
#' 
#' @title First, Last, and nth Records
#' @description Extracts specific records from within an id in a repeated
#'   measures data set.
#' 
#' @param data A data frame to be reduced to the first row per identifiers
#' @param id a vector of identifiers
#' @param ... additional variables by which the data should be sorted prior
#'   to selecting the first per identifier set.
#' @param n The record number to pull from each ID.
#'   
#' @details The data frame will first be sorted by the variables in \code{id} 
#'   and then by the variables in \code{...}. 
#'   
#'   Sorting is performed by the \code{dplyr::arrange} function, and accepts 
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
  id2 <- as.character(substitute(id))
  id2 <- sub(pattern = "[[:space:]]+", 
             replacement = "", 
             x = id2)
  id2 <- ifelse(test = substr(id2, 1, 4) == "desc",
                yes = paste0("dplyr::", id2),
                no = id2)
  if (id2[1] %in% c("c", "list")) id2 <- id2[-1]
  
  id.dot <- as.character(substitute(list(...)))[-1]
  
  
  data <- data %>%
    dplyr::arrange_(c(id2, id.dot))
  
  data[!duplicated(data[, id2, drop=FALSE]), , drop=FALSE]
}

#' @rdname FirstLastRecord
#' @export 

lastRecord <- function(data, id, ...){
  id2 <- as.character(substitute(id))
  id2 <- sub(pattern = "[[:space:]]+", 
             replacement = "", 
             x = id2)
  id2 <- ifelse(test = substr(id2, 1, 4) == "desc",
                yes = paste0("dplyr::", id2),
                no = id2)
  if (id2[1] %in% c("c", "list")) id2 <- id2[-1]
  
  id.dot <- as.character(substitute(list(...)))[-1]
  
  
  data <- data %>%
    dplyr::arrange_(c(id2, id.dot))
  
  data[!duplicated(data[, id2, drop=FALSE], fromLast=TRUE), , drop=FALSE]
}

#' @rdname FirstLastRecord
#' @export

nthRecord <- function(data, id, ..., n=1){
  id2 <- as.character(substitute(id))
  id2 <- sub(pattern = "[[:space:]]+", 
             replacement = "", 
             x = id2)
  id2 <- ifelse(test = substr(id2, 1, 4) == "desc",
                yes = paste0("dplyr::", id2),
                no = id2)
  if (id2[1] %in% c("c", "list")) id2 <- id2[-1]
  
  id.dot <- as.character(substitute(list(...)))[-1]
  
  #* Hack my way around the visible binding in global variable note
  nth_record_sorted_index <- NULL 
  
  data <- data %>%
    dplyr::arrange_(c(id2, id.dot)) %>%
    dplyr::group_by_(id2) %>%
    dplyr::mutate(nth_record_sorted_index = 1:n()) %>%
    dplyr::filter(nth_record_sorted_index == n)
  
  data$nth_record_sorted_index <- NULL
  data
}



