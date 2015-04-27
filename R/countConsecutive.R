#' @name countConsecutive
#' @importFrom dplyr data_frame
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' 
#' @title Count Runs of Observations Satisfying a Condition
#' @description Shewhart's rules provide conditions under which a process can
#'   be considered to be out of control. \code{countConsecutive} generates
#'   an index by which runs can be systematically identified and can 
#'   facilitate identifying these observations on control charts.
#'   
#' @param x A numeric vector of observations.
#' @param condition An unevaluated logical expression based on \code{x}.
#' @param maxIndex Logical. When \code{TRUE}, each element is the maximum run
#'   length of each unique run.  When \code{FALSE}, each element is the element's
#'   individual run count.
#' @param ... Additional arguments to be passed to \code{plyr::ddply} 
#'   (especially for running in parallel).
#'   
#' @details Shewhart's rules for identifying out of control processes are:
#' \enumerate{
#'   \item Any one point is more than three standard units from the central line
#'   \item Any two consecutive points are more than two standard units in the 
#'         same direction from the central line 
#'   \item A run of ___ consecutive points are more than one standard units in 
#'         the same direction from the central line
#'   \item A run of eight consecutive points are on the same side of the 
#'         central line.
#' }
#'         
#' @return A vector the length of \code{x} giving each elements run length or 
#'   run count.
#'   
#' @author Benjamin Nutter
#' 

countConsecutive <- function(x, condition, maxIndex=TRUE, ...){
  #* Hack my way around the global binding warning
  logic <- toggle <- toggle_index <- NULL
  
  Runs <- 
    dplyr::data_frame(logic = eval(condition)) %>%
    #* toggle will be 0 when (i-1) == i, and 1 otherwise.  
    #* A 1 indicates a change from the previous value.  
    #* toggle_index becomes a key indicating a new run.
    dplyr::mutate(toggle = ifelse(c(0, diff(logic)) == 0,
                                  0, 1),
                  toggle_index = cumsum(toggle)) %>%
    dplyr::group_by(toggle_index) %>%
    dplyr::mutate(run = cumsum(logic),
                  maxRun = sum(logic))
  
  if (maxIndex) return(Runs$maxRun) else return(Runs$run)
}