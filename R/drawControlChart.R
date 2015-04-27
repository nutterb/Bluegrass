#' @name drawControlChart
#' @import ggplot2
#' 
#' @title Generic Control Chart
#' @description Produces a basic and generic control chart with the values
#'   plotted, center line, and lower and upper control limits.  It is 
#'   expected that all values to be plotted will be provided to the function
#'   in \code{chartData}.  Accordingly, this function is not exported and 
#'   is used only internally by \code{Bluegrass}.
#'   
#' @param chartData A data frame (or similar object) with vectors described below.
#' 
#' @details The data frame requires five columns to be provided in order to 
#'   produce the control chart
#'  
#'  \enumerate{ 
#'   \item id Identifies the plotting order on the x-axis.
#'   \item measure Identifies the control measure, such as average (mean), median, 
#'     range, sd, etc.
#'   \item element Identifies what element of the plot is represented.  Should 
#'     be one of \code{value} (the data point to be plotted); \code{center} 
#'     (the location of the center line); \code{lcl} (the lower control limit); 
#'     or \code{ucl} (the upper control limit).
#'   \item element_class Identifies the class of element and is used to control 
#'     color, linetype, etc.  Expected values are \code{value}, \code{center}, 
#'     and \code{control_limit}
#'   \item value The value to be plotted for each element type.
#'  }
#'   
#' @return Returns a \code{ggplot2} object.
#' 
#' @author Benjamin Nutter
#' 
#' @seealso controlData, controlChart, Constants

drawControlChart <- function(chartData){
  #* Hack my way around the visible binding for global variable warning
  id <- value <- element_class <- NULL
  
#   if (!"package:ggplot2" %in% search()) require(ggplot2)
  ggplot2::ggplot(chartData, aes(x=id, y=value, colour=element_class)) + 
    ggplot2::geom_point(data=chartData[chartData$element == "value", , drop=FALSE]) + 
    ggplot2::geom_line(data=chartData[chartData$element == "value", , drop=FALSE]) +
    ggplot2::geom_line(data=chartData[chartData$element == "center", , drop=FALSE]) +
    ggplot2::geom_line(data=chartData[chartData$element == "lcl", , drop=FALSE]) + 
    ggplot2::geom_line(data=chartData[chartData$element == "ucl", , drop=FALSE]) + 
    ggplot2::facet_grid(measure ~ .) 
  
}

