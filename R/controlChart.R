#' @name controlChart
#' @export controlChart
#' 
#' @title Shewhart Style Control Charts
#' @description Create basic plot structures for control charts and the data to create them 
#'   based on Shewhart's methods.  The intention of this function was to provide a flexible 
#'   structure from which any common control chart could be plotted.  Labeling and annotating 
#'   are left to the user. 
#' 
#' @param data Data frame
#' @param id Vector of length 1 indicating the sub-group IDs for the data
#' @param value Vector of length 1 indicating the values to be charted
#' @param center A character string denoting the type of center line to use.
#' @param variability A character string denoting the method by which to calculate the
#'   control limits.
#' @param give_me A character vector denoting what the user would like to receive from the
#'   function.  Valid options are \code{"plot"} (for the ggplot2 object) and \code{"data"}
#'   (for the data table with the plotting data)
#'   
#' @details \code{controlChart} supports the combination of any \code{center} metric having control
#'   limits and variability associated with any of the \code{variability} metrics.  The constants
#'   used to define the control limits for centrality measures are:
#'   
#' \tabular{lll}{
#'   measure      \tab variability  \tab control constant \cr
#'   mean         \tab mean_range   \tab A2 \cr
#'   mean         \tab median_range \tab A4 \cr
#'   mean         \tab moving_range \tab A2 \cr
#'   mean         \tab rms          \tab A1 \cr
#'   mean         \tab sd           \tab A3 \cr 
#'   median       \tab mean_range   \tab A6 \cr
#'   median       \tab median_range \tab A9
#' }
#' 
#' The constants used to define the control limits for variability measures are: 
#' 
#' \tabular{lll}{
#'   measure      \tab lower / upper control constant \cr
#'   mean_range   \tab D3 / D4   \cr
#'   median_range \tab D5 / D6   \cr
#'   moving_range \tab 0 / D4    \cr
#'   rms          \tab B3 / B4   \cr
#'   sd           \tab B3 / B4 
#' }
#' 
#' @return The output is determined by the contents of the \code{give_me} arguments.
#'
#'   \code{plot} Returns a \code{ggplot2} object with the core control chart.  Modification will
#'     likely be necessary.
#'     
#'   \code{data} Returns a \code{dat_tbl} object with the columns:
#'   \enumerate{
#'     \item id The subgroup ID
#'     \item measure The control chart measure (average = centrality, vary = variability)
#'     \item element The control chart element (value, center, lcl, ucl)
#'     \item element_class The control chart element class (value, center, control_limit)
#'     \item value The value to be plotted
#'   }
#'     
#'   If \code{give_me = c("plot", "data")}, both objects are returned.
#'     
#' @author Benjamin Nutter
#' 
#' @seealso Constants
#' 
#' @source
#' #' Donald J. Wheeler and David S. Chambers, \emph{Understanding Statistical Process Control, Second Edition},
#' SPC Press, Knoxville, TN, 1992. ISBN: 0-945320-13-2.\cr
#' 
#' Emanuel P. Barbosa, Flavio M. M. Barros, Elias de Jesus Goncalves and Daniela R. Recchia (2014). 
#' \emph{IQCC: Improved Quality Control Charts}. R package version 0.6. http://CRAN.R-project.org/package=IQCC
#' d2 and d3 are copied from their package.
#' 
controlChart <- function(data, id, value,
                         center = c("mean", "median"),
                         variability = c("mean_range", "median_range", 
                                         "moving_range",
                                         "rms", "sd"),
                         give_me = "plot"){
  if (!all(give_me %in% c("plot", "data"))) 
    stop("The only valid options for 'give_me' are 'plot' and 'data'")
  
  controlData <- controlData(data=data, 
                             id=as.character(substitute(id)), 
                             value=as.character(substitute(value)), 
                             center = center, 
                             variability = variability)
  
  if ("plot" %in% give_me) chart <- drawControlChart(controlData)
  
  if (all(c("plot", "data") %in% give_me))
  {
    return(list(plot = chart, 
                data = controlData))
  }
  else if (give_me == "plot") 
  {
    return(chart)
  }
  else if (give_me == "data")
  {
    return(controlData)
  }
}

