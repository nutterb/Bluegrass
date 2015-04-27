#' @name controlData
#' @export controlData
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise_
#' @importFrom dplyr tbl_df
#' @importFrom tidyr gather
#' 
#' 
#' @title Data for Constructing Shewhart Control Charts
#' @description Produces a data frame suitable for making control charts in \code{ggplot2} 
#' 
#' @param data Data frame
#' @param id Vector of length 1 indicating the sub-group IDs for the data
#' @param value Vector of length 1 indicating the values to be charted
#' @param center A character string denoting the type of center line to use.
#' @param variability A character string denoting the method by which to calculate the
#'   control limits.
#'   
#' @details \code{controlData} supports the combination of any \code{center} metric having control
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
#' @return 
#' Returns a \code{dat_tbl} object with the columns:
#' \enumerate{
#'     \item id The subgroup ID
#'     \item measure The control chart measure (average = centrality, vary = variability)
#'     \item element The control chart element (value, center, lcl, ucl)
#'     \item element_class The control chart element class (value, center, control_limit)
#'     \item value The value to be plotted
#' }
#'     
#' @author Benjamin Nutter
#' 
#' @seealso Constants, controlChart
#' 
#' @source
#' #' Donald J. Wheeler and David S. Chambers, \emph{Understanding Statistical Process Control, Second Edition},
#' SPC Press, Knoxville, TN, 1992. ISBN: 0-945320-13-2.\cr
#' 
#' Emanuel P. Barbosa, Flavio M. M. Barros, Elias de Jesus Goncalves and Daniela R. Recchia (2014). 
#' \emph{IQCC: Improved Quality Control Charts}. R package version 0.6. http://CRAN.R-project.org/package=IQCC
#' d2 and d3 are copied from their package.

controlData <- function(data, id, value,
                        center = c("mean", "median"),
                        variability = c("mean_range", "median_range", 
                                        "moving_range",
                                        "rms", "sd")){
  center <- match.arg(center, c("mean", "median"))
  variability <- match.arg(variability, c("mean_range", "median_range", "moving_range",
                                          "rms", "sd"))
  
  ctr_fn <- if (center == "mean") quote(mean(value, na.rm=TRUE)) 
            else if (center == "median") quote(median(value, na.rm=TRUE))
  
  var_fn <- if (grepl("^_range", variability)) quote(max(value, na.rm=TRUE) - min(value, na.rm=TRUE))
            else if (variability == "rms") quote((n() - 1) / n() * sd(value, na.rm=TRUE))
            else if (variability == "sd") quote(sd(value, na.rm=TRUE))
            else if (variability == "moving_range") quote(mean(mr, na.rm=TRUE))
  
  var_lim <- if (grepl("median", variability)) quote(median(vary, na.rm=TRUE))
            else quote(mean(vary, na.rm=TRUE))
  
  
  ctr_cnst <- if (center == "mean" & variability =="mean_range") quote(A2(n))
              else if (center == "mean" & variability == "moving_range") quote(A2(1))
              else if (center == "mean" & variability == "median_range") quote(A4(n))
              else if (center == "mean" & variability == "rms") quote(A1(n))
              else if (center == "mean" & variability == "sd") quote(A3(n))
              else if (center == "median" & variability == "mean_range") quote(A6(n))
              else if (center == "median" & variability == "median_range") quote(A9(n))
  
  var_lwr <- if (variability == "mean_range") quote(D3(n))
             else if (variability == "median_range") quote(D5(n))
             else if (variability %in% c("rms", "sd")) quote(B3(n))
             else if (variability == "moving_range") quote(0)
  
  var_upr <- if (variability %in% c("mean_range", "moving_range")) quote(D4(n))
             else if (variability == "median_range") quote(D6(n))
             else if (variability %in% c("rms", "sd")) quote(B4(n))
  
  #* Hack my way round the global binding warning
  average <- vary <- measure <- n <- center_x <- center_v <- element <- 
    lcl <- ucl <- value.y <- value.x <- element_class <- NULL
  
  controlData <- 
    dplyr::tbl_df(data.frame(id = data[, id],
                            value = data[, value],
                            stringsAsFactors=FALSE)) %>%
    dplyr::mutate(mr = movingRange(value)) %>%
    dplyr::group_by(id) %>%    
    dplyr::summarise_(average = ~eval(ctr_fn),
                      vary = ~eval(var_fn),
                      n = ~sum(!is.na(value))) %>%
    dplyr::mutate(center_x = mean(average),
                  center_v = if (grepl("median", variability)) median(vary, na.rm=TRUE)
                             else mean(vary, na.rm=TRUE)) %>%
    tidyr::gather(measure, value, 
                  average, vary,  
                  -id, -n, -center_x, -center_v) %>%
    dplyr::group_by(measure) %>%
    dplyr::mutate(center = mean(value),
                  lcl = ifelse(test = measure == "average", 
                               yes = mean(center_x) - eval(ctr_cnst) * mean(center_v),
                               no = eval(var_lwr) * center_v),
                  ucl = ifelse(test = measure == "average",
                               yes = mean(center_x) + eval(ctr_cnst) * mean(center_v),
                               no = eval(var_upr) * mean(center_v))) %>%
    tidyr::gather(element, value,
                  value, center, lcl, ucl) %>%
    dplyr::mutate(element_class = factor(ifelse(test = element %in% c("lcl", "ucl"), 
                                                yes = "control_limit", 
                                                no = as.character(element)),
                                         c("value", "center", "control_limit")))
  
  if (variability == "moving_range")
  {
    controlData <- controlData %>%
      dplyr::rename(cc_id=id) %>%
      dplyr::full_join(tbl_df(data.frame(cc_id = rep(1, length(data[, id])),
                                         id = 1:length(data[, id]),
                                         value = data[, value],
                                         stringsAsFactors=FALSE)),
                        by=c("cc_id" = "cc_id")) %>%
      dplyr::group_by(measure, element) %>%
      dplyr::mutate(value = ifelse(test = measure == "average" & element == "value",
                                   yes = value.y,
                                   no = ifelse(test = measure == "vary" & element == "value",
                                               yes = movingRange(value.y),
                                               no = value.x)),
                     n = 1)
  }
  
  controlData %>%
    dplyr::select(id, measure, element, element_class, value)
}


