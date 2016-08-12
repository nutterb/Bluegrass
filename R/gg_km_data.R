#' @name gg_km_data
#' @title Construct Data for Use in Kaplan-Meier Plots
#' 
#' @description Generates data sets suitable for use in \code{ggplot} style 
#'   Kaplan-Meier plots
#'   
#' @param object Either a \code{survfit} or \code{summary.survfit} object. The 
#'   \code{survfit} object is used to build the base plot.  The 
#'   \code{summary.survfit} object is used for formatting the x-axis, N at risk,
#'   N events, and confidence bars.
#' @param offset Offset values to be used for confidence bars.
#' @param ... Additional arguments to pass to other methods.
#' 
#' @details The \code{survfit} method forces the data in each group to start at time 0 (unless
#'   a negative time exists)
#'   
#' The \code{summary.survfit} method only returns times requested by the user (or the default 
#'   times from the \code{survfit} object).
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accept either \code{survfit} or \code{summary.survfit} objects
#'   \item Return a tidy dataframe suitable for use in \code{ggplot} style grpahics
#'   \item Force the times in \code{survfit} objects to begin at time 0, unless a negative
#'         time is found in the object as received
#'   \item Remove the \code{"x="} prefix from the \code{strata} column.
#' }
#' 
#' @author Benjamin Nutter
#' @export

gg_km_data <- function(object, ...)
{
  UseMethod("gg_km_data")
}

#' @rdname gg_km_data
#' @export

gg_km_data.survfit <- function(object, ...)
{
  km_data <- broom::tidy(object) 
  
  split(km_data, km_data$strata) %>%
    lapply(
      function(km)
      {
        if (km$time[1] > 0)
        {
          dplyr::bind_rows(
            data.frame(time = 0, 
                       n.risk = km$n.risk[1],
                       n.event = 0,
                       n.censor = 0,
                       estimate = 1.0,
                       std.error = 0,
                       conf.high = 1.0,
                       conf.low = 1.0,
                       strata = km$strata[1],
                       stringsAsFactors = FALSE),
            km
          )
        }
        else
        {
          km
        }
      }
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(strata) %>%
    dplyr::mutate(time.next = dplyr::lead(time)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(strata = gsub("^x[=]", "", strata))
}

#' @rdname gg_km_data
#' @export

gg_km_data.summary.survfit <- function(object, offset = NULL, ...)
{
  km_data <- 
    tidy.summary.survfit(object, 
                         detail = TRUE)
  
  if (is.null(offset))
  {
    offset <- rep(0, length(unique(km_data[["strata"]])))
  }
  
  split(km_data, km_data$strata) %>%
    mapply(
      function(x, off) 
      {
        x$time_offset <- x$time + off
        x
      },
      .,
      offset,
      SIMPLIFY = FALSE
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(strata = gsub("^x[=]", "", strata))
    
}

utils::globalVariables(c("strata", "time"))