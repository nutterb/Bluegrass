#' @name gg_km
#' @title Kaplan-Meier Survival Plots using \code{ggplot}
#' 
#' @description Kaplan-Meier plot for survival in a time-to-event analysis with
#'   multiple display options.
#'   
#' @param object An object of class \code{survfit}, \code{coxph}, or \code{cph}. 
#'     \code{coxph} and \code{cph} objects are coerced to \code{survfit} objects.
#' @param times A numeric vector of time at which ticks on the x-axis should be drawn.
#'     These times are also used for determining where to draw confidence bars, 
#'     number of subjects and risk, and number of cumulative events, when any of 
#'     these are chosen.
#' @param conf_type A character value denoting the method to use for displaying confidence
#'     limts.  See Details.
#' @param cum_inc \code{logical(1)} indicating if the plot should be drawn as a 
#'     cumulative incidence plot.  Cumulative incidence is calcualated as 1 - survival.
#' @param plot_censor \code{logical(1)} indicating if times at which censored observations 
#'     are recorded should be plotted.
#' @param n_risk \code{logical(1)} indicating if the number at risk should be displayed
#'     below the plot.
#' @param n_event \code{logical(1)} indicating if the cumulative number of events should 
#'     be displayed below the plot.
#' @param region_alpha \code{numeric(1)} giving the alpha (transparency) value for confidence
#'     regions.
#' @param bar_offset \code{numeric(1)}. Specifies the units by which confidence bars are 
#'     separated along the x-axis. Defaults to \code{1}.
#' @param band_linetype May be either an integer or character of length 1 specifying the line
#'     type to be used in confidence bands.
#' @param censor_shape May be either an integer or character of length 1 specifying the 
#'     plotting character to be used for censored observations.
#' @param gg_expr A list of layers to add to the primary plot.  See Details.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#'
#' @details 
#' Confidence limits may be omitted (the default) or drawn as regions (shaded regions extending 
#'     from the lower to the upper confidence limits); bars (vertical bars extending from 
#'     the lower to the upper confidence limits at \code{times}; or bands (step function lines
#'     drawn through the lower and the upper confidence limits, as in \code{stats::plot.survfit}).
#'     
#' When \code{n_risk} or \code{n_event} is \code{TRUE}, a plot is returned through 
#' \code{gridExtra::gridArrange}.  When this occurs, the plot object is not directly 
#' modifiable.  Any additional layers that need to be added to the graph must be passed as 
#' a list to \code{gg_expr}.
#'         
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accept \code{survfit}, \code{coxph}, and \code{cph} objects.
#'   \item Allow user to specify the times displayed on the x-axis.  If no times are
#'         selected, use the default times from the \code{survfit} object.
#'   \item Allow the user to select no confidence limits, confidence regions, confidence
#'         bars, or confidence bands.
#'   \item Display confidence bars only at the times requested by the user
#'   \item Allow user control over the offset of the confidence bars
#'   \item Include an option to display as a cumulative incidence plot
#'   \item Include an option to display number of subjects at risk. Only display for 
#'         times requested by the user.
#'   \item Include an option to display cumulative events. Only display for 
#'         times requested by the user.
#'   \item Provide a list argument that may be used to modify the content of the plot
#'         when a simple \code{ggplot} object cannot be returned.
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @export

gg_km <- function(object, times = NULL, ...)
{
  UseMethod("gg_km")
}

#' @rdname gg_km
#' @export

gg_km.survfit <- function(object, times = NULL, 
                          conf_type = c("none", "region", "bar", "band"),
                          cum_inc = FALSE, plot_censor = FALSE,
                          n_risk = FALSE, n_event = FALSE,
                          region_alpha = 0.5,
                          bar_offset = 1,
                          band_linetype = 2,
                          censor_shape = 4,
                          gg_expr = list(), ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = object,
                          classes = "survfit",
                          add = coll)
  
  if (!is.null(times))
  {
    checkmate::assert_numeric(x = times,
                              add = coll)
  }
  
  conf_type <- 
    checkmate::matchArg(x = conf_type,
                        choices = c("none", "region", "bar", "band"),
                        add = coll)
  
  checkmate::assert_logical(x = cum_inc,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = n_risk,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = n_event,
                            len = 1,
                            add = coll)
  
  checkmate::assert_numeric(x = region_alpha,
                            lower = 0,
                            upper = 1,
                            len = 1,
                            add = coll)
  
  checkmate::assert_numeric(x = bar_offset,
                            len = 1,
                            add = coll)
  
  if (!checkmate::test_integerish(x = band_linetype,
                                  len = 1) & 
      !checkmate::test_character(x = band_linetype,
                                 len = 1))
  {
    coll$push("'band_linetype' must be either an integer-like value or a character and have length 1")
  }
    
  if (!checkmate::test_integerish(x = censor_shape,
                                  len = 1) & 
      !checkmate::test_character(x = censor_shape,
                                 len = 1))
  {
    coll$push("'censor_shape' must be either an integer-like value or a character and have length 1")
  }
  
  checkmate::assert_class(x = gg_expr,
                          classes = "list",
                          add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (is.null(times)) 
  {
    times <- object$time %>% unique %>% sort
  }
  
  km_data <- gg_km_data(object)
  
  offset <- seq_along(unique(km_data[["strata"]]))
  offset <- (offset - mean(offset)) * bar_offset
  
  km_data_time <- 
    summary(object,
            times = times) %>%
    gg_km_data(offset = offset)
  
  if (cum_inc)
  {
    km_data[c("estimate", "conf.low", "conf.high")] <- 
      lapply(km_data[c("estimate", "conf.low", "conf.high")],
             function(x) 1 - x)
    
    km_data_time[c("estimate", "conf.low", "conf.high")] <- 
      lapply(km_data_time[c("estimate", "conf.low", "conf.high")],
             function(x) 1 - x)
  }
    
  
  km_plot <- 
    ggplot2::ggplot(data = km_data,
                    mapping = ggplot2::aes(x = time,
                                           y = estimate,
                                           colour = strata,
                                           fill = strata)) + 
    ggplot2::geom_step() + 
    ggplot2::scale_x_continuous(breaks = km_data_time[["time"]])
  
  km_plot <-
    km_plot + 
      switch(
        conf_type,
        "region" = 
          ggplot2::geom_rect(
            mapping = ggplot2::aes(xmin = time,
                                   xmax = time.next, 
                                   ymin = conf.low,
                                   ymax = conf.high),
            colour = 0,
            alpha = region_alpha
          ),
        
        "bar" = 
          ggplot2::geom_segment(
            data = km_data_time,
            mapping = ggplot2::aes(x = time_offset,
                                   xend = time_offset,
                                   y = conf.low,
                                   yend = conf.high)
          ),
          
        "band" = 
          list(
            ggplot2::geom_step(
              mapping = ggplot2::aes(y = conf.high),
              linetype = band_linetype
            ),
            ggplot2::geom_step(
              mapping = ggplot2::aes(y = conf.low),
              linetype = band_linetype
            )
          ),
        NULL
      )
  
  if (plot_censor)
  {
    km_plot <- 
      km_plot + 
      ggplot2::geom_point(
        data = dplyr::filter(km_data, n.censor > 0),
        shape = censor_shape
      )
  }
  
  km_plot 
  
  
  
}
         

utils::globalVariables(c("time", "estimate", "strata", "time.next",
                         "conf.low", "conf.high", "time_offset", "n.censor"))
