#' @name ggSurvGraph
#' @export ggSurvGraph
#' @import ggplot2
#' @importFrom grid unit
#' @importFrom gridExtra grid.arrange
#' @importFrom plyr arrange
#' @importFrom plyr ddply
#' @importFrom reshape2 melt
#' @importFrom zoo na.locf
#' 
#' @title Kaplan-Meier Survival Graphs
#' @description Creates a graph of one or more survival curves with vertical 
#'   bars to represent confidence intervals. Uses the \code{ggplot} system 
#'   instead of base graphics.
#' 
#' @param object A fitted survival object, such as a survfit model or a data 
#'   frame containing the information for the graph. If a data frame, the 
#'   columns must be named time, surv, lower, upper, n.risk, n.event, group. 
#'   (see details for more information).
#' @param times Vector of times at which confidence bars should be drawn. 
#'   Defaults to all event times.
#' @param cum.inc If \code{TRUE}, a cumulative incidence plot is drawn instead 
#'   of a survival plot.
#' @param conf.bar If \code{TRUE}, vertical bars denoting the confidence 
#'   limits are drawn.
#' @param offset.scale the scale of time units to offset the bars from the 
#'   specified interval. For example, if a bar should be placed at 25 months, 
#'   and scale=.5, the bars will be placed at 24.5 and 25.5 months. If scale=1, 
#'   the bars will be placed at 24 months and 26 months. Additional groups are 
#'   placed further out on the same scale
#' @param n.risk A logical indicating whether the number of subjects at 
#'   risk should be printed at the bottom of the graph. Defaults to \code{FALSE} 
#'   for no printing.
#' @param n.event A logical indicating whether the cumulative number of events 
#'   should be printed at the bottom of the graph. Defaults to \code{FALSE} 
#'   for no printing.
#' @param gg_expr A list of expressions to be added to the initial \code{ggplot}
#'   command.
#' 
#' @details The function plots the Kaplan-Meier survival curves of the provided 
#'   object or data. It then places vertical bars the length of the confidence 
#'   interval at that point at the distances specified by span.
#'   
#'   \code{ggSurvGraph} operates my creating a data frame of values to be 
#'   plotted, even if a survfit object is passed to the function. Thus, 
#'   \code{SurvGraph} does not utilize the options in \code{plot.survfit.} 
#'   Those accustomed to plotting Kaplan-Meier curves with \code{plot.survfit} 
#'   will not necessarily get the same results with \code{ggSurvGraph}.
#'   
#'   When a data frame is passed to \code{ggSurvGraph} the columns of data must 
#'   have the named elements: time, surv, lower, upper, n.risk, n.event 
#'   [,strata], where
#' 
#'   When a data frame is passed to SurvGraph the columns of data must have the named elements:
#'   \code{time, surv, lower, upper, n.risk, n.event [,strata]}, where 
#'   \tabular{ll}{
#'     \code{time}  \tab denotes the survival time\cr
#'     \code{surv}  \tab denotes the probability of survival at \code{time}\cr
#'     \code{lower} \tab denotes the lower confidence limit for \code{surv}\cr
#'     \code{upper} \tab denotes the upper confidence limit for \code{surv}\cr
#'     \code{n.risk}\tab denotes the number at risk at \code{time}\cr
#'     \code{n.event}\tab denotes the number of events occuring between \code{time} 
#'       and the previous \code{time}.\cr
#'     \code{strata}\tab is an optional variable that allows for simultaneous plotting of stratified
#'       survival curves.  Each observation in the data set should have a group 
#'       associated with it if \code{group} is supplied.\cr
#'   }
#'   
#'   \code{SurvGraph} is not capable of handling cox models.
#'   
#' @section Internal Data:
#' Additional layers may be added to the plot using the \code{gg_expr} 
#' argument.  The layers should use the data produced internal to the function.
#' There are two data sets created.  \code{survRaw} is generated from the 
#' output of \code{summary(object)} without a \code{times} argument.  The
#' Kaplan-Meier curve is based on this data frame as it contains the information
#' for the full curve.  A second data frame, called \code{survData}, is generated
#' from the output of \code{summary(object, times=[times])} and is used to 
#' draw the confidence bars.  
#' 
#' Contents of \code{survRaw}
#' \itemize{
#'   \item{time} The time elapsed since the index time
#'   \item{n.risk} The number at risk at time
#'   \item{n.event} The number of events between time[t-1] and time[t].
#'   \item{n.censor} The number of censored observations between time[t-1] and time[t]
#'   \item{surv} The survival proportion at time[t]
#'   \item{lower} The lower confidence limit of survival at time[t]
#'   \item{upper} The upper confidence limit of survival at time[t]
#'   \item{strata} Group assignment
#'   \item{cum.evt} The cumulative events at time[t]
#'   \item{next.time} The next observed time in the object.  This is useful for 
#'     plotting confidence bands.
#' }
#' 
#' The contents \code{survData} are identical, except it lacks the \code{next.time}
#' variable.
#'  
#' @author Benjamin Nutter
#' @examples
#' \dontrun{ 
#'   library(survival)
#'   fit <- survfit(Surv(time, status) ~ x, data=aml)
#'   ggSurvGraph(fit)
#'   ggSurvGraph(fit, offset.scale=2, n.risk=TRUE) 
#'   
#'   #* changing the linetype:
#'   #* Because the graph is based on the output from the survfit object,
#'   #* the aesthetics passed to the plot must be based on the variable names
#'   #* from that object, not the original data frame.  The linetype and 
#'   #* colour will be based on the 'strata'.
#'   ggSurvGraph(fit, n.risk=TRUE,
#'               gg_expr=list(aes(linetype=strata)))
#'               
#'   #* Confidence Bands can be obtained using geom_rect()
#'   ggSurvGraph(fit, conf.bar=FALSE, times=seq(0, 60, by=12),
#'               gg_expr=list(geom_rect(aes(xmin=time, xmax=next.time,
#'                                          ymin=lower, ymax=upper,
#'                                          fill=strata),
#'                                      alpha=.25, linetype=0)))
#' }
#' 

ggSurvGraph <- function(object, times, cum.inc=FALSE, conf.bar=TRUE,
                        offset.scale=1, n.risk=FALSE, n.event=FALSE,
                        gg_expr){
  requireNamespace("survival")
  
  #* Determine if the user provided axis break points in gg_expr
  #* If not, we will define them based on the times 
  userAxis <- any(grepl("scale_x_continuous", as.character(substitute(gg_expr))))
  
  #**************************************************************
  #*** Parameter checking
  error.count <- 0
  error.msg <- NULL
  
  #*** 'object' should be either a 'survfit' object or a 'data.frame'
  if (!(any(class(object) %in% c("survfit","data.frame")))){
    error.count <- error.count + 1
    error.msg <- c(error.msg, paste0(error.count, ": \'object\' must be either a survfit object or a data frame", sep=""))
  }
  
  #*** When 'object' is a data frame, it must have the columns in 'req.col'
  #*** This is a feature that was added so that we could make survival graphs with PROC LIFETEST output
  req.col <- c("time","surv","lower","upper","n.risk","n.event")
  if ("data.frame" %in% class(object) && !any(req.col %in% names(object))){
    miss.col <- paste0("\'", req.col[!req.col %in% names(object)], "\'", sep="", collapse=", ")
    error.count <- error.count + 1
    error.msg <- c(error.msg, paste0(error.count, ": data frame \'object\' is missing columns ", miss.col, sep=""))
  }
  
  #*** Stop the function if any parameter checks failed
  if (error.count){
    stop(paste(error.msg, collapse="\n"))
  }
  
  #** CMD Check preparations
  #* The following assignments are made to avoid the R CMD Check NOTE
  #* no visible binding for global variable ......
  #* These notes occur because surv, lower, upper, and variable are called
  #* within transform(), melt(), and ddply() where there is a data argument
  #* that R CMD check can't reconcile with the variables.
  
  surv <- lower <- upper <- variable <- n.censor <- NULL
  
  #********************************************************************
  #*** Prepare the data for plotting
  
  rawTimes <- sort(unique(object$time))
  
  if (missing(times)){
    times <- sort(unique(object$time))
    if (!0 %in% times) times <- c(0, times)
  }
  
  if ("survfit" %in% class(object)){ 
    survRaw <- summary(object, times=rawTimes[rawTimes <= max(times)])
    survData <- summary(object, times=times)
  }
  
  if (is.null(survRaw$strata)) survRaw$strata <- factor(1)
  if (cum.inc){
    survRaw$surv <- 1-survRaw$surv
    survRaw$lower <- 1-survRaw$lower
    survRaw$upper <- 1-survRaw$upper
  }
  
  if (is.null(survData$strata)) survData$strata <- factor(1)
  if (cum.inc){ 
    survData$surv = 1-survData$surv
    survData$lower = 1-survData$lower
    survData$upper = 1-survData$upper
  }
  
  #*** Generate offset values
  if(nlevels(survData$strata)>1){
    offset <- seq.int(-1*ceiling(nlevels(survData$strata)/2),ceiling(nlevels(survData$strata)/2),length.out=nlevels(survData$strata)+1)
    offset <- offset[offset!=0]
    offset <- offset[order(abs(offset))] * offset.scale
    offset <- offset[1:nlevels(survData$strata)]
  }
  else offset <- 0
  
  offset <- data.frame(strata = levels(survData$strata), offset = offset)
  
  survRaw <- as.data.frame(lapply(survRaw[c("time", "n.risk", "n.event", "n.censor", 
                                            "surv", "lower", "upper", "strata")], 
                                  identity))
  
  survRaw <- plyr::ddply(survRaw,
                         "strata",
                         transform,
                         time = if (0 %in% time) time else c(0, time),
                         n.risk = if (0 %in% time) n.risk else c(n.risk[1], n.risk),
                         n.event = if (0 %in% time) n.event else c(n.event[1], n.event),
                         n.censor = if (0 %in% time) n.censor else c(n.censor[1], n.censor),
                         surv = if (0 %in% time) surv else c(surv[1], surv),
                         lower = if (0 %in% time) lower else c(lower[1], lower),
                         upper = if (0 %in% time) upper else c(upper[1], upper))
  
  survRaw <- plyr::ddply(survRaw,
                          "strata",
                          plyr::arrange,
                          time)
  
  survRaw <- plyr::ddply(survRaw,
                         "strata",
                         transform,
                         cum.evt = cumsum(n.event),
                         next.time = c(time[-1], time[length(time)]))
  #   return(survRaw)
  levels(survRaw$strata) <- gsub("[[:print:]]+[=]", "", levels(survRaw$strata))
  
  
  
  survData <- as.data.frame(lapply(survData[c("time", "n.risk", "n.event", "n.censor", 
                                              "surv", "lower", "upper", "strata")], 
                                   identity))
  survData <- plyr::ddply(survData,
                          "strata",
                          transform,
                          cum.evt = cumsum(n.event))
  
  survData <- merge(survData, offset, by="strata")
  levels(survData$strata) <- gsub("[[:print:]]+[=]", "", levels(survData$strata))
  
  #*************************************************************
  #*** Create Plot
  
  #*** Add the scale_x_continous layer if not provided by the user
  if (!userAxis){
    if (missing(gg_expr)) gg_expr <- list(scale_x_continuous(breaks=times))
    else gg_expr <- c(gg_expr, list(scale_x_continuous(breaks=times)))
  }
  
  #*** Creates a blank plot for a spacer between survival plot and risk/event data
  blank.pic <- ggplot(survData, aes_string('time', 'surv')) +
    geom_blank() + theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(), panel.border = element_blank())
  
  
  #*** Create the survival plot
  if (nlevels(survData$strata) > 1){ 
    .plot <- ggplot(survRaw, aes_string(x='time', y='surv', colour='strata')) + geom_step() 
  }
  else{
    .plot <- ggplot(survRaw, aes_string(x='time', y='surv')) + geom_step()
  }
  
  #*** Add Confidence bars
  if (conf.bar){
    .plot <- .plot +  
      geom_segment(data=survData, aes_string(x='time + offset', 
                                             xend='time + offset', 
                                             y='lower', yend='upper'))
  }
  
  #*** Add additional layers
  if (!missing(gg_expr)) .plot <- .plot + gg_expr
  
  
  #*** Number at risk and events
  riskTable <- survData
  riskTable <- reshape2::melt(riskTable[, c("time", "strata", "n.risk", "cum.evt")],
                              c("time", "strata"))
  riskTable <- transform(riskTable,
                         y.pos = ifelse(variable %in% "n.risk", 1, 0))
  
  riskTable <- plyr::ddply(riskTable,
                           c("strata", "variable"),
                           function(d) merge(data.frame(time=times), d, by="time", all.x=TRUE))
  
  riskTable[, 2:5] <- lapply(riskTable[, 2:5],
                             zoo::na.locf, na.rm=FALSE)
  
  if (!n.risk) riskTable <- riskTable[!riskTable$variable %in% "n.risk", ]
  if (!n.event) riskTable <- riskTable[!riskTable$variable %in% "cum.evt", ]
  
  .risk <- ggplot(survData, aes_string(x='time', y='surv')) + 
    geom_text(data=riskTable, aes_string(x='time', y='rev(variable)', label='value'), size=3.5, hjust=0) + 
    theme_bw() + 
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(), panel.border = element_blank())  + 
    scale_y_discrete(labels=c("Total Events", "N at Risk")[c(n.event, n.risk)])
  
  #* Adjust the limits of the Risk Table to match the limits of the plot
  .lim <- ggplot_build(.plot)$panel$ranges[[1]]$x.range
  .lim_scalar <- .lim[2] - .lim[2]/1.05
  .lim <- .lim + c(1, -1) * .lim_scalar
  .risk <- .risk + xlim(.lim)
  
  if (nlevels(riskTable$strata) > 1) .risk <- .risk + facet_wrap(~ strata, ncol=1)
  
  #* Add risk table
  if (n.risk || n.event){
    gridExtra::grid.arrange(.plot + theme(plot.margin = grid::unit(c(1,1,0,.5), "lines"), legend.position="bottom"), 
                            blank.pic + theme(plot.margin = grid::unit(c(0,0,0,0), "lines")), 
                            .risk + theme(plot.margin = grid::unit(c(0,1,0,0), "lines")), 
                            clip = FALSE, nrow = 3,
                            ncol = 1, heights = grid::unit(c(.70, .04, .35),c("null", "null", "null"))) 
  } else print(.plot)
  
  
}
