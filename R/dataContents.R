#' @name dataContents
#' @export dataContents
#' @importFrom lazyWeave univ
#' @importFrom lazyWeave lazy.matrix
#' @importFrom Hmisc label.default
#' @importFrom Hmisc label.data.frame
#' @importFrom Hmisc 'label<-.default'
#' @importFrom Hmisc 'label<-.data.frame'
#' @importFrom Hmisc print.labelled
#' @importFrom Hmisc '[.labelled'
#' 
#' @title Summarize Data
#' @description Prepare a set of tables to describe a data set.  Five tables 
#'   are available giving an overview of the variables and summaries of numeric,
#'   categorical, date, and other variables (usually character).
#'   
#' @param data A data frame to be summarized
#' @param round The number of significant digits to which numerical values 
#'   will be rounded in the summaries.
#' @param report Logical.  Indicates if the results are printed to a report
#'   (using \code{lazyWeave}) or returned to the environment.
#' @param print_tabs A character vector giving the tables to be returned.
#'   Partial matches are accepted.
#' @param overview_args list of additional arguments to be passed to 
#'   \code{lazy.matrix} for the overview table.
#' @param numeric_args list of additional arguments to be passed to 
#'   \code{lazy.matrix} for the numeric summary table.
#' @param categorical_args list of additional arguments to be passed to 
#'   \code{lazy.matrix} for the categorical summary table.
#' @param date_args list of additional arguments to be passed to 
#'   \code{lazy.matrix} for the date summary table.
#' @param other_args list of additional arguments to be passed to 
#'   \code{lazy.matrix} for the other variables summary table.
#' @param ... Arguments to be passed to other methods. Currently ignored.
#' 
#' @details The overview summary table is an approximation of the SAS PROC 
#' CONTENTS output.  
#' 
#' The numeric summary is generated using the \code{univ} function from the
#' \code{lazyWeave} package.
#' 
#' @return
#' The columns of the overview table include:
#' \enumerate{
#'   \item Variable The variable name
#'   \item Label The variable label
#'   \item Type The variable class
#'   \item Missing The number of missing values
#'   \item Prop.Miss The proportion of missing values.
#' }
#' 
#' The columns of the numeric summary table include:
#' \enumerate{
#'   \item Factor The variable label
#'   \item N The number of non-missing values
#'   \item Missing The number of missing values
#'   \item Mean The variable mean
#'   \item SD The variable Standard Deviation
#'   \item LCL The lower 95% confidence limit for the mean
#'   \item UCL The upper 95% confidence limit for the mean
#'   \item Min The variable minimum
#'   \item P25 The variable 25th percentile
#'   \item MEDIAN The variable median
#'   \item P75 The variable 75th percentile
#'   \item MAX the variable maximum
#'   \item CV The variable coefficient of variation
#' }
#' 
#' The columns of the categorical summary table include:
#' \enumerate{
#'   \item Variable Variable label
#'   \item Level The level of the variable
#'   \item Missing The number of missing values
#'   \item Freq The frequency of the level
#'   \item Rel.Freq The relative frequency of the level
#'   \item Cum.Freq The cumulative frequency for the variable
#'   \item Rel.Cum.Freq The cumulative relative frequency for the table.
#' }  
#' 
#' The columns of the date summary table include:
#' \enumerate{
#'   \item Variable The variable label
#'   \item Missing The number of missing values
#'   \item Min The minimum date
#'   \item Max The maximum date
#' } 
#'   
#' The columns of the other variables summary table include:
#' \enumerate{
#'   \item Label The variable label
#'   \item Missing The number of missing values
#'   \item Type The variable class
#' }
#' 
#' @author Benjamin Nutter



dataContents <- function(data, round=2, report=TRUE,
        print_tabs = c("overview", "numeric", "categorical", "date", "other"),
        overview_args=list(align=c("left", "left", "left", "center", "center"), 
                           caption="Variable Overview"), 
        numeric_args=list(align = c("left", rep("center", 12)), 
                          caption="Summary of Numerical Variables"),
        categorical_args=list(align=c("left", "left", rep("center", 5)), 
                              caption="Summary of Categorical Variables"), 
        date_args=list(align=c("left", rep("center", 3)), 
                       caption="Summary of Date Variables"),
        other_args=list(align=c("left", "center", "center"),
                        caption="Summary of Other Variables"), ...){
  
  print_tabs <- match.arg(print_tabs, 
                          c("overview", "numeric", "categorical", "date", "other"),
                          several.ok = TRUE)
  
  #****************************************************************
  #* Overview of data frame
  #* 1. get.type subroutine
  #* 2. get.miss subroutine (gets number of missing observations)
  #* 3. get variable name, label, type, and number of missing
  #* 4. Combine info to a data frame.
  #****************************************************************
  
  if ("overview" %in% print_tabs){
    #*** 1. get.type subroutine
    get.type <- function(x){
      if(is.numeric(x)) type <- "numeric"
      else if(is.factor(x)) type <- "factor"
      else if("Date" %in% class(x) | "POSIXct" %in% class(x) | "POSIXlt" %in% class(x)) type <- "date"
      else if(is.character(x)) type <- "character"
      else type <- "other"
      return(type)
    }
  
    #*** 2. get.miss subroutine (gets number of missing observations)
    get.miss <- function(x) sum(is.na(x))
  
    #*** 3. get variable name, label, type, and number of missing
    var <- names(data)
    lab <- Hmisc::label(data, default=names(data))[var]
    type <- sapply(data, get.type)
    miss <- sapply(data,get.miss)
    p.miss <- miss/nrow(data)
  
    #*** 4. Combine info to a data frame.
    overview <- data.frame(Variable=var, Label=lab, Type=type,
                           Missing=miss, Prop.Miss=p.miss,
                           stringsAsFactors=FALSE)
    rownames(overview) <- NULL
  
    overview[, sapply(overview, is.numeric)] <- lapply(overview[, sapply(overview, is.numeric)], round, round)
  }
  else overview <- NULL
  
  #****************************************************************
  #* Summary of Numeric Variables
  #* 1. Identifty numeric variables
  #* 2. Extract numeric variables from data and get descriptives
  #* 3. Prepare data frame for presentation
  #****************************************************************
  
  if ("numeric" %in% print_tabs){
    #*** 1. Identifty numeric variables
    num.n <- subset(overview, overview$Type=="numeric")$Variable
  
    #*** 2. Extract numeric variables from data and get descriptives
    if(length(num.n) > 0){
      num <- data.frame(data[, num.n])
      names(num) <- num.n
      numeric <- lazyWeave::univ(num, names(num))
    
      #*** 3. Prepare data frame for presentation
      numeric[,!names(numeric) %in% c("Factor", "Group", "N", "MISSING")] <- 
        lapply(numeric[,!names(numeric) %in% 
                         c("Factor", "Group", "N", "MISSING")], round, round)
      names(numeric)[1] <- "Variable"
      numeric <- numeric[, -2]
      #numeric[is.na(numeric)] <- ""
      numeric <- numeric[, !names(numeric) %in% "PVAL"]
      rownames(numeric) <- NULL
    }
    else numeric <- NULL
  }
  else numeric <- NULL
  
  #****************************************************************
  #* Summary of Categorical Variables
  #* 1. Categorical table subroutine
  #* 2. Identify factor variables
  #* 3. Extract factor variables and get descriptives
  #****************************************************************
  
  if ("categorical" %in% print_tabs){
    #*** 1. Categorical table subroutine
    cat.tab <- function(x){
      if (nlevels(data[, x]) > 0){
        Factor <- c(Hmisc::label(data[, x, drop=FALSE], 
                                 default=names(data[, x, drop=FALSE])), 
                    rep("", nlevels(data[, x]) - 1))
        Level <- levels(data[, x])
        Freq <- as.vector(table(data[, x]))
        Rel.Freq <- prop.table(Freq)
        Miss <- c(sum(is.na(data[, x])), rep("", nlevels(data[, x]) - 1))
        categ <- data.frame(Variable = Factor, Level = Level, 
                            Missing = Miss,
                            Freq = Freq, Rel.Freq = Rel.Freq,
                            Cum.Freq = cumsum(Freq), Cum.Rel.Freq = cumsum(Rel.Freq))
      
        return(categ)
      }
      else{
        categ <- data.frame(Variable = Hmisc::label(data[, x, drop=FALSE], 
                                                    default=names(data[, x, drop=FALSE])), 
                            Level = NA, Missing = NA,
                            Freq = NA, Rel.Freq = NA,
                            Cum.Freq = NA, Cum.Rel.Freq = NA)
        return(categ)
      }
    }

  
    #*** 2. Identify factor variables
    cat.n <- subset(overview, overview$Type=="factor")$Variable
    
    #*** 3. Extract factor variables and get descriptives
    if(length(cat.n) > 0){
      cat <- data.frame(data[,cat.n])
      names(cat) <- cat.n
    
      freq <- do.call("rbind",lapply(names(cat),cat.tab))
      rownames(freq) <- NULL
    
      freq[, sapply(freq, is.numeric)] <- lapply(freq[, sapply(freq, is.numeric)], round, round)
    }
    else freq <- NULL
  }
  else freq <- NULL
  
  #****************************************************************
  #* Summary of Date Variables
  #* 1. Subroutines for min and max date
  #* 2. Identify date variables
  #* 3. Extract date variables and get descriptives
  #* 4. Build data frame
  #****************************************************************
  
  if ("date" %in% print_tabs){
    #*** 1. Subroutines for min and max date
    get.min.date <- function(d) as.character(min(d,na.rm=TRUE))
    get.max.date <- function(d) as.character(max(d,na.rm=TRUE))
  
    #*** 2. Identify date variables
    dat.n <- subset(overview, overview$Type=="date")$Variable
  
    #*** 3. Extract date variables and get descriptives
    if(length(dat.n) > 0){
      dat <- as.data.frame(data[,dat.n])
      names(dat) <- dat.n
    
      min.date <- as.Date(sapply(dat,get.min.date),format="%Y-%m-%d")
      max.date <- as.Date(sapply(dat,get.max.date),format="%Y-%m-%d")
      miss <- sapply(dat, function(x) sum(is.na(x)))
    
      #*** 4. Build data frame
      date <- data.frame(Variable=Hmisc::label(data[, dat.n], default=dat.n), 
                         Missing = miss,
                         Min=min.date, Max=max.date,
                         stringsAsFactors=FALSE)
      rownames(date) <- NULL
    }
    else date <- NULL
  }
  else date <- NULL
  
  #****************************************************************
  #* List of Category and other types
  #****************************************************************
  
  if ("other" %in% print_tabs){
    oth <- which(overview$Type=="character" | overview$Type=="other")
    if(length(oth) > 0){ 
      other <- overview[oth,c("Label", "Missing", "Type")]
      rownames(other) <- NULL
    }
    else other <- NULL
  }
  else other <- NULL
  
  #****************************************************************
  #* Return results
  #****************************************************************
  
  #* If not being written to a report, return a list of data frames
  if (!report){
    return(list(overview, numeric, freq, date, other))
  }
  
  #* Return the printed overview table
  out <- ""
  if ("overview" %in% print_tabs){
    out <- if (is.null(overview)) out
           else paste(out, do.call("lazy.matrix", c(list(x=overview),
                                                    overview_args)),
                      sep="\n\n")
  }
  
  #* Return the printed numeric table
  if ("numeric" %in% print_tabs){
    out <- if (is.null(numeric)) out
    else paste(out, do.call("lazy.matrix", c(list(x=numeric),
                                             numeric_args)),
               sep="\n\n")
  }
  
  #* Return the printed categorical table
  if ("categorical" %in% print_tabs){
    out <- if (is.null(freq)) out
    else paste(out, do.call("lazy.matrix", c(list(x=freq),
                                             categorical_args)),
               sep="\n\n")
  }
  
  #* Return the printed dates table
  if ("date" %in% print_tabs){
    out <- if (is.null(date)) out
    else paste(out, do.call("lazy.matrix", c(list(x=date),
                                             date_args)),
               sep="\n\n")
  }
  
  #* Return the printed other variables table
  if ("other" %in% print_tabs){
    out <- if (is.null(other)) out
    else paste(out, do.call("lazy.matrix", c(list(x=other),
                                             other_args)),
               sep="\n\n")
  }
  
  return(out)
}