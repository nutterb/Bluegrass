#' @name data_contents
#' @export data_contents
#' 
#' @title Summarize Data
#' @description Prepare a set of tables to describe a data set.  Five tables 
#'   are available giving an overview of the variables and summaries of numeric,
#'   categorical, date, and other variables (usually character).
#'   
#' @param data A data frame to be summarized
#' 
#' @details The overview summary table is an approximation of the SAS PROC 
#' CONTENTS output.  
#' 
#' The numeric summary is generated using the \code{univ} function from the
#' \code{lazyWeave} package.
#' 
#' @return
#' The columns of the overview table include:
#' \itemize{
#'   \item Variable The variable name
#'   \item Label The variable label
#'   \item Type The variable class
#'   \item Missing The number of missing values
#'   \item Prop.Miss The proportion of missing values.
#' }
#' 
#' The columns of the numeric summary table include:
#' \itemize{
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
#' \itemize{
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
#' \itemize{
#'   \item Variable The variable label
#'   \item Missing The number of missing values
#'   \item Min The minimum date
#'   \item Max The maximum date
#' } 
#'   
#' The columns of the other variables summary table include:
#' \itemize{
#'   \item Label The variable label
#'   \item Missing The number of missing values
#'   \item Type The variable class
#' }
#' 
#' @author Benjamin Nutter

data_contents <- function(data)
{
  overview <- 
    data.frame(
      variable = names(data),
      label = Hmisc::label(data, self = FALSE),
      class = vapply(data,
                     function(x) paste0(class(x), collapse = ", "),
                     character(1)),
      missing = vapply(data,
                       function(x) sum(is.na(x)),
                       numeric(1)),
      prop_missing = vapply(data,
                            function(x) sum(is.na(x)) / length(x),
                            numeric(1)),
      stringsAsFactors = FALSE
    )

  numeric_variables <- 
    vapply(data,
           inherits,
           logical(1),
           what = c("numeric", "double", "integer"))
  
  numeric <- 
    if (sum(numeric_variables))
    {
      data[, numeric_variables, drop = FALSE] %>%
        tidyr::gather(variable, value) %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(n = length(value),
                         missing = sum(is.na(value)),
                         mean = mean(value, na.rm = TRUE),
                         sd = stats::sd(value, na.rm = TRUE),
                         min = min(value, na.rm = TRUE),
                         p25 = stats::quantile(value, probs = 0.25, na.rm = TRUE),
                         median = stats::median(value, na.rm = TRUE),
                         p75 = stats::quantile(value, probs = 0.75, na.rm = TRUE),
                         max = max(value, na.rm = TRUE),
                         cv = sd / mean) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(dplyr::select(overview, variable, label),
                         by = c("variable" = "variable")) %>%
        dplyr::select(variable, label, n:max)
    }
    else
    {
      NULL
    }

  
  categorical_variables <- 
    vapply(data,
           inherits,
           logical(1),
           what = c("factor"))
  
  categorical <- 
    if (sum(categorical_variables))
    {
      data[, categorical_variables, drop = FALSE] %>%
        tidyr::gather(variable, value) %>%
        dplyr::group_by(variable, value) %>%
        dplyr::summarise(
          missing = sum(is.na(value)),
          freq = sum(!is.na(value))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(variable) %>%
        dplyr::mutate(
          rel_freq = freq / sum(freq),
          cum_freq = cumsum(freq),
          cum_rel_freq = cumsum(rel_freq)
        ) %>%
        dplyr::ungroup()
    }
    else
    {
      NULL
    }
  
  date_variables <- 
    vapply(data,
           inherits,
           logical(1),
           what = c("Date", "POSIXct"))
  
  date <- 
    if (sum(date_variables))
    {
      date_classes <- 
        vapply(data[, date_variables, drop = FALSE], 
               function(x) class(x)[1],
               character(1)) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        stats::setNames("class") %>%
        dplyr::mutate(variable = names(data[, date_variables, drop = FALSE]))
      
      lapply(
        data[, date_variables, drop = FALSE],
        function(x) 
          data.frame(
            min = as.character(min(x, na.rm = TRUE)),
            max = as.character(max(x, na.rm = TRUE)),
            stringsAsFactors = FALSE
          )
      ) %>%
        dplyr::bind_rows() %>%
        dplyr::bind_cols(date_classes) %>%
        dplyr::select(variable, class, min, max)
    }
    else
    {
      NULL
    }
  
  other_variables <- 
    !vapply(data,
            inherits,
            logical(1),
            what = c("numeric", "double", "integer",  
                     "factor", "Date", "POSIXct"))
  
  other <- 
    if (sum(other_variables))
    {
      other_classes <- 
        vapply(data[, other_variables, drop = FALSE], 
               function(x) paste0(class(x), collapse = ", "),
               character(1)) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        stats::setNames("class") %>%
        dplyr::mutate(variable = names(data[, other_variables, drop = FALSE]))
      
      data[, other_variables, drop = FALSE] %>%
        tidyr::gather(variable, value) %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(missing = sum(is.na(value))) %>%
        dplyr::left_join(other_classes,
                         by = c("variable" = "variable"))
    }
  else
  {
    NULL
  }
    
  
  list(overview = overview, 
       numeric = numeric,
       categorical = categorical,
       date = date,
       other = other)
}

utils::globalVariables(c("variable", "value", "sd",
                         "label", "n", "freq", "rel_freq"))
