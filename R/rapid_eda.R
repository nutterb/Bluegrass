#' @name rapid_eda
#' @title Summarize Numeric Variables 
#' 
#' @description Produce common numeric summary values for continuous 
#'   variables in a data frame.  The function only operates on 
#'   variables of class \code{numeric} and \code{integer}.
#'   
#' @param data A \code{data.frame}-like object.  Accepts objects of 
#'   class \code{data.frame}, \code{tbl_df}, \code{grouped_df}, 
#'   and \code{data.table}.
#' @param vars A character vector giving the names of columns to 
#'   summarize.  When \code{NULL}, all numeric variables will be 
#'   summarized.
#' @param group A character vector giving the names of columns by 
#'   which the summaries are to be grouped.  When \code{NULL}, 
#'   no grouping is performed.
#' @param quiet \code{logical(1)}. Should warnings be silenced.  See
#'   Details.
#'   
#' @details 
#'   If \code{data} is either a \code{data.frame} or \code{data.table}, 
#'   it is first converted to a \code{tbl_df}.  It is converted back 
#'   to it's original class when returning the output.
#' 
#'   \code{rapid_eda} only summarizes data for numeric variables.
#'   To prevent failures, any non-numeric variables provided for summary
#'   will be dropped from \code{data} prior to generating the summary.
#'   A warning is printed naming the columns dropped unless 
#'   \code{quite = FALSE}
#'   
#' @return Returns an object of the same class as \code{data}.  Summarized
#'   values include:
#' \itemize{
#'   \item{n }{Total non-missing values}
#'   \item{n_miss }{Total missing values}
#'   \item{mean }{Arithmetic mean}
#'   \item{sd }{Standard deviation}
#'   \item{min }{Minimum}
#'   \item{p25 }{Twenty-fifth percentile (first quartile)}
#'   \item{median }{Median}
#'   \item{p75 }{Seventy-fifth percentile (third quartile)}
#'   \item{max }{Maximum}
#' }
#'   
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accepts the following classes as input: \code{data.frame}, 
#'         \code{tbl_df}, \code{grouped_df}, \code{data.table}
#'   \item Accepts a vector of column names to select for the summary.
#'   \item Accepts a vector of column names to group by
#'   \item When given a \code{groupd_df}, the grouping variables are 
#'         maintained.  Any new variables given to group by in the argument
#'         are added.
#'   \item Includes an option to silence warnings
#'   \item Returns a warning naming any non-numeric variables dropped
#'         from the summary.
#'   \item Returns a warning naming an variables requested but not found
#'         in \code{data}
#'   \item Returns an object that is the same class as the object 
#'         given to the function.
#'   \item Output lists variables in the same order as \code{vars}.  When
#'         \code{vars = NULL}, the order is assumed to be the same order
#'         they are given in the data.
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' library(dplyr)
#' 
#' rapid_eda(mtcars)
#' 
#' rapid_eda(mtcars, 
#'           group = c("am", "gear"))
#' 
#' mtcars %>% 
#'   group_by(am, gear) %>%
#'   rapid_eda()
#' 
#' @export

rapid_eda <- function(data, vars = NULL, group = NULL, quiet = FALSE)
{
  coll <- checkmate::makeAssertCollection()
  
  #* Functional requirement 1
  checkmate::assert_class(
    x = data, 
    classes = "data.frame",
    add = coll
  )
  
  #* Functional requirement 2
  if (!is.null(vars))
  {
    checkmate::assert_character(
      x = vars,
      add = coll
    )
  }
  
  #* Functional requirement 3
  if (!is.null(group))
  {
    checkmate::assert_character(
      x = group,
      add = coll
    )
  }
  
  #* Funtional requirement 5
  checkmate::assert_logical(
    x = quiet,
    len = 1,
    add = coll
  )
  
  checkmate::reportAssertions(coll)
  
  orig_class <- class(data)
  
  if (class(data) == "data.frame" || inherits(data, "data.table"))
  {
    data <- tibble::as_tibble(data)
  }
  
  #* If 'vars' is NULL, set it to the names of 'data'
  if (is.null(vars)) 
  {
    vars <- names(data)
  }
  
  #* Functional requirement 7
  #* Find variables in `vars` not in `data`
  not_in_data <- 
   vars[!vars %in% names(data)]
  
  if (length(not_in_data))
  {
    vars <- vars[!vars %in% not_in_data]
    
    if (!quiet)
    {
      warning("The following columns were not found in ",
              "'data' and will not be displayed ",
              "in the summary: ",
              paste0(not_in_data, collapse = ", "))
    }
  }
  
  #* Functional requirement 6
  #* Find variables in `vars` that are not numeric
  not_numeric <-
    vars[!vapply(data[vars],
                 is.numeric,
                 logical(1))]
  
  if (length(not_numeric))
  {
    vars <- vars[!vars %in% not_numeric]
    
    if (!quiet)
    {
      warning("The following columns in 'data' are not numeric ",
              "and will not be displayed in the summary: ",
              paste0(not_in_data, collapse = ", "))
    }
  }
  
  #* Find variables in `group` that are not in `data`
  group_not_in_data <- 
    group[!group %in% names(data)]
  
  if (length(group_not_in_data))
  {
    group <- group[!group %in% group_not_in_data]
   
    if (!quiet)
    {
      warning("The following grouping columns were not found in ",
              "'data' and will not be used to group ",
              "the summary: ",
              paste0(group_not_in_data, collapse = ", "))
    }
  }
  
  #* Functional requirement 4
  #* Get the original grouping variables 
  #* Convert the grouped_df to a plain old tibble
  if ("grouped_df" %in% class(data))
  {
    orig_group <- 
      as.character(attributes(data)$vars)
    group <- c(orig_group, group)
    
    data <- dplyr::ungroup(data)
  }
  
  #* Find variables in both `vars` and `group`. Remove them from `vars`
  vars_in_group <- 
   vars[vars %in% group]
  
  if (length(vars_in_group))
  {
   vars <- vars[!vars %in% vars_in_group]
   
   if (!quiet)
   {
    warning("The following variables were also designated as ",
            "grouping variables.  They will not be used in the summary: ",
            paste0(vars_in_group, collapse = ", "))
   }
  }
  
  if (!is.null(group))
  {
    data %<>% 
      dplyr::group_by_(group)
  }

  data %<>%
    tidyr::gather_(key_col = "variable",
                   value_col = "value",
                   gather_cols = vars) %>%
    dplyr::group_by_(.dots = c("variable", group)) %>%
    dplyr::summarise(
      n      = sum(!is.na(value)),
      n_miss = sum(is.na(value)),
      mean   = mean(value, na.rm = TRUE),
      sd     = stats::sd(value, na.rm = TRUE),
      min    = min(value, na.rm = TRUE),
      p25    = stats::quantile(value, na.rm = TRUE, probs = 0.25),
      median = stats::median(value, na.rm = TRUE),
      p75    = stats::quantile(value, na.rm = TRUE, probs = 0.75),
      max    = max(value, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    #* Functional requirement 9
    #* reordering the data frame to the same order as `vars`
    dplyr::mutate(variable = factor(variable, vars, vars)) %>%
    dplyr::arrange_(.dots = c("variable", group)) %>%
    dplyr::mutate(variable = as.character(variable))
  
  
   #* Functional requirement 8
   if ("data.table" %in% orig_class)
   {
     data.table::as.data.table(data)
   }
   else if ("grouped_df" %in% orig_class)
   {
     dplyr::group_by_(data, .dots = orig_group)
   }
   else if ("tbl_df" %in% orig_class)
   {
     data
   }
   else
   {
     as.data.frame(data)
   }
          
}

utils::globalVariables(c("value", "variable"))