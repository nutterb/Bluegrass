#' @name univariable_cat 
#' @title Univariable summary of a categorical variable.
#' 
#' @description 
#' 
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accept a vector of any type and treat it like categorical data
#'   \item Optionally accept a vector of any type the same length as 
#'     the first vector and treat it like categorical data
#'   \item Throw a warning when the grouping variable is not a character/factor
#'     or when there are more then 5 distinct categories.
#'   \item Accept a function name to be used for a univariable test.
#'   \item Accept additional arguments to the univariable test
#' }
#' 
#' @export

univariable_cat <- function(x, by = NULL, test = chisq.test, ..., 
                            ncat_warn = TRUE)
{
  name_x <- deparse(substitute(x))
  name_by <- deparse(substitute(by))
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_atomic_vector(x = x,
                                  add = coll)
  
  by_is_null <- is.null(by)
  
  if (!is.null(by))
  {
    checkmate::assert_atomic_vector(x = by,
                                    len = length(x),
                                    add = coll)
  }
  else
  {
    by <- 1
    
  }
  
  checkmate::reportAssertions(coll)
  
  if (length(unique(by)) > 5 & ncat_warn)
  {
    warning(sprintf("'%s' has more than 5 categories. This may not print well",
                    name_by))
  }
  
  test_result <- 
    if (!by_is_null)
    {
      do.call(test, 
              args = c(list(x, by),
                       list(...)))
    }
    else
    {
      do.call(test,
              args = c(list(x),
                       list(...)))
    }
  
  summary_table <- 
    data.frame(x = x, by = by) %>%
    dplyr::group_by(x, by) %>%
    dplyr::summarise(n = sum(!is.na(x) | !is.na(by))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(x) %>%
    dplyr::mutate(row_prop = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(by) %>%
    dplyr::mutate(col_prop = n / sum(n)) %>%
    tidyr::gather(measure, value,
                  n, col_prop, row_prop) %>%
    reshape2::dcast(x + by ~ measure,
                    value.var = "value",
                    fill = 0) %>%
    dplyr::ungroup() 
  
  structure(list(summary_table = summary_table,
                 test_result = broom::tidy(test_result)),
            class = "univariable_cat")
}
