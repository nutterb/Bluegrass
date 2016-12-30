#' @rdname univariable_cat
#' @title Univariable summary of a categorical variable.
#' 
#' @export

univariable_cont<- function(x, by = NULL, 
                            test = wilcox.test, ..., 
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
    dplyr::group_by(by) %>%
    dplyr::summarise(n = sum(!is.na(x)),
                     n_miss = sum(is.na(x)),
                     mean = mean(x, na.rm = TRUE),
                     sd = sd(x, na.rm = TRUE),
                     min = min(x, na.rm = TRUE),
                     p25 = quantile(x, na.rm = TRUE, probs = 0.25),
                     median = median(x, na.rm = TRUE),
                     p75 = quantile(x, na.rm = TRUE, probs = 0.75),
                     max = max(x, na.rm = TRUE)) %>%
    dplyr::ungroup() 
  
  structure(list(summary_table = summary_table,
                 test_result = broom::tidy(test_result)),
            class = "univariable_cont")
}
