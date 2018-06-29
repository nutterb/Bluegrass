#' @name lrt_any
#' @title Likelihood Ratio Tests for Models of any Class
#' 
#' @description A form of the likelihood ratio test that does not assume 
#'   the models are of the same form.  The motivation is to compare fixed
#'   effects models to mixed effects models to determine if the random 
#'   effects improve the fit.  Note, in most cases, it probably isn't wise
#'   to perform likelihood ratio tests between models of different types.
#'   
#'   The other advantage to this function is it always performs a likelihood
#'   ratio test. The most common way to perform this test is to use the 
#'   \code{anova} function. However, since \code{anova} is a generic, there 
#'   is not guarantee that what is returned is a likelihood ratio test.
#'   
#' @param fit0 A fit object with a \code{LogLik} method.
#' @param fit1 A fit object with a \code{LogLik} method.
#' 
#' @examples 
#' fit0 <- lm(mpg ~ qsec, data = mtcars)
#' fit1 <- lm(mpg ~ qsec + wt, data = mtcars)
#' fit2 <- lm(mpg ~ qsec + wt + disp, data = mtcars)
#' 
#' lrt_any(fit0, fit1)
#' lrt_any(fit1, fit2)
#' 
#' @export

lrt_any <- function(fit0, fit1)
{
  fit0_name <- as.character(substitute(fit0))
  fit1_name <- as.character(substitute(fit1))
  
  lr_fit0 <- stats::logLik(fit0)
  lr_fit1 <- stats::logLik(fit1)
  
  if (attr(lr_fit0, "df") == attr(lr_fit1, "df"))
  {
    stop("fit0 and fit1 may not have equal degrees of freedom")
  }
  
  if (attr(lr_fit0, "df") < attr(lr_fit1, "df"))
  {
    null_model <- lr_fit0
    null_model_name <- fit0_name
    alt_model <- lr_fit1
    alt_model_name <- fit1_name
  }
  else
  {
    null_model <- lr_fit1
    null_model_name <- fit1_name
    alt_model <- lr_fit0
    alt_model_name <- fit0_name
  }
  
  diff <- 2 * (as.numeric(alt_model) - as.numeric(null_model))
  df <- attr(alt_model, "df") - attr(null_model, "df")
  
  data.frame(null_model = null_model_name,
             alt_model = alt_model_name,
             null_loglik = as.numeric(null_model),
             alt_loglik = as.numeric(alt_model),
             likelihood_ratio = diff,
             df = df,
             p_value = stats::pchisq(diff, df, lower.tail = FALSE),
             stringsAsFactors = FALSE)
}
