#' @name univariable_table
#' @title Univariable Table Summary
#' 
#' @description Performs a number of iterations to \code{univariable_cat} 
#'   and \code{univariable_cont} to construct a table of univariable summaries
#'   and comparisons. This is intended to streamline the generation of 
#'   "Table 1"s. 
#'   
#' @param data A data frame like object.
#' @param vars A character vector of variable names. The order listed here
#'   determines the order the variables are listed in the table.
#' @param by A \code{character(1)} specifying a variable in \code{data} to 
#'   use as a grouping variable.
#' @param ... lists specifying tests and parameters to use for variables given in 
#'   \code{vars}.  These are used to override the defaults of 
#'   \code{univariable_cat} (\code{chisq.test}) and \code{univariable_cont} 
#'   (\code{wilcox.test}).  See Specifying Tests.
#'   
#' @section Specifying Tests:
#' There is no need to use \code{...} if you are satisfied with the default 
#' tests.  Each variable that requires a different test must have an entry in 
#' \code{...}.  Each entry must have an element named \code{test}. Additional
#' elements may be made to pass to the test.
#' 
#' If you would like to alter the arguments to a default test, you may 
#' pass the arguments without naming a \code{test} element.  
#' 
#' For a variable named \code{xvar} to be compared with a t-test, the 
#' entry in \code{...} would look like 
#' will look like: 
#' \code{xvar = list(test = t.test, alternative = 'greater')}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accept a data frame like object
#'   \item Accept a vector of variable name in \code{data} to summarize.
#'   \item Accept a \code{character(1)} giving a variable name to use as 
#'     a grouping variable.
#'   \item Accept a character vector of variables listed in \code{vars}
#'     to treat at categorical variables. (an override of numeric types)
#'   \item Gather all warnings and return them as one.
#'   \item Warnings should indicate the analysis in which the warning 
#'     was generated.
#'   \item Allow the user to designate alternate tests for each variable
#'     in \code{vars}
#'   \item Capture errors thrown during tests, return them as warnings
#'   \item Return NULL when an error occurs during a test
#' }
#' 
#' @examples 
#' univariable_table(data = mtcars,
#'                   vars = c("mpg", "vs", "qsec", "gear"),
#'                   by = "cyl",
#'                   cat_vars = c("vs", "gear"),
#'                   mpg = list(test = t.test, alternative = "greater"))
#' 
#' @export

univariable_table <- function(data, vars, by = NULL, 
                              cat_vars = NULL, ...)
{
  #******************************************************************
  #* Argument Validation
  #* 1. `data` is a data.frame like object
  #* 2. `vars` is a character vector
  #* 3. `by` is a character vector of length 1
  #* 4. `cat_vars` is a character vector
  #* 5. All values in `vars` and `by` are variables in `data`
  #******************************************************************
  
  coll <- checkmate::makeAssertCollection()
  
  #* 1. `data` is a data.frame like object
  checkmate::assert_class(x = data,
                          classes = "data.frame",
                          add = coll)
  
  #* 2. `vars` is a character vector
  checkmate::assert_character(x = vars,
                              add = coll)
  
  #* 3. `by` is a character vector of length 1
  if (!is.null(by))
  {
    checkmate::assert_character(x = by,
                                len = 1,
                                add = coll)
  }
  else
  {
    by <- "..by_var.."
    data[["..by_var.."]] <- 1
  }
  
  #* 4. `cat_vars` is a character vector
  if (!is.null(cat_vars))
  {
    checkmate::assert_character(x = cat_vars,
                                add = coll)
  }
  
  #* If any of the assertions have failed thus far, there isn't much 
  #* point in continuing.
  checkmate::reportAssertions(coll)
  
  #* 5. All values in `vars` and `by` are variables in `data`
  if (any(!c(vars, by) %in% names(data)))
  {
    coll$push(
      c(vars, by)[!c(vars, by) %in% names(data)] %>%
        paste(collapse = "`, `") %>%
        paste0("`", ., "`") %>%
        sprintf("The following variables are not found in `data`: %s", .) 
    )
  }
  
  checkmate::reportAssertions(coll)
  
  #******************************************************************
  #* Additional warnings
  #******************************************************************
  
  if (length(unique(data[[by]])) > 5)
  {
    warning(sprintf("'%s' has more than 5 categories. This may not print well",
                    by))
  }
  
  #******************************************************************
  #* Function Body
  #* 1. Identify numeric variables
  #* 2. Separate custom defined tests and their parameters.
  #* 3. Produce the summaries for each variable.
  #* 4. Separate the summaries from the warnings
  #* 5. Isolate the warnings and send them.
  #* 6. Return the summaries
  #******************************************************************
  
  #* 1. Identify the numeric variables.
  numeric_vars <- 
    vapply(data[, vars],
           FUN = is.numeric,
           FUN.VALUE = logical(1))
  
  #* Flag numeric variables as categorical if they are in `cat_vars`
  if (!is.null(cat_vars))
  {
    numeric_vars[vars %in% cat_vars] <- FALSE
  }
  
  #* 2. Separate custom defined tests and their parameters.
  test_param <- list(...)
  
  #*         Unexported function (defined below)
  test_fn <- fill_default_test(vars, numeric_vars, test_param)
  
  #*          Unexported function (defined below)
  test_arg <- fill_test_arg(vars, test_param)
  
  #* 3. Produce the summaries for each variable.
  #*             Unexported function (defined below)
  var_summary <- variable_summary(data, vars, by, 
                                  numeric_vars, test_fn, test_arg)
  
  #* 4. Separate the summaries from the warnings
  output <- 
    lapply(var_summary,
           `[[`,
           "value")
  
  #* 5. Isolate the warnings and send them.
  warn <- 
    lapply(var_summary,
           `[[`,
           "warnings")
  
  warn <- warn[!vapply(warn, is.null, logical(1))]
  
  if (length(warn))
  {
    mapply(FUN = function(nm, w) if (!is.null(w)) sprintf("%s: %s", nm, w),
           nm = names(warn),
           w = warn,
           SIMPLIFY = FALSE) %>%
      paste0("\n") %>%
      warning(call. = FALSE)
  }
  
  #* 6. Return the summaries
  output[vars]

}


#********************************************************************
#* Unexported Functions
#********************************************************************

fill_default_test <- function(vars, numeric_vars, test_param)
{
  test_fn <- lapply(test_param,
                    `[[`,
                    "test")
  
  mapply(
    FUN = 
      function(v, numeric_vars)
      {
        if (v %in% names(test_fn)) unlist(test_fn[[v]])
        else if (numeric_vars) wilcox.test
        else chisq.test
      },
    v = vars,
    numeric_vars = numeric_vars,
    SIMPLIFY = FALSE
  )
}

fill_test_arg <- function(vars, test_param)
{
  test_arg <- lapply(test_param,
                     function(l) l[!names(l) %in% "test"])
  
  lapply(vars, 
         function(v) if (v %in% names(test_arg)) test_arg[[v]] else NULL)
}

variable_summary <- function(data, vars, by, numeric_vars, 
                             test_fn, test_arg)
{
  #* this is a really ugly way of running 
  #* univariable_cat(x = data[[var]],
  #*                 by = data[[by]],
  #*                 test = test_fn[i],
  #*                 ...)
  #* This has to be run using `do.call` because of the way custom 
  #* test arguments are passed to `univariable_table`
  
  #* container for the results
  var_summary <- vector("list", length = length(vars))
  
  
  for (i in seq_along(var_summary))
  {
    var_summary[[i]] <- 
      with_warning(
        tryCatch(
          do.call(
            #* Determine if it is numeric or categorical
            what = 
              if (numeric_vars[i])
              {
                "univariable_cont" 
              }
            else 
            {
              "univariable_cat"
            },
            args = 
              #* General arguments
              c(list(x = data[[vars[i]]],
                     by = data[[by]],
                     test = test_fn[[i]],
                     ncat_warn = FALSE),
                #* Test specific arguments
                test_arg[[i]])
          ),
          #* Capture any errors thrown, return them as warnings.
          #* A NULL value is returned in place of the test result.
          error = function(condition)
          {
            warning(sprintf("Error occurred (NULL object returned): %s",
                            conditionMessage(condition)), 
                    call. = FALSE)
            NULL
          }
        )
      )
  }
  
  #* Name the list elements
  names(var_summary) <- vars
  
  structure(var_summary,
            class = "univariable_table")
}


