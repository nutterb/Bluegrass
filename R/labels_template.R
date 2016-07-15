#' @name labels_template
#'
#' @title Generate the code to alter the labels in a data frame.
#'
#' @description Using labels from the \code{\link[Hmisc]{label}} functions in 
#'   can be useful in clarifying reports and making them more readable 
#'   to investigators.  In large data frames, however, it can be tedious 
#'   to write out the \code{label(df$var) <- ''} part of the code for 
#'   every variable.  \code{labels_template} prints the necessary code
#'   to the console for a quick copy and paste of all of the variables.  
#'   Options are provided for sorting the variable names alphabetically, 
#'   and for retaining their current values in the template.
#'
#' @param data The data frame for which the labels template should be 
#'   written.
#' @param sort \code{logical(1)}. If \code{FALSE} (default), variables 
#'   appear in the same order as they appear in the data frame.  
#'   If \code{TRUE}, variables are listed in alphabetical order.
#' @param retain_current \code{logical(1)}.  If \code{TRUE}, the current 
#'   values in the labels attribute is printed with the template.  
#'   If \code{FALSE}, all labels are given an empty character set.
#' @param prefix \code{logical(1)}.  When \code{TRUE}, the commands are 
#'   printed as \code{Hmisc::label}.  This is necessary if the 
#'   \code{Hmisc} package is not loaded into the workspace.
#'
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accepts an object that inherits \code{data.frame}
#'   \item Variables may be ordered in the regular order or alphabetically
#'   \item The template may be empty or include current values
#'   \item The template may include the \code{Hmisc::} prefix, allowing 
#'         use without loading the \code{Hmisc} package.
#' }
#'
#' @author Benjamin Nutter
#'
#' @examples
#' library(Hmisc)
#' 
#' mtcars2 <- mtcars # make a copy of mtcars data set
#' 
#' label(mtcars2$am) <- "Automatic Transmission"
#' label(mtcars2$cyl) <- "Number of Cylinders"
#' 
#' labels_template(mtcars2)
#' labels_template(mtcars2, sort = TRUE)
#' labels_template(mtcars2, retain_current = FALSE)
#' 
#' @export


labels_template <- function(data, sort = FALSE, 
                            retain_current = TRUE, prefix = TRUE)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(
    x = data,
    classes = "data.frame",
    add = coll
  )
  
  checkmate::assert_logical(
    x = sort,
    len = 1,
    add = coll
  )
  
  checkmate::assert_logical(
    x = retain_current,
    len = 1,
    add = coll
  )
  
  checkmate::assert_logical(
    x = prefix,
    len = 1,
    add = coll
  )
  
  checkmate::reportAssertions(coll)
  
  dataname <- substitute(data)
  
  nms <- names(data)
  
  if (sort) nms <- sort(nms)
  
  prefix <- if (prefix) "Hmisc::" else ""
  
  lbl <- 
    if (retain_current) 
    {
      sprintf(
        "%slabel(%s$%s) <- \"%s\"",
        prefix,
        as.character(dataname),
        nms,
        Hmisc::label(data[, nms], self = FALSE)
      )
    }
    else
    {
      sprintf(
        "%slabel(%s$%s) <- \"\"",
        prefix,
        as.character(dataname),
        nms
      )
    }
  
  cat(lbl, sep="\n")
}
