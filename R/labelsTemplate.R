#' @name labelsTemplate
#' @export labelsTemplate
#' @importFrom Hmisc label.default
#' @importFrom Hmisc label.data.frame
#' @importFrom Hmisc 'label<-.default'
#' @importFrom Hmisc 'label<-.data.frame'
#' @importFrom Hmisc print.labelled
#' @importFrom Hmisc '[.labelled'
#'
#' @title Generate the code to alter the labels in a data frame.
#'
#' @description Using labels from the \code{label} functions in \code{Hmisc} can be useful in clarifying reports and
#'   making them more readable to investigators.  In large data frames, however, it can be tedious to write out the 
#'   \code{label(df$var) <- ''} part of the code for every variable.  \code{labels.template} prints the necessary code
#'   to the console for a quick copy and paste of all of the variables.  Options are provided for sorting the 
#'   variable names alphabetically, and for retaining their current values in the template.
#'
#' @param data The data frame for which the labels template should be written.
#' @param sort Logical. If \code{FALSE} (default), variables appear in the same order as they appear in the data frame.
#'   If \code{TRUE}, variables are listed in alphabetical order.
#' @param retain.current Logical.  If \code{TRUE}, the current values in the labels attribute is printed with the 
#'   template.  If \code{FALSE}, all labels are given an empty character set.
#' @param prefix Logical.  When \code{TRUE}, the commands are printed as 
#'   \code{Hmisc::label}.  This is necessary if the \code{Hmisc} package is not
#'   loaded into the workspace.
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
#' labelsTemplate(mtcars2)
#' labelsTemplate(mtcars2, sort=TRUE)
#' labelsTemplate(mtcars2, retain.current=FALSE)


labelsTemplate <- function(data, sort=FALSE, retain.current=TRUE, prefix=TRUE){
  dataname <- substitute(data)
  nms <- names(data)
  if (sort) nms <- sort(nms)
  lbl <- if (retain.current) 
            paste0(if (prefix) "Hmisc::" else "", 
                   "label(", dataname, "$", nms, ") <- \"",
                   Hmisc::label(data[, nms], self=FALSE), "\"")
         else paste0(if (prefix) "Hmisc::" else "",
                     "label(", dataname, "$", nms, ") <- \"\"")
  cat(lbl, sep="\n")
}
