#' @name movingRange
#' @export movingRange
#' 
#' @title Moving Range Calculation
#' @description The moving range is used in situations where subgroup
#'   sizes are of size n=1.  The short term variability is determined
#'   by the difference of successive points, which is then used to 
#'   estimate the long term variability.
#'   
#' @param x A vector of values.
#' 
#' @details The mathematical implementation is \code{abs(c(NA, diff(x)))} and
#'   is fairly trivial.  \code{movingRange} is provided for convenience and
#'   to reduce the potential for typographical mistakes in calculating the
#'   moving range.
#'   
#' @author Benjamin Nutter
#' 
#' @source
#' Donald J. Wheeler and David S. Chambers, \emph{Understanding Statistical Process Control, Second Edition},
#' SPC Press, Knoxville, TN, 1992. ISBN: 0-945320-13-2.
#' 
#' @examples
#' movingRange(c(39, 41, 41, 41, 43, 44, 41, 42, 40, 41, 44, 40))

movingRange <- function(x){
  abs(c(NA, diff(x)))
}

