#' @name ControlChartConstants
#' 
#' @title Control Chart Constants
#' @description Functions to calculate or reference control chart constants.
#' 
#' @param n Subgroup sample size
#' @param rms Determines if the root mean square deviation is used or the standard deviation.  
#'   The root mean deviation divides by \code{n} while the standard deviation divides by 
#'   \code{n-1}.
#' 
#' @details All of the constants are directly calculated within R except for d4, A6, and A9.  I 
#'   have been unable to determine a way to calculate the d4 constant from the \code{ptukey} 
#'   function, nor have I figured out the relationship between the skewness of the studentized 
#'   range distribution and d4.  I have not found formulae for A6 and A9.  Instead, the values of
#'   these constants are copied out of Wheeler and Chambers and referenced out of the a data 
#'   frame.  In cases where the supplied value of \code{n} is not found in the reference table,
#'   the constant for the largest tabulated n that is smaller than \code{n} is used.
#' 
#' @author 
#' Benjamin Nutter (except for d2 and d3)
#' 
#' Barbosa et. al (d2, d3) (vectorized by Nutter)
#' 
#' @source
#' Donald J. Wheeler and David S. Chambers, \emph{Understanding Statistical Process Control, Second Edition},
#' SPC Press, Knoxville, TN, 1992. ISBN: 0-945320-13-2.\cr
#' For c2 and c4, see page 393. \cr
#' For A2, D3, and D4, see page 394. \cr
#' For A4, D5, and D6, see page 395. (d4 not yet defined) \cr 
#' For A1, B3, and B4, see page 396. \cr
#' For A3, B3, and B4, see page 397. \cr
#' For A6 and A9, see pages 398-399.
#'  
#' 
#' Emanuel P. Barbosa, Flavio M. M. Barros, Elias de Jesus Goncalves and Daniela R. Recchia (2014). 
#' \emph{IQCC: Improved Quality Control Charts}. R package version 0.6. http://CRAN.R-project.org/package=IQCC
#' d2 and d3 are copied from their package.
#' 

#' @rdname ControlChartConstants
A1 <- function(n) 3 / (c2(n) * sqrt(n))

#' @rdname ControlChartConstants
A2 <- function(n) 3 / (d2(n) * sqrt(n))

#' @rdname ControlChartConstants
A3 <- function(n) 3 / (c4(n) * sqrt(n))

#' @rdname ControlChartConstants
A4 <- function(n){
  3 / (d4(n) * sqrt(n))
}

#' @rdname ControlChartConstants
A6 <- function(n){
  A <- data.frame(n = c(3, 5, 7, 9, 11),
                  A6 = c(1.187, 0.691, 0.509, 0.412, 0.350))
  A$A6[sapply(n, function(n) max(which(A$n <= n)))]
}

#' @rdname ControlChartConstants
A9 <- function(n){
  A <- data.frame(n = c(3, 5, 7, 9),
                  A9 = c(1.265, 0.712, 0.520, 0.419))
  A$A9[sapply(n, function(n) max(which(A$n <= n)))]
}

#' @rdname ControlChartConstants
B3 <- function(n, rms=TRUE){
  cn <- if (rms) c2(n) else c4(n)
  1 - (3 / cn) * sqrt((n-1)/n - cn^2)
}

#' @rdname ControlChartConstants
B4 <- function(n, rms=TRUE){
  cn <- if (rms) c2(n) else c4(n)
  1 + (3 / cn) * sqrt((n-1)/n - cn^2)
}

#' @rdname ControlChartConstants
c2 <- function(n) sqrt(2/n) * (gamma(n/2) / gamma((n-1)/2))

#' @rdname ControlChartConstants
c4 <- function(n) c2(n) * sqrt(n/(n-1))

#' @rdname ControlChartConstants
d2 <- function (n) 
{
  fn <- quote(function(w){ptukey(w, n, Inf, lower.tail=FALSE)})
  sapply(n, 
         function(n)
         {
           if (n == 1) integrate(function(w){ptukey(w, 2, Inf, lower.tail=FALSE)}, 0, Inf)[[1]]
           else integrate(eval(fn), 0, Inf)[[1]]
         })
}

#' @rdname ControlChartConstants
d3 <- function (n) 
{
  d2 <- d2(n)
  fn <- quote(function(w){ w * (1 - ptukey(w, n, Inf))})
  d <- sapply(n,
              function(n) integrate(eval(fn), 0, Inf)[[1]])
  sqrt(2 * d - d2^2)
}

#' @rdname ControlChartConstants
d4 <- function(n)
{
  D <- data.frame(n = c(2:25, seq(30, 50, by=5), seq(60, 100, by=10)),
                  d4 = c(.954, 1.588, 1.978, 2.257, 
                         2.472, 2.645, 2.791, 2.915, 3.024,
                         3.121, 3.207, 3.285, 3.356, 3.422, 
                         3.482, 3.538, 3.591, 3.640, 3.686,
                         3.730, 3.771, 3.811, 3.3847, 
                         3.883, 4.037, 4.166, 4.274, 4.372,
                         4.450, 4.591, 4.707, 4.806, 4.892,
                         4.968))
  D$d4[sapply(n, function(n) max(which(D$n <= n)))]
}


#' @rdname ControlChartConstants
D3 <- function(n){
  D <- 1 - (3 * d3(n) / d2(n))
  D <- ifelse(D < 0, NA, D)
  D
}

#' @rdname ControlChartConstants
D4 <- function(n){
  ifelse(n == 1, 3.268, 1 + (3 * d3(n) / d2(n)))
}

#' @rdname ControlChartConstants
D5 <- function(n)
{
  (d2(n) - 3 * d3(n)) / d4(n)
}

#' @rdname ControlChartConstants
D6 <- function(n)
{
  (d2(n) + 3 * d3(n)) / d4(n)
}
  