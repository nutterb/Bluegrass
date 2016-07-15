#' @name vector_code
#' @title Print Vector to Console as R Code
#'
#' @description Prints a vector to the console as R code that can be 
#' copied and pasted into a script.
#' 
#' @param x A vector
#' @param width The maximum width of each line of code.  By default, 
#'   it uses the \code{width} option.
#'   
#' @details This is intended as a code-writing utility.  As a convenience,
#'   the string returned to the console will have a width that will
#'   paste well into the user's code (instead of running off the 
#'   right side of the screen).  To maintain the width, efficiency has 
#'   been sacrificed.  It is assumed that this will not be used on 
#'   large vectors.
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accept a vector of any type.
#'   \item Return a string suitable for a copy and paste into code.
#'   \item The string returned must provide the same class as the
#'         original vector (within limits: dates will be strings)
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @examples
#' vector_code(1:3)
#' 
#' @export

vector_code <- function(x, width=getOption("width")){
 
 if (inherits(x, "Date")) x <- format(x,
                                      format = "%Y-%m-%d")
 if (inherits(x, "POSIXct")) x <- format(x, 
                                         format = "%Y-%m-%d %H:%M:%S")
 
 #* Format with quotes, if necessary
 #* Also, put commas at the end of each element.
 if (is.character(x) | is.factor(x))
 {
  x <- sprintf("\"%s\", ", 
               x)
 }
 else 
 {
  x <- sprintf("%s, ",
               x)
 }
 
 #* Break into lines of code
 x_char <- NULL;
 i <- 1
 while(length(x) > 0){
  char_count <- cumsum(nchar(x))
  x_char[[i]] <- paste(x[which(char_count <= width)], collapse="")
  x <- x[which(!char_count <= width)]
  i <- i + 1
 }
 
 #* Format as code
 x_char[1] <- paste0("c(", x_char[1])
 x_char[length(x_char)] <- paste0(substr(x_char[length(x_char)],
                                         1,
                                         nchar(x_char[length(x_char)])-2),
                                  ")")
 x_char <- paste(x_char, collapse="\n")
 
 cat(x_char)
}