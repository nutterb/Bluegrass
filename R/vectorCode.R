#' @name vectorCode
#' @export vectorCode
#' 
#' @title Print Vector to Console as R Code
#' @description Prints a vector to the console as R code that can be copied and pasted
#' into a script.
#' 
#' @param x A vector
#' @param width The maximum width of each line of code.  By default, it uses the 
#'   \code{width} option.  
#' 
#' @author Benjamin Nutter
#' 
#' @examples
#' vectorCode(1:3)
vectorCode <- function(x, width=getOption("width")){
  #* Format with quotes, if necessary
  #* Also, put commas at the end of each element.
  if (is.character(x) | is.factor(x))
    x <- paste0("\"", x, "\", ")
  else if (is.numeric(x)) x <- paste0(x, ", ")
  
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
