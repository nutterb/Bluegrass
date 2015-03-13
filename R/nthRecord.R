#' @rdname FirstLastRecord
#' @export nthRecord
#' @importFrom plyr ddply
#' 
#' @param n The record number to pull from each ID.

nthRecord <- function(data, id, ..., n=1){
  #* Order the data frame
  if (as.character(substitute(id))[1] == "c")
    id2 <- paste(as.character(substitute(id))[-1], collapse=", ")
  else id2 <- as.character(substitute(id))
  
  expr <- gsub("desc[(]", "plyr::desc(",
               as.character(substitute(plyr::arrange(data, id2, ...))))
  expr <- paste0(expr[1], "(", paste(expr[-1], collapse=","), ")")
  data <- eval(parse(text=expr))
  
  #* Retrieve the nth row
  data <- plyr::ddply(data, substitute(id), 
                      function(x){
                        if (n > nrow(x)) NULL
                        else x[n, , drop=FALSE]})
  return(data)
}
