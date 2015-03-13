#' @rdname FirstLastRecord
#' @export lastRecord

lastRecord <- function(data, id, ...){
  #* Order the data frame
  if (as.character(substitute(id))[1] == "c")
    id2 <- paste(as.character(substitute(id))[-1], collapse=", ")
  else id2 <- as.character(substitute(id))
  
  expr <- gsub("desc[(]", "plyr::desc(",
               as.character(substitute(plyr::arrange(data, id2, ...))))
  expr <- paste0(expr[1], "(", paste(expr[-1], collapse=","), ")")
  data <- eval(parse(text=expr))
  
  #* Identify duplicates
  expr <- paste0("duplicated(data[, c('", 
                 gsub(", ", "', '", id2), 
                 "'), drop=FALSE], fromLast=TRUE)")
  dup <- eval(parse(text=expr))
  return(data[!dup, , drop=FALSE])
}
