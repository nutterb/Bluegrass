#' @name numberToWord
#' @export numberToWord
#' @importFrom dplyr mutate
#' @importFrom stringr str_pad
#' @importFrom stringr str_split
#' @importFrom stringr str_trim
#' 
#' @title Convert Numbers to Text/Word Strings
#' @description Reprints numeric representations as printed words.  For example, '10' becomes
#'   'ten' and '28' becomes 'twenty eight'.
#'   
#' @param x A numeric vector
#' 
#' @details This will fail whenever R forces the number to be represented in scientific notation.
#'   There is some relief from this as \code{numberToWord} will attempt to resent the 
#'   \code{scipen} option to prevent the use of scientific notation, but sometimes R insists on
#'   doing things its own way.  I'm sure there's a good, succinct explanation for when to expect
#'   this to fail, but since it is mostly likely to happen at preposterously large values (into the
#'   trillions), I'm not too concerned about because let's face it--at that point, you should
#'   probably reconsider if you really want to print out the number as text.  Do note, however, 
#'   that exact powers of 10 that exceed one billion are problematic.
#'   
#'   \code{numberToWord} also starts to lose precision somewhere between 1 and 2 quadrillion.
#'   I apologize for not being more specific for that, but my motivation for this was I needed
#'   to be able to print numbers between one and thirty one.
#' 
#' @author Benjamin Nutter
#' 
#' @examples
#' numberToWord(0:20)
#' numberToWord(1549846132187489464)
#' 

numberToWord <- function(x)
{
  orig.scipen <- getOption("scipen")
  options(scipen = max(nchar(x)))
  on.exit(options(scipen = orig.scipen))
  
  groups <- c("",               "thousand",    
              "million",        "billion",      "trillion",         "quadrillion", 
              "quintillion",    "sextillion",   "septillion",       "octillion",   
              "nonillion",      "decillion",
              "undecillion",    "duodecillion", "tredecillion",     "quattuor-decillion",
              "quindecillion",  "sexdecillion", "septen-decillion", "octodecillion",
              "novemdecillion", "vigintillion", "centillion")
              
  x <- format(x, big.mark=",")
  x <- stringr::str_trim(x)
  x <- stringr::str_split(x, ",")

  x <- lapply(x, translateHundred)
  
  x <- sapply(x, 
         function(n, g=groups)
         {
           paste(paste(n[n != ""], rev(g[1:length(n)]))[n != ""], collapse=" ")
         })
  stringr::str_trim(x)
}

#' @rdname numberToWord
#' 
translateHundred <- function(x)
{
  if (length(x) == 1) if (x == "0") return ("zero")
  numRef <- data_frame(num = c("", 0:19),
                       one_place = c("",         "",    "one",     "two",       "three", 
                                     "four",     "five",    "six",     "seven",     "eight", 
                                     "nine",     "ten",     "eleven",  "twelve",    "thirteen",
                                     "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
                                     "nineteen"),
                       ten_place = c("",         "",        "",        "twenty",    "thirty", 
                                     "forty",    "fifty",   "sixty",   "seventy",   "eighty", 
                                     "ninety",   rep("", 10)),
                       hundred_place = c("", "", 
                                         paste(c("one", "two", "three", "four", "five", "six",
                                                 "seven", "eight", "nine"), "hundred"),
                                         rep("", 10)))
  places <- c("one", "ten", "hundred")
  x <- stringr::str_pad(string = x,
                        width = max(nchar(x)),
                        side = "left",
                        pad = "0")
  X <- as.data.frame(do.call("rbind", stringr::str_split(x, "")), 
                     stringsAsFactors=FALSE)[, -1, drop=FALSE]
  
  names(X) <- rev(places[1:ncol(X)])
  
  if ("ten" %in% names(X))
  {
    X <- X %>%
      dplyr::mutate(one = ifelse(ten == 1,
                                 yes = paste0(ten, one),
                                 no = one),
                    ten = ifelse(ten == 1,
                                 yes = "",
                                 no = ten))
  }

  for (p in rev(places))
  {
    if (p %in% names(X))
    {
      X[[paste0(p, "_place")]] <- numRef[[paste0(p, "_place")]][match(X[[p]], numRef$num)]
    }
  }

  text <- apply(X[, !names(X) %in% places, drop=FALSE], 
                1,
                function(t, ...) paste(t[t != ""], ...),
                collapse = " ")
  stringr::str_trim(text)
}

