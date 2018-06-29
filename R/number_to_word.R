#' @name number_to_text
#' @export number_to_text
#'
#' @title Convert Numbers to Text/Word Strings
#' @description Reprints numeric representations as printed words.  For example, '10' becomes
#'   'ten' and '28' becomes 'twenty eight'.
#'
#' @param x A numeric vector
#'
#' @details This will fail whenever R forces the number to be represented in 
#'   scientific notation. There is some relief from this as 
#'   \code{number_to_word} will attempt to reset the \code{scipen} option to 
#'   prevent the use of scientific notation, but sometimes R insists on
#'   doing things its own way.  I'm sure there's a good, succinct explanation 
#'   for when to expect this to fail, but since it is mostly likely to happen 
#'   at preposterously large values (into the trillions), I'm not too concerned 
#'   about because let's face it--at that point, you should probably reconsider 
#'   if you really want to print out the number as text.  Do note, however,
#'   that exact powers of 10 that exceed one billion are problematic.
#'
#'   \code{number_to_word} also starts to lose precision in the argument check at 
#'   4,503,599,627,370,496 (four quadrillion). Precision in the translation 
#'   appears to be lost for values larger than 9,999,999,999,998. (almost ten
#'   quadrillion).
#'   
#' @seealso \code{\link{word_to_number}}
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item return a character vector the same length as \code{x}
#'   \item Cast an error if \code{x} is not integerish
#'   \item Cast a warning if any value in \code{x} is larger than 
#'     9,999,999,999,998
#' }
#'
#' @author Benjamin Nutter
#'
#' @examples
#' number_to_text(0:20)
#' 
#' # Largest value before loss of precision in modulo
#' number_to_text(4503599627370496)
#' number_to_text(4503599627370497)
#' 
#' # Limits of precision on the translation
#' number_to_text(9999999999999998)
#' number_to_text(9999999999999999)  # Incorrectly translated
#' number_to_text(10000000000000000) # Correctly translated
#' number_to_text(10000000000000001) # Incorrectly translated

number_to_text <- function(x)
{
  coll <- checkmate::makeAssertCollection()
  
  if (any(!vapply(x %% 1, is_equal, logical(1), y = 0)))
  {
    coll$push("All values in x must be integer-like values")
  }
  
  if (any(x > 9999999999999998)){
    warning("No promises that values larger than (approx.) ten quadrillion will ",
            "be translated correctly")
  }
  
  checkmate::reportAssertions(coll)

  # Prevent scientific notation
  orig.scipen <- getOption("scipen")
  options(scipen = max(nchar(x)))
  on.exit(options(scipen = orig.scipen))
  
  # Separate each grouping
  x <- format(x, 
              big.mark=",", 
              scientific = FALSE)
  x <- trimws(x)
  x <- strsplit(x, ",")
  
  x <- lapply(x, translate_hundred)
  
  # number_groupings is an unexported character vector
  x <- sapply(x,
              function(n, g=number_groupings)
              {
                paste(paste(n[n != ""], rev(g[1:length(n)]))[n != ""], collapse=" ")
              })
  trimws(x)
}

#' @rdname number_to_text

translate_hundred <- function(x)
{
  if (length(x) == 1)
  {
    if (x == "0") return ("zero")
  }
  
  places <- c("one", "ten", "hundred")
  x <- sprintf(paste0("%0", max(nchar(x)), "d"), as.numeric(x))
  X <- as.data.frame(do.call("rbind", strsplit(x, "")),
                     stringsAsFactors=FALSE)
  
  names(X) <- rev(places[1:ncol(X)])
  
  if ("ten" %in% names(X))
  {
    is_ten_one <- X[["ten"]] == 1
    X[["one"]][is_ten_one] <- 
      paste0(X[["ten"]][is_ten_one], 
             X[["one"]][is_ten_one])
    
    X[["ten"]][is_ten_one] <- ""
  }
  
  for (p in rev(places))
  {
    if (p %in% names(X))
    {
      # numRef is an unexported data frame used for reference
      X[[paste0(p, "_place")]] <- 
        num_ref[[paste0(p, "_place")]][match(X[[p]], num_ref$num)]
    }
  }
  
  text <- apply(X[, !names(X) %in% places, drop=FALSE],
                1,
                function(t, ...) paste(t[t != ""], ...),
                collapse = " ")
  trimws(text)
}

# UNEXPORTED --------------------------------------------------------

number_groupings <- c("",               "thousand",
                      "million",        "billion",      "trillion",         "quadrillion",
                      "quintillion",    "sextillion",   "septillion",       "octillion",
                      "nonillion",      "decillion",
                      "undecillion",    "duodecillion", "tredecillion",     "quattuor-decillion",
                      "quindecillion",  "sexdecillion", "septen-decillion", "octodecillion",
                      "novemdecillion", "vigintillion", "centillion")

num_ref <- 
  data.frame(num = c("", 0:19),
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
                               rep("", 10)),
             stringsAsFactors = FALSE)
