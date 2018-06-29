#' @name word_to_number
#' @title Convert Text-Represented Numbers to Their Numeric Representation
#' 
#' @description Convert a text-based numeric descriptor into its numeric 
#'   form.  For instance, "three" becomes "3".  Capable of handling 
#'   the special cases like "twelve hundred".
#'   
#' @param x A character vector. All punctuation and instances of " and "
#'   will be stripped from the vector within the function. In other words,
#'   "eighty seven", "eighty-seven", and "eighty and seven" will all 
#'   produce the same result.
#' 
#' @details The conversion method here recognizes that it is only necessary
#'   to translate anything between 0 and 999.  After that point, everything
#'   begins to repeat, and only an adjustment to the thousands, millions, 
#'   etc need be applied.  The translation, then, takes place in three steps.
#'   First, the number is broken into magnitude groups in which the hundreds
#'   may be translated. Second, each magnitude is translated. Finally, 
#'   the translation of each magnitude is multiplied by its magnitude's value
#'   and all of the results are summed.
#'   
#'   For example, the description "thirteen million three hundred and ninety 
#'   one thousand four hundred and twelve" is broken into three groups:
#'   
#'   \itemize{
#'     \item "thirteen million"
#'     \item "three hundred and ninety one thousand"
#'     \item "four hundred and twelve"
#'   }
#'   
#'   The translation of each becomes
#'   
#'   \itemize{
#'     \item 13 * 1000000
#'     \item 391 * 1000
#'     \item 412
#'   }
#'   
#'   Those three translations are added together to yield 13391412.
#' 
#' @seealso \code{\link{number_to_word}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Returns a numeric vector translate the contents of \code{x}
#'  \item Cast an error if \code{x} is not a character vector.
#'  \item Cast an error if any words in \code{x} do not match the 
#'    names of \code{c(word_to_number_reference, magnitude_reference)}
#'  \item Cleanses \code{x} of all punctuation and instances of the 
#'    string \code{" and "}.
#'  \item Cast a warning if any string failed to cast an error but 
#'    could not be translated.
#' }
#' 
#' @examples 
#' word_to_number(c("one hundred eighty six", 
#'                  "three hundred and seventy-one",
#'                  "twelve hundred eighty-eight"))
#' word_to_number("thirteen million three hundred and ninety one thousand four hundred and twelve")
#' word_to_number("eight hundred and eleven")
#' @export

word_to_number <- function(x){
  # Remove punctuation and 'and'
  x <- tolower(gsub("([[:punct:]]| and )", " ", x))
  # separate into distinct words
  x <- strsplit(x, "\\s+")
  x <- lapply(X = x,
              FUN = trimws)
  
  coll <- checkmate::makeAssertCollection()
  
  # verify that all words are found in the reference vectors.
  bad_text_in_x <- 
    vapply(X = x,
           FUN = function(k) (!all(k %in% names(c(word_to_number_reference, magnitude_reference)))),
           FUN.VALUE = logical(1))
  
  if (any(bad_text_in_x))
    coll$push("Text found that is not compatible with conversion. Check your spelling?")
  
  checkmate::reportAssertions(coll)
  
  num <- vapply(X = x,
                FUN = word_to_number_single,
                FUN.VALUE = numeric(1))

  if (any(is.na(num)))  
    warning(sprintf("Unable to translate '%s'", 
                    paste0(x[is.na(num)], collapse = ', ')))
  
  num
}

# UNEXPORTED --------------------------------------------------------

word_to_number_single <- function(x){
  # translate words to the numeric reference
  num <- c(word_to_number_reference, magnitude_reference)[x]
  
  # Identify positions with a magnitude indicator
  magnitude_at <- 
    which(names(num) %in% 
            c("quadrillion", "trillion", "billion",
              "million", "thousand"))
  
  # Create an indexing vector for each magnitude class of the number
  magnitude_index <- 
    cut(seq_along(num), 
        breaks = unique(c(0, magnitude_at, length(num))))
  
  # Make a list with each magnitude
  num_component <- 
    lapply(unique(magnitude_index),
           FUN = function(i) num[magnitude_index == i])
  
  # Transate each component
  num_component <- 
    vapply(num_component,
           FUN = word_to_number_translate_hundred,
           FUN.VALUE = numeric(1))
  
  # Add the components together
  sum(num_component)
}

# ROUTINE TO TRANSLATE HUNDREDS

word_to_number_translate_hundred <- function(n){
  # set a magnitude multiplier for thousands and greater
  if (tail(names(n), 1) %in% names(magnitude_reference)){
    magnitude <- tail(n, 1)
    n <- head(n, -1)
  } else {
    magnitude <- 1
  }
  
  # if hundred appears anywhere but the second position or of the
  # value preceding hundred is greater than 9, handle with care
  # (for instance, 1200)
  if ( ("hundred" %in% names(n) && which(names(n) == "hundred") != 2) ||
       ("hundred" %in% names(n) && n[1] > 1) )
  {
    which_hundred <- which(names(n) == "hundred")
    (sum(n[seq_along(n) < which_hundred]) * 100 + 
        sum(n[seq_along(n) > which_hundred])) * magnitude
  } else {
    op <- rep("+", length(n) - 1)
    op[names(n)[-1] == "hundred"] <- "*"
    op <- c(op, "")
    eval(parse(text = paste(paste(n, op), collapse = " "))) * magnitude
  }
}

# CORE REFERENCE NUMBERS

word_to_number_reference <- 
  c("zero" = 0,
    "one" = 1,
    "two" = 2,
    "three" = 3,
    "four" = 4,
    "five" = 5,
    "six" = 6,
    "seven" = 7,
    "eight" = 8,
    "nine" = 9,
    "ten" = 10,
    "eleven" = 11,
    "twelve" = 12,
    "thirteen" = 13,
    "fourteen" = 14,
    "fifteen" = 15,
    "sixteen" = 16,
    "seventeen" = 17,
    "eighteen" = 18,
    "nineteen" = 19,
    "twenty" = 20,
    "thirty" = 30,
    "forty" = 40,
    "fifty" = 50,
    "sixty" = 60,
    "seventy" = 70,
    "eighty" = 80,
    "ninety" = 90,
    "hundred" = 100)

# MAGNITUDE REFERENCE NUMBERS

magnitude_reference <- 
  c("thousand" = 1000,
    "million" =  1e6,
    "billion" =  1e9,
    "trillion" = 1e12,
    "quadrillion" = 1e15)