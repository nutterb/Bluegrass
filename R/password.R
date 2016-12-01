#' @name password
#' @title Random Password Generation
#' 
#' @description Generates character strings for use as passwords.  
#' 
#' @param n \code{numeric(1)}, number of characters in the desired password.
#' @param digits \code{logical(1)}, should the digits \code{0:9} be included
#'   in the character pool?
#' @param upperalpha \code{logical(1)}, should the upper case alphabet 
#'   \code{LETTERS} be included in the character pool.
#' @param loweralpha \code{logical(1)}, should the lower case alphabet
#'   \code{letters} be included in the character pool.
#' @param special A character vector of special characters to include in the 
#'   character pool.  If special characters should not be considered, provide
#'   either \code{character(0)} or \code{NULL}.
#' @param replace \code{logical(1)}, should sampling from the character pool 
#'   be done with replacement?
#' @param digit_first_allowed \code{logical(1)}, is it allowable for the first
#'   character in the password to be a digit?
#' @param one_from_each \code{logical(1)}, is it mandatory that at least one 
#'   from each character set be selected?
#'   
#' @export

password <- function(n = 10, digits = TRUE, upperalpha = TRUE,
                     loweralpha = TRUE, 
                     special = c("!", "@", "#", "$", "%", "^", "&", "*", "?", "-", "+"),
                     replace = TRUE,
                     digit_first_allowed = TRUE,
                     one_from_each = TRUE)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = n,
                               len = 1,
                               add = coll)
  
  checkmate::assert_logical(x = digits,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = upperalpha,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = loweralpha,
                            len = 1,
                            add = coll)
  
  if (!is.null(special))
  {
    checkmate::assert_character(x = special,
                                add = coll)
  }
  
  checkmate::assert_logical(x = replace,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = digit_first_allowed,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = one_from_each,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  #******************************************************************
  #* 1. If one from each is required, draw one from each of the 
  #*    requested character types.
  #* 2. Develop the character pool
  #* 3. If sampling is to be done without replacement, drop the 
  #*    characters selected in 1. from the pool
  #* 4. Sample the pool for any remaining characters needed
  #* 5. Select a character from those selected to be the first character
  #*    The first is pulled separately to permit the user to decide
  #*    if a digit may be used in the first position.
  #* 6. Randomly order the remaining characters
  #* 7. Combine the characters in order and paste into a single string
  #******************************************************************
  
  #* 1. If one from each is required, draw one from each of the 
  #*    requested character types.
  req <- 
    if (one_from_each)
    {
      mapply(FUN = function(l, x) if (l) sample(x, size = 1) else character(0),
             l = list(upperalpha, loweralpha, digits, length(special)),
             x = list(LETTERS,    letters,    0:9,    special),
             SIMPLIFY = FALSE) %>%
        unlist()
    }
    else
    {
      character(0)
    }

  #* 2. Develop the character pool
  char_pool <- 
    c(if (upperalpha) LETTERS else character(0),
      if (loweralpha) letters else character(0),
      if (digits) 0:9 else character(0),
      special) %>% 
    unique
  
  #* 3. If sampling is to be done without replacement, drop the 
  #*    characters selected in 1. from the pool
  if (!replace)
  {
    char_pool <- char_pool[!char_pool %in% req]
  }
  
  #* 4. Sample the pool for any remaining characters needed
  selected_char <- 
    sample(char_pool, 
           size = n - length(req),
           replace = replace) %>%
    c(., req)
  
  #* 5. Select a character from those selected to be the first character
  #*    The first is pulled separately to permit the user to decide
  #*    if a digit may be used in the first position.
  first_char <- 
    if (digit_first_allowed)
    {
      sample(selected_char, 
             size = 1)
    }
    else
    {
      sample(selected_char[!selected_char %in% 0:9],
             size = 1)
    }
  
  #* 6. Randomly order the remaining characters
  remainder_char <- 
    sample(selected_char[-min(which(selected_char %in% first_char))],
           size = n - 1)
  
  #* 7. Combine the characters in order and paste into a single string
  c(first_char, remainder_char) %>%
    paste(collapse = "")
}
