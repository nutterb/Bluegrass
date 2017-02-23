context("password.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: User may designate the length of the password",
  {
    lapply(4:10,
           password) %>%
      vapply(nchar,
             FUN.VALUE = numeric(1)) %>%
      expect_equal(4:10)
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR2: User may exclude digits from the character pool",
  {
    password(100, digits = FALSE) %>%
      grepl(pattern = "[0-9]", .) %>%
      expect_false()
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR3: User may exclude lower case character from the character pool",
  {
    password(100, loweralpha = FALSE) %>%
      grepl(patter = "[a-z]", .) %>%
      expect_false()
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "FR4: User may exclude upper case character from the character pool",
  {
    password(100, upperalpha = FALSE) %>%
      grepl(patter = "[A-Z]", .) %>%
      expect_false()
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "FR5: User may include special characters in the character pool",
  {
    special <- c("!", "@", "#", "$", "%", "^", "&", "*", "?", "-", "+")
    vapply(special,
           grepl,
           password(100, special = special),
           FUN.VALUE = logical(1)) %>%
      any %>%
      expect_true
  }
)

test_that(
  "FR5: User may include special characters in the character pool",
  {
    special <- c("!", "@", "#", "$", "-", "+")
    vapply(special,
           grepl,
           password(100, special = special),
           FUN.VALUE = logical(1)) %>%
      any %>%
      expect_true
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "FR6: Sampling may be done without replacement",
  {
    c(0:9, letters, LETTERS) %>%
      vapply(grepl,
             password(62, 
                      special = NULL, 
                      replace = FALSE),
             FUN.VALUE = logical(1)) %>%
      all %>%
      expect_true()
  }
)

test_that(
  "FR6: Sampling may be done with replacement",
  {
    c(0:9, letters, LETTERS) %>%
      vapply(grepl,
             password(62, 
                      special = NULL, 
                      replace = TRUE),
             FUN.VALUE = logical(1)) %>%
      all %>%
      expect_false()
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "FR7: User may disallow the first character from being a digit",
  {
    lapply(1:1000,
           function(i) password(10, digit_first_allowed = FALSE)) %>%
      vapply(substr,
             FUN.VALUE = character(1),
             start = 1,
             stop = 1) %>%
      grepl("[0-9]", .) %>%
      any() %>%
      expect_false
  }
)

test_that(
  "FR7: User may allow the first character to be a digit",
  {
    lapply(1:1000,
           function(i) password(10, digit_first_allowed = TRUE)) %>%
      vapply(substr,
             FUN.VALUE = character(1),
             start = 1,
             stop = 1) %>%
      grepl("[0-9]", .) %>%
      any() %>%
      expect_true()
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "FR8: The user may require that one value from each character set be included",
  {
    p <- password(4, 
                  one_from_each = TRUE,
                  special = "#")
    expect_true(grepl("[A-Z]", p) & 
                  grepl("[a-z]", p) & 
                  grepl("[0-9]", p) & 
                  grepl("#", p))
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "FR9: the output is a character(1)",
  {
    lapply(1:15, 
           function(i) password()) %>%
      vapply(FUN = test_character,
             len = 1,
             FUN.VALUE = logical(1)) %>%
      all %>%
      expect_true()
  }
)

