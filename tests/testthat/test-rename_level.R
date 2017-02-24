context("rename_level.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Succeed if x is a factor variable",
  {
    expect_silent(
      rename_level(factor(letters), a = 'A')
    )
  }
)

test_that(
  "FR1: Cast an error if x is not a factor variable",
  {
    expect_error(
      rename_level(letters, a = "A")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR2: Cast an error if any element in ... is not named",
  {
    expect_error(
      rename_level(factor(letters), "A")
    )
  }
)

test_that(
  "FR2: Succeed if all elements in ... are named",
  {
    expect_silent(
      rename_level(factor(letters), a = "A", c = "C")
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR3: Send a warning if any name of ... is not a level in x",
  {
    expect_warning(
      rename_level(factor(letters), "A" = "a")
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "FR4: Cast an error if any element of ... is not a character(1)",
  {
    expect_error(
      rename_level(factor(letters), "a" = c("1", "2"))
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "FR5: Allow a label to be given to missing values",
  {
    expect_equal(
      rename_level(factor(c(LETTERS[1:3], NA, LETTERS[4:6])), 
                   A = "hello", 
                   na_val = "sweet", 
                   D = NA),
      factor(c("hello", "B", "C", "sweet", NA, "E", "F"),
             levels = c("hello", "B", "C", "E", "F", "sweet"))
    )
  }
)

test_that(
  "FR5: Cast an error if na_val is not a character(1)",
  {
    expect_error(
      rename_level(factor(letters[1:5]), 
                   na_val = c("yes", "no"))
    )
  }
)

# Check the output --------------------------------------------------

test_that(
  "Confirm the output matches expectation",
  {
    expect_equal(
      rename_level(factor(rep(letters[1:5], 2)),
                   a = "Q",
                   d = "F"),
      factor(c("Q", "b", "c", "F", "e", 
               "Q", "b", "c", "F", "e"),
             levels = c("Q", "b", "c", "F", "e"))
    )
  }
)