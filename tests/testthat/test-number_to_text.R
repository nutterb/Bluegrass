context("number_to_text.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "return a character vector the same length as x",
  {
    expect_equal(
      length(number_to_text(1:13)),
      13
    )
  }
)

test_that(
  "return a character vector the same length as x",
  {
    expect_true(is.character(number_to_text(1:13)))
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not integerish",
  {
    expect_error(
      number_to_text(pi)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast a warning if any value in x is larger than 9,999,999,999,998",
  {
    expect_warning(
      number_to_text(9999999999999999),
      "No promises"
    )
  }
)