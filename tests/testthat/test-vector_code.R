context("vector_code.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Accept a logical vector",
  {
    expect_output(
      vector_code(c(TRUE, FALSE))
    )
  }
)

test_that(
  "FR1: Accept a character vector",
  {
    expect_output(
      vector_code(letters)
    )
  }
)

test_that(
  "FR1: Accept a numeric vector",
  {
    expect_output(
      vector_code(0:9)
    )
  }
)

test_that(
  "FR1: Accept a factor vector",
  {
    expect_output(
      vector_code(factor(letters))
    )
  }
)

test_that(
  "FR1: Cast an error when given a non-atomic object",
  {
    expect_error(
      vector_code(list(letters))
    )
  }
)

test_that(
  "FR1: Cast an error when given NULL",
  {
    expect_error(
      vector_code(NULL)
    )
  }
)

# Functional Requirements 2 and 3 -----------------------------------

## These requirements remain untested.




    