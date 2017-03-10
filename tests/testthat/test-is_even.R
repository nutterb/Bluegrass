context("is_even.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a vector the same length as x",
  {
    x <- 1:10
    expect_logical(
      x = is_even(x),
      len = length(x)
    )
  }
)

test_that(
  "Return a vector the same length as x",
  {
    x <- c(1:10, NULL, 11:20)
    expect_logical(
      x = is_even(x),
      len = length(x)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error when x is a non-numeric vector",
  {
    expect_error(
      is_even(letters)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error when x is not a vector",
  {
    expect_error(
      is_even(mtcars)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Return a logical vector for non-integer-like values when require_int is FALSE",
  {
    expect_logical(
      x = is_even(1:10 + 0.15),
      len = 10
    )
  }
)

test_that(
  "Cast an error when x contains non-integer-like values when require_int is TRUE",
  {
    expect_error(
      is_even(1:10 + .15, require_int = TRUE)
    )
  }
)