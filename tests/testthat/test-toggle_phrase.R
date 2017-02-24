context("toggle_plural.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Return an error if x is not numeric",
  {
    expect_error(
      toggle_plural("1", "is", "were")
    )
  }
)

test_that(
  "FR1: Function succeeds when x is numeric",
  {
    expect_silent(
      toggle_plural(1, "is", "were")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR2: Return an error if singular is not a character",
  {
    expect_error(
      toggle_plural(1, 1, "were")
    )
  }
)

test_that(
  "FR2: Return an error if singular is not a character(1)",
  {
    expect_error(
      toggle_plural(1, c("is", "is"), "were")
    )
  }
)

test_that(
  "FR2: Return an error if plural is not a character",
  {
    expect_error(
      toggle_plural(1, "were", 1)
    )
  }
)

test_that(
  "FR2: Return an error if plural is not a character(1)",
  {
    expect_error(
      toggle_plural(1, "is", c("were", "were"))
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR3: Return singular if x is 1",
  {
    expect_equal(
      toggle_plural(1, "is", "were"),
      "is 1"
    )
  }
)

test_that(
  "FR3: Return plural if x is not 1",
  {
    expect_equal(
      toggle_plural(0, "is", "were"),
      "were 0"
    )
  }
)

test_that(
  "FR3: Return plural if x is not 1",
  {
    expect_equal(
      toggle_plural(3, "is", "were"),
      "were 3"
    )
  }
)
