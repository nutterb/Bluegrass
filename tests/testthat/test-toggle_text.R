context("toggle_text.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Return an error if condition does not resolve to logical",
  {
    expect_error(
      toggle_text("true", "yes", "no")
    )
  }
)

test_that(
  "FR1: Return an error if condition does not resolve to logical(1)",
  {
    expect_error(
      toggle_text(c(TRUE, FALSE), "yes", "no")
    )
  }
)

test_that(
  "FR1: Function succeeds if condition resolves to logical(1)",
  {
    expect_silent(
      toggle_text(TRUE, "yes", "no")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR2: Return an error if true is not a character string",
  {
    expect_error(
      toggle_text(TRUE, 1, "no")
    )
  }
)

test_that(
  "FR2: Return an error if true is not a character(1)",
  {
    expect_error(
      toggle_text(TRUE, c("yes", "yes"), "no")
    )
  }
)

test_that(
  "FR2: Succeed if true is a character(1)",
  {
    expect_silent(
      toggle_text(TRUE, "yes", "no")
    )
  }
)

test_that(
  "FR2: Return an error if false is not a character string",
  {
    expect_error(
      toggle_text(FALSE, "yes", 1)
    )
  }
)

test_that(
  "FR2: Return an error if false is not a character(1)",
  {
    expect_error(
      toggle_text(FALSE, "yes", c("no", "no"))
    )
  }
)

test_that(
  "FR2: Succeed if true is a character(1)",
  {
    expect_silent(
      toggle_text(FALSE, "yes", "no")
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR3: Return the value of true when condition resolves to true",
  {
    expect_equal(
      toggle_text(TRUE, "yes", "no"),
      "yes"
    )
  }
)

test_that(
  "FR3: Return the value of false when condition resolves to false",
  {
    expect_equal(
      toggle_text(FALSE, "yes", "no"),
      "no"
    )
  }
)