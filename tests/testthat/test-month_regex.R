context("month_regex.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Return a regular expression that will match full month names",
  {
    expect_equal(
      month_regex(),
      "(January|February|March|April|May|June|July|August|September|October|November|December)"
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR1: Return a regular expression that will match abbreviated month names",
  {
    expect_equal(
      month_regex(abbreviated = TRUE),
      "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
    )
  }
)