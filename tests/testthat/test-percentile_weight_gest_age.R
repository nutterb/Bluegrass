context("percentile_weight_gest_age.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Cast an error if weeks is not integerish",
  {
    expect_error(
      percentile_weight_gest_age(weeks = c("30", "34", "36"),
                                 weight = c(1500, 2500, 2000))
    )
  }
)

test_that(
  "FR1: Cast an error if weight is not integerish",
  {
    expect_error(
      percentile_weight_gest_age(weeks = c(30, 34, 36),
                                 weight = c("1500", "2500", "2000"))
    )
  }
)

test_that(
  "FR1: Succeed when weight and weeks are integerish",
  {
    expect_silent(
      percentile_weight_gest_age(weeks = c(30, 34, 36),
                                 weight = c(1500, 2500, 2000))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR2: Return a numeric vector when result = ptile",
  {
    expect_numeric(
      percentile_weight_gest_age(weeks = c(30, 34, 36),
                                 weight = c(1500, 2500, 2000))
    )
  }
)

test_that(
  "FR2: Return a character vector when result = code",
  {
    expect_character(
      percentile_weight_gest_age(weeks = c(30, 34, 36),
                                 weight = c(1500, 2500, 2000),
                                 result = "code")
    )
  }
)

test_that(
  "FR2: Return a numeric vector when result = desc",
  {
    expect_character(
      percentile_weight_gest_age(weeks = c(30, 34, 36),
                                 weight = c(1500, 2500, 2000),
                                 result = "desc")
    )
  }
)

test_that(
  "FR3: cast an error when result is not an allowable option",
  {
    expect_error(
      percentile_weight_gest_age(weeks = c(30, 34, 36),
                                 weight = c(1500, 2500, 2000),
                                 result = "something wrong")
    )
  }
)

# Other argument validations ----------------------------------------

test_that(
  "cast an error when less is not logical",
  {
    expect_error(
      percentile_weight_gest_age(weeks = c(30, 34, 36),
                                 weight = c(1500, 2500, 2000),
                                 less = "true")
    )
  }
)

test_that(
  "function succeeds when less is logical",
  {
    expect_silent(
      percentile_weight_gest_age(weeks = c(30, 34, 36),
                                 weight = c(1500, 2500, 2000),
                                 less = FALSE)
    )
  }
)

test_that(
  "cast an error when direction is not logical",
  {
    expect_error(
      percentile_weight_gest_age(weeks = c(30, 34, 36),
                                 weight = c(1500, 2500, 2000),
                                 direction = "true")
    )
  }
)

test_that(
  "function succeeds when direction is logical",
  {
    expect_silent(
      percentile_weight_gest_age(weeks = c(30, 34, 36),
                                 weight = c(1500, 2500, 2000),
                                 direction = TRUE)
    )
  }
)