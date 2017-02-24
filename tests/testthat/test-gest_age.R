context("gest_age.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Cast an error when edc does not inherit either `Date` or `POSIXct`",
  {
    expect_error(
      gest_age(1, Sys.Date())
    )
  }
)

test_that(
  "FR1: Cast an error when adc does not inherit either `Date` or `POSIXct`",
  {
    expect_error(
      gest_age(Sys.Date(), 1)
    )
  }
)

test_that(
  "FR1: Cast an error when neither edc nor adc inherit from `Date` or `POSIXct`",
  {
    expect_error(
      gest_age(1, 1)
    )
  }
)

test_that(
  "FR1: Succeed when both edc and adc inherit `Date` or `POSIXct`",
  {
    expect_silent(
      gest_age(edc = Sys.Date(), 
               adc = Sys.Date() + 270)
    )
  }
)

test_that(
  "FR1: Succeed when both edc and adc inherit `Date` or `POSIXct`",
  {
    expect_silent(
      gest_age(Sys.time(), Sys.Date() + 270)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

edc <- as.Date("2016-07-10")
adc <- as.Date("2016-07-01")

test_that(
  "FR2: Return the correct number of weeks - result = 'both_string'",
  {
    expect_equal(
      gest_age(edc, adc),
      "38/5"
    )
  }
)

test_that(
  "FR2: Return the correct number of weeks - result = 'both_df'",
  {
    expect_equal(
      gest_age(edc, adc, result = 'both_df'),
      data.frame(week = 38, 
                 day = 5)
    )
  }
)

test_that(
  "FR2: Return the correct number of weeks - result = 'week'",
  {
    expect_equal(
      gest_age(edc, adc, result = 'week'),
      38
    )
  }
)

test_that(
  "FR2: Return the correct number of weeks - result = 'day'",
  {
    expect_equal(
      gest_age(edc, adc, result = 'day'),
      5
    )
  }
)

test_that(
  "FR2: Return the correct number of weeks - result = 'week_decimal'",
  {
    expect_equal(
      gest_age(edc, adc, result = 'week_decimal'),
      38 + 5 / 7
    )
  }
)

test_that(
  "FR2: Return the correct number of weeks - result = 'day_decimal'",
  {
    expect_equal(
      gest_age(edc, adc, result = 'day_decimal'),
      271
    )
  }
)

rm(list = c("edc", "adc"))