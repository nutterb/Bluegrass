context("is_equal.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a logical vector when x and y are numeric vectors",
  {
    expect_logical(
      is_equal(1:3, 1:3)
    )
  }
)

test_that(
  "Return a logical vector when x and y are character vectors",
  {
    expect_logical(
      is_equal(letters, letters)
    )
  }
)

test_that(
  "Return a logical vector when x and y are logical vectors",
  {
    expect_logical(
      is_equal(c(TRUE, FALSE, TRUE, FALSE), 
               c(TRUE, FALSE, FALSE, TRUE))
    )
  }
)

test_that(
  "Return a logical vector when x and y are Date vectors",
  {
    expect_logical(
      is_equal(Sys.Date() + 0:1, Sys.Date() + c(0, 2))
    )
  }
)

test_that(
  "Return a logical vector when x and y are POSIXct vectors",
  {
    expect_logical(
      is_equal(Sys.time() + 0:1, Sys.time() + c(0, 2))
    )
  }
)

test_that(
  "Return a logical vector when x and y are factor vectors",
  {
    expect_logical(
      is_equal(factor(letters), factor(letters))
    )
  }
)

test_that(
  "Cast an error if x is not a vector",
  {
    expect_error(
      is_equal(mtcars, 1:3)
    )
  }
)

test_that(
  "Cast an error if y is not a vector",
  {
    expect_error(
      is_equal(1:3, mtcars)
    )
  }
)

test_that(
  "Cast an error if both x and y are not vectors",
  {
    expect_error(
      is_equal(list(1:3), list(1:3))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x and y do not have the same length",
  {
    expect_error(
      is_equal(1:2, 1:4)
    )
  }
)

# Functional Requirement 3 ------------------------------------------
## This is not extensively tested.  Given that `mapply` is used 
## to do the pairwise comparison, it would be remarkable if the
## length did not match up.  This could only happen if `NULL` 
## were somehow returned. 

test_that(
  "Return a logical vector the length of x",
  {
    expect_true(
      length(is_equal(1:3, 1:3)) == 3
    )
  }
)

test_that(
  "Return a logical vector when x and y are character vectors",
  {
    expect_true(
      length(is_equal(letters, letters)) == 26
    )
  }
)

## If an element in a vector is NULL, the length of that vector is decreased.  
## Hopefully, this triggers a length error in most cases.  Cases where 
## the reduction in length causes x and y to be of similar length could
## be problematic, but we assume that is a failure in the creation of the
## vector.
test_that(
  "",
  {
    expect_error(
      is_equal(c(1, NULL, 2),
               c(1, 3, 2))
    )
  }
)


# Functional Requirement 4 ------------------------------------------

test_that(
  "Return NA when `compare_na = FALSE`",
  {
    expect_equal(
      is_equal(c(1, NA, 2),
               c(1, 3, 2)),
      c(TRUE, NA, TRUE)
    )
  }
)

test_that(
  "Return logical when `compare_na = TRUE",
  {
    expect_equal(
      is_equal(c(1, NA, 2),
               c(1, 3, 2),
               compare_na = TRUE),
      c(TRUE, FALSE, TRUE)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast a warning if x and y do not share a class",
  {
    expect_warning(
      is_equal(1:3,
               factor(1:3))
    )
  }
)

# Additional Tests --------------------------------------------------

test_that(
  "Cast an error with compare_na is not logical",
  {
    expect_error(
      is_equal(1:3, 1:3, compare_na = 1)
    )
  }
)

test_that(
  "Cast an error when compare_na has length greater than 1",
  {
    expect_error(
      is_equal(1:3, 1:3, compare_na = c(TRUE, FALSE, TRUE))
    )
  }
)

test_that(
  "Cast an error with use_names is not logical",
  {
    expect_error(
      is_equal(1:3, 1:3, use_names = 1)
    )
  }
)

test_that(
  "Cast an error when use_names has length greater than 1",
  {
    expect_error(
      is_equal(1:3, 1:3, use_names = c(TRUE, FALSE, TRUE))
    )
  }
)