context("show_duplicated.R")

DF <- data.frame(
  id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  week = c(1, 1, 2, 1, 2, 2, 1, 2, 3),
  obs = rep(letters[1:3], 3)
)

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Accept an object of class data.frame",
  {
    expect_silent(
      show_duplicated(DF,
                      key = c("id", "week"))
    )
  }
)

test_that(
  "FR1: Accept an object of class data.table",
  {
    expect_silent(
      show_duplicated(data.table::as.data.table(DF),
                      key = c("id", "week"))
    )
  }
)

test_that(
  "FR1: Accept an object of class tbl_df",
  {
    expect_silent(
      show_duplicated(tibble::as_data_frame(DF),
                      key = c("id", "week"))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR2: Return an object of class data.frame when given a data.frame",
  {
    expect_class(
      show_duplicated(DF,
                      key = c("id", "week")),
      "data.frame"
    )
  }
)

test_that(
  "FR2: Return an object of class data.table when given a data.table",
  {
    expect_class(
      show_duplicated(data.table::as.data.table(DF),
                      key = c("id", "week")),
      "data.table"
    )
  }
)

test_that(
  "FR2: Return an object of class tbl_df when given a tbl_df",
  {
    expect_class(
      show_duplicated(tibble::as_data_frame(DF),
                      key = c("id", "week")),
      "tbl_df"
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR3: Correctly identify duplicated rows",
  {
    expect_equal(
      show_duplicated(DF,
                      key = c("id", "week")),
      structure(list(id = c(1, 1, 2, 2), 
                     week = c(1, 1, 2, 2), 
                     obs = structure(c(1L, 2L, 2L, 3L), 
                                     .Label = c("a", "b", "c"), 
                                     class = "factor")), 
                .Names = c("id", "week", "obs"), 
                row.names = c(1L, 2L, 5L, 6L), 
                class = "data.frame")
    )
  }
)

# Additional Argument Validations -----------------------------------

test_that(
  "Cast an error if data does not inherit data.frame",
  {
    expect_error(
      show_duplicated(as.list(mtcars),
                      key = c("id", "week"))
    )
  }
)

test_that(
  "Cast an error if key is not a character vector",
  {
    expect_error(
      show_duplicated(mtcars,
                      1:2)
    )
  }
)

test_that(
  "Cast an error if key contains a name that doesn't exist in data",
  {
    expect_error(
      show_duplicated(mtcars,
                      c("ID", "WEEK"))
    )
  }
)