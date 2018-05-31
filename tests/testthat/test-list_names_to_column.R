context("list_names_to_columns.R")

x <- split(mtcars, 
           mtcars[c("gear")])

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a list of data frames.",
  {
    expect_list(list_names_to_column(x, "group"),
                names = "named",
                types = "data.frame")
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Each data frame in the list has a new column named by new_col",
  {
    has_new_col <- 
      vapply(list_names_to_column(x, "group"),
             function(x) "group" %in% names(x),
             logical(1))
    expect_true(all(has_new_col))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if list_data is not a named list of data frames.",
  {
    expect_error(list_names_to_column(mtcars))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if new_col is not a character(1)",
  {
    expect_error(list_names_to_column(x, c("group", "column")))
  }
)

test_that(
  "Cast an error if new_col is not a character(1)",
  {
    expect_error(list_names_to_column(x, TRUE))
  }
)


# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if new_col exists in any element of list_data.",
  {
    expect_error(
      list_names_to_column(x, "gear")
    )
  }
)
