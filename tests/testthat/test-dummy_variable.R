context("dummy_variable.R")

# Function Requirement 1 --------------------------------------------
test_that("FR 1: Accepts an object that inherits data.frame",
          {
            expect_silent(dummy_variable(data = iris,
                                         x = "Species"))
          })

test_that("FR 2: Casts an error when given a non-data frame object",
          {
            expect_error(dummy_variable(data = as.list(iris),
                                        x = "Species"))
          })

# Function Requirement 2 --------------------------------------------

test_that("FR2: Accepts a character(1) naming a variable in data",
          {
            expect_silent(dummy_variable(data = iris,
                                         x = "Species"))
          })

test_that("FR2: Casts an error when `x` is not a character",
          {
            expect_error(dummy_variable(data = iris,
                                        x = factor("Species")))
          })

test_that("FR2: Casts an error when `x` is not a character",
          {
            expect_error(dummy_variable(data = iris,
                                        x = 3))
          })

test_that("FR2: Casts an error when `x` is a character of length > 1",
          {
            expect_error(dummy_variable(data = iris,
                                        x = c("Species", "Color")))
          })

# Function Requirement 3 --------------------------------------------

test_that("FR3: Casts an error when `x` is not in `data`",
          {
            expect_error(dummy_variable(data = iris,
                                        x = "not_here"))
          })

# Function Requirement 4 --------------------------------------------

test_that("FR4: Accepts a length one variable than names a value in x to use as a reference",
          {
            expect_silent(dummy_variable(data = iris,
                                         x = "Species",
                                         lev = "virginica"))
          })

test_that("FR4: Casts an error if lev has length greater than 1",
          {
            expect_error(dummy_variable(data = iris,
                                        x = "Species",
                                        lev = c("setosa", "virginica")))
          })

# Function Requirement 5 --------------------------------------------

test_that("FR5: Casts an error if `lev` is not a value in `x`",
          {
            expect_error(dummy_variable(iris,
                                        x = "Species",
                                        lev = "not_here"))
          })

# Function Requirement 6 --------------------------------------------

test_that("FR6: Provide an option to prepend the variable name to the front of the level name",
          {
            expect_silent(dummy_variable(data = iris,
                                         x = "Species",
                                         var_name = TRUE))
          })

test_that("FR6: Provide an option to prepend the variable name to the front of the level name",
          {
            DF <- dummy_variable(data = iris,
                                 x = "Species",
                                 var_name = TRUE)
            expect_subset(x = sprintf("Species_%s", 
                                      c("versicolor", "virginica")),
                          choices = names(DF))
          })

# Function Requirement 7 --------------------------------------------

test_that("FR7: Provide an option to return dummy variables as logical values",
          {
            DF <- dummy_variable(data = iris,
                                 x = "Species",
                                 as_logical = TRUE)
            expect_logical(x = DF[["virginica"]])
          })