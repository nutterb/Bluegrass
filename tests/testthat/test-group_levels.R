context("group_levels.R")

DF <- data.frame(Members = c("Eva",  "Charlie1", "Fred",     "Charlie2",
                             "Adam", "Eva",      "Charlie2", "David",
                             "Adam", "David",    "Charlie1"))

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Return an object of class factor",
  {
    expect_true(
      group_levels(DF$Members,
                   Group1 = c("Eva", "Adam"),
                   Group2 = c("Charlie1", "Charlie2"),
                   Group3 = NULL) %>%
        is.factor()
    )
  }
)

test_that(
  "FR1: Return an object with the same length as x",
  {
    expect_true(
      group_levels(DF$Members,
                   Group1 = c("Eva", "Adam"),
                   Group2 = c("Charlie1", "Charlie2"),
                   Group3 = NULL) %>%
        length() %>%
        magrittr::equals(length(DF$Members))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR2: cast an error if an argument to ... is not a vector",
  {
    expect_error(
      group_levels(DF$Members,
                   mtcars)
    )
  }
)

test_that(
  "FR2: cast an error if an argument is not named",
  {
    expect_error(
      group_levels(DF$Members,
                   c("Eva", "Adam"))
    )
  }
)

test_that(
  "FR2: cast an error if no vectors given to `...`",
  {
    expect_error(
      group_levels(DF$Members)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR3: function succeeds with no NULL value in ...",
  {
    expect_equal(
      group_levels(DF$Members,
                   Group1 = c("Eva", "Adam"),
                   Group2 = c("Charlie1", "Charlie2")),
      structure(c(1L, 2L, NA, 2L, 1L, 1L, 2L, NA, 1L, NA, 2L), 
                .Label = c("Group1", "Group2"), 
                class = "factor")
    )
  }
)

test_that(
  "FR3: function succeeds with one NULL value in ...",
  {
    expect_equal(
      group_levels(DF$Members,
                   Group1 = c("Eva", "Adam"),
                   Group2 = c("Charlie1", "Charlie2"),
                   Group3 = NULL),
      structure(c(1L, 2L, 3L, 2L, 1L, 1L, 2L, 3L, 1L, 3L, 2L), 
                .Label = c("Group1", "Group2", "Group3"), 
                class = "factor")
    )
  }
)

test_that(
  "FR3: cast an error when more than one NULL value in ...",
  {
    expect_error(
      group_levels(DF$Members,
                   Group1 = c("Eva", "Adam"),
                   Group2 = NULL,
                   Group3 = NULL)
    )
  }
)

rm(list = "DF")