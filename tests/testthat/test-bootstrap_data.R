context("bootstrap_data.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Accept an object of class data.frame",
  {
    expect_silent(
      bootstrap_data(data = mtcars,
                     B = 10)
    )
  }
)

test_that(
  "FR1: Accept an object of class data.table",
  {
    expect_silent(
      data.table::as.data.table(mtcars) %>%
        bootstrap_data(B = 10)
    )
  }
)

test_that(
  "FR1: Accept an object of class tbl_df",
  {
    expect_silent(
      tibble::as_data_frame(mtcars) %>%
        bootstrap_data(B = 10)
    )
  }
)

test_that(
  "FR1: Return an error if the object does not inherit data.frame",
  {
    expect_error(
      as.matrix(mtcars) %>%
        bootstrap_data(B = 10)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR2: When given a data.frame, return a list of data.frame",
  {
    expect_true(
      bootstrap_data(mtcars, 
                     B = 10) %>%
        vapply(FUN = inherits, 
               FUN.VALUE = logical(1),
               "data.frame") %>%
        all()
    )
  }
)

test_that(
  "FR2: When given a data.table, return a list of data.table",
  {
    expect_true(
      bootstrap_data(data.table::as.data.table(mtcars), 
                     B = 10) %>%
        vapply(FUN = inherits, 
               FUN.VALUE = logical(1),
               "data.table") %>%
        all()
    )
  }
)

test_that(
  "FR2: When given a tbl_df, return a list of tbl_df",
  {
    expect_true(
      bootstrap_data(tibble::as_data_frame(mtcars), 
                     B = 10) %>%
        vapply(FUN = inherits, 
               FUN.VALUE = logical(1),
               "tbl_df") %>%
        all()
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR3: Returns a list of length B",
  {
    set.seed(13)
    B_list <- sample(1:100, 10)
    
    lapply(B_list,
           function(B, data){bootstrap_data(data, B = B)},
           mtcars) %>%
      vapply(length,
             FUN.VALUE = numeric(1)) %>%
      expect_equal(B_list)
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "FR4: cast an error when `cl` does not inherit `cluster`",
  {
    expect_error(
      bootstrap_data(mtcars, cl = 3)
    )
  }
)

test_that(
  "FR4: accept `cl` when it inherits `cluster`",
  {
    expect_silent({
      cl <- parallel::makeCluster(1)
      parallel::clusterEvalQ(cl, library(Bluegrass))
      bootstrap_data(mtcars, B = 10, cl = cl)
      parallel::stopCluster(cl)
    })
  }
)