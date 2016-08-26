context("factor_reference")

mtcars2 <- 
  transform(mtcars,
            am = factor(am),
            gear = factor(gear),
            cyl = factor(cyl))

test_that(
  "FR #1: Accepts a data.frame",
  {
    factor_reference(mtcars2) %>%
      expect_silent()
  }
)

test_that(
  "FR #1: Accepts a data.table",
  {
    data.table::as.data.table(mtcars2) %>%
      factor_reference() %>%
      expect_silent()
  }
)

test_that(
  "FR #1: Accepts a tibble",
  {
    tibble::as_tibble(mtcars2) %>%
      factor_reference() %>%
      expect_silent()
  }
)

test_that(
  "FR #1: Accepts a grouped_df",
  {
    dplyr::group_by(mtcars2, am) %>%
      factor_reference() %>%
      expect_silent()
  }
)

test_that(
  "FR #2: Returns a list",
  {
    factor_reference(mtcars2) %>%
    class %>%
    expect_equal("list")
  }
)

test_that(
  "FR #2: Each element in the list is a data.frame",
  {
    factor_reference(mtcars2) %>%
    vapply(X = .,
           FUN = class,
           FUN.VALUE = character(1)) %>%
    expect_equivalent(rep("data.frame", 3))
  }
)

test_that(
  "FR #3: Return a message if no factors are in `data`",
  {
    expect_message(
      factor_reference(mtcars)
    )
  }
)