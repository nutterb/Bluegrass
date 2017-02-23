context("labels_template.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Accept an object that inherits class data.frame",
  {
    expect_output(
      labels_template(mtcars)
    )
  }
)

test_that(
  "FR1: Cast en error when `data` does not inherit data.frame",
  {
    expect_error(
      labels_template(as.matrix(mtcars))
    )
  }
)

# Functional Requirements 2-4 ---------------------------------------

# functional requirements 2 - 4 cannot be directly tested because 
# the output is printed via `cat`. Rather than fight the output,
# I'm satisfied that these options work as intended and will leave these
# untested unless a problem develops.