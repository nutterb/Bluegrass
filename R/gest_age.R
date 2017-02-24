#' @name gest_age
#' 
#' @title Calculate Gestational Age
#' @description Calculate the gestational age of a child or fetus based on the
#'   estimated date of confinement (or estimated date of delivery).
#'   
#' @param edc The estimated date of confinement (sometimes called the estimated
#'   date of delivery).  Must be an object suitable 
#'   for the \code{difftime} function (such as \code{Date}, \code{POSIXct},
#'   etc.) or the calculation will fail.  
#' @param adc The date for which the gestational age needs to be calcualted.  
#'   This may be the current date, the date of a prenatal care visit, or the
#'   actual date of delivery.  As with \code{edc}, this must be an object
#'   suitable for \code{difftime}.
#' @param result A character option that determines the nature of the result
#'   returned.  See Details.
#'   
#' @details Gestational age is commonly reported as [weeks]/[days] where 
#'   [weeks] is an integer between 0 and 42 (though in some cases is may be 
#'   larger), and days is an integer between 0 and 6.  Thus, 39/2 is 39 weeks 
#'   and 2 days into pregnancy.  The commonly accepted standard length of 
#'   pregnancy is 40 weeks.
#'   
#'   The \code{result} argument controls how the output is reported to the 
#'   user with the following options.
#'   \enumerate{
#'     \item{both_string }{A character string in the format "[week]/[day]"}
#'     \item{both_df }{A data frame with a column for week and a column for 
#'                     day}
#'     \item{week }{A numeric value of the weeks gestational age}
#'     \item{day }{A numeric value of the day component of the gestational
#'                 age (an integer between 0 and 6)}
#'     \item{week_decimal }{A numeric value of the total weeks gestational
#'                          age (\code{week + (day / 7)})}
#'     \item{day_decimal }{A numeric value of the total days gestational 
#'                         age (\code{week * 7 + day})}
#'   }
#'   
#' @author Benjamin Nutter
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accept two date-like objects to calculate gestational age.
#'   \item Return the number of weeks, the number of days within the week,
#'         both, the number of decimal weeks, or the number of decimal days.
#' }
#' 
#' @source \url{http://www.acog.org/-/media/Departments/Patient-Safety-and-Quality-Improvement/201213IssuesandRationale-GestationalAgeTerm.pdf }
#' 
#' @examples
#' edc <- as.Date(c("6/18/2010", "10/22/2010"), format="%m/%d/%Y")
#' adc <- as.Date(c("5/22/2010", "10/18/2010"), format="%m/%d/%Y")
#' gest_age(edc, adc, "both_string")
#' gest_age(edc, adc, "week")
#' gest_age(edc, adc, "day")
#' 
#' @export

gest_age <- function(edc, adc, 
                    result = c("both_string", "both_df", 
                               "week", "day", 
                               "week_decimal", "day_decimal"))
{
# Argument Validation -----------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  result <- 
    checkmate::matchArg(
      x = result,
      choices = c("both_string", "both_df", 
                  "week", "day",
                  "week_decimal", "day_decimal"),
      several.ok = FALSE,
      add = coll
    )
  
  if (!inherits(edc, "Date") & !inherits(edc, "POSIXct"))
  {
    coll$push("'edc' must be of class 'Date' or 'POSIXct'")
  }
  
  if (!inherits(adc, "Date") & !inherits(adc, "POSIXct"))
  {
    coll$push("'adc' must be of class 'Date' or 'POSIXct'")
  }
  
  checkmate::reportAssertions(coll)

# Functional Code ---------------------------------------------------
  
  #* both week and day are based on the days difference between
  #* edc and adc.  Calculate it here once.
  base <- difftime(time1 = edc, 
                   time2 = adc, 
                   units = "days")
  base <- as.numeric(base)
  base <- 280 - base
  
  week <- floor(base / 7)

  day <- base %% 7
  
  switch(
    result,
    "both_string" = sprintf("%s/%s",
                            sprintf("%02d", week),
                            day),
    "both_df" = data.frame(week = week, 
                           day = day),
    "week" = week,
    "day" = day,
    "week_decimal" = week + (day / 7),
    "day_decimal" = week * 7 + day
  )
}
