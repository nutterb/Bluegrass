#' @name gestAge
#' @export gestAge
#' @importFrom stringr str_pad
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
#'   returned.  \code{"both"} returns a character string in the format
#'   \code{"[weeks]/[days]"}; \code{"week"} returns the integer value of the 
#'   weeks component; \code{"day"} returns the integer value of the days 
#'   component.
#'   
#' @details Gestational age is commonly reported as [weeks]/[days] where 
#'   [weeks] is an integer between 0 and 42 (though in some cases is may be 
#'   larger), and days is an integer between 0 and 6.  Thus, 39/2 is 39 weeks 
#'   and 2 days into pregnancy.  The commonly accepted standard length of 
#'   pregnancy is 40 weeks.
#'   
#' @author Benjamin Nutter
#' 
#' @source \url{http://www.acog.org/-/media/Departments/Patient-Safety-and-Quality-Improvement/201213IssuesandRationale-GestationalAgeTerm.pdf }
#' 
#' @examples
#' edc <- as.Date(c("6/18/2010", "10/22/2010"), format="%m/%d/%Y")
#' adc <- as.Date(c("5/22/2010", "10/18/2010"), format="%m/%d/%Y")
#' gestAge(edc, adc, "both")
#' gestAge(edc, adc, "week")
#' gestAge(edc, adc, "day")
#' 

gestAge <- function(edc, adc, result=c("both", "week", "day")){
  result <- match.arg(result, c("both", "week", "day"))
  if (result %in% c("both", "week")){
    week <- floor((280-as.numeric(difftime(edc, adc, units="days")))/7)
  }
  
  if (result %in% c("both", "day")){
    day <- (280 - as.numeric(difftime(edc, adc, units="days"))) %% 7
  }
  
  switch(result,
         "both" = return(paste0(stringr::str_pad(week, 2, pad="0"), "/", day)),
         "week" = return(week),
         "day" = return(day))
}
