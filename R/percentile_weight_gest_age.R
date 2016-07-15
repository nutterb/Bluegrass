#' @name percentile_weight_gest_age
#' 
#' @title Percentile Weight for Gestational Age
#' @description Reference a birth weight for gestational age to the 
#'   World Health Organization growth tables.
#'   
#' @param weeks Weeks gestational age (as an integer)
#' @param weight Weight in grams
#' @param less Logical.  Determines if the percentile returned should be read as
#'   "less than [percentile]" or "greater than [percentile]"
#' @param direction Logical. Should an inequality symbol be printed with the
#'   result to indicate the direction.  See Examples.
#' @param result A character specifying how the results should be returned.
#'   \code{"ptile"} results in the numeric percentile value being returned; 
#'   \code{"code"} returns the percentile code; and
#'   \code{"desc"} returns the percentile description.  These values are
#'   represented by the \code{percentile}, \code{percentile_code}, and
#'   \code{percentile_desc} fields, respectively, in the \code{GestAgeWtPercentile}
#'   data set.
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accepts the number of weeks gestational age and the fetal weight
#'         in grams as numeric values.
#'   \item Provides options to return output as a numeric value or a 
#'         character value.
#'   \item Use World Health Organization reports for the basis of evaluation.
#' }
#'   
#' @author Benjamin Nutter
#' 
#' @source Data are extracted from the World Health Organization (WHO) website (see link below).  
#'   The link to download the MS Excel sheet is under the "Methodological aspects and other analysis"
#'   and titled "Weight percentiles calculator."
#'   
#'   \url{http://www.who.int/reproductivehealth/topics/maternal_perinatal/globalsurvey/en/}
#'   
#' @examples
#' #* Results can be read as "less than 75th percentile" (for example) 
#' percentile_weight_gest_age(weeks = c(30, 34, 36),
#'                            weight = c(1500, 2500, 2000))
#' 
#' #* Same results, but with directional indicator
#' percentile_weight_gest_age(weeks = c(30, 34, 36),
#'                            weight = c(1500, 2500, 2000),
#'                            direction=TRUE)
#' 
#' #* Same results, but with directional indicator
#' #* and percentile description
#' percentile_weight_gest_age(weeks = c(30, 34, 36),
#'                            weight = c(1500, 2500, 2000),
#'                            direction=TRUE,
#'                            result="desc")
#' 
#' @export

percentile_weight_gest_age <- function(weeks, weight, 
                                less=TRUE, direction=FALSE,
                                result=c("ptile", "code", "desc")){
  coll <- checkmate::makeAssertCollection()
 
  checkmate::assert_integerish(
    x = weeks,
    add = coll
  )
 
  checkmate::assert_numeric(
    x = weight,
    add = coll
  )
  
  checkmate::assert_logical(
    x = less,
    len = 1,
    add = coll
  )
  
  checkmate::assert_logical(
    x = direction,
    len = 1,
    add = coll
  )
  
  result <- 
    checkmate::matchArg(
      x = result,
      choices = c("ptile", "code", "desc"),
      add = coll
    )
  
  checkmate::reportAssertions(coll)
  
  # Convert the result argument into the appropriate suffix to 
  # for extracting the requested result.
  result <- switch(result,
                   "ptile" = "",
                   "code" = "_code",
                   "desc" = "_desc")
  
  mapply(
    match_percentile,
    weeks = weeks,
    weight = weight,
    MoreArgs = list(less = less,
                    direction = direction,
                    result = result),
    SIMPLIFY = FALSE
  ) %>%
   unlist()
}


# Vectorizable function for extracting percentiles
match_percentile <- function(weeks, weight, less, direction, result){
  # Subset the reference data
  ga_ref <- GestAgeWtPercentile[GestAgeWtPercentile$ga == weeks, ]
  
  #* Extract the results in the form of 'less than [percentile]'
  if (less)
  { 
    ptile <- ga_ref[min(which(ga_ref[["weight"]] >= weight)), 
                    paste0("percentile", result)]
   
    if (direction) ptile <- paste0("< ", ptile)
  }
  #* Extract the results in the form of 'greater than [percentile]'
  else{ 
    ptile <- ga_ref[max(which(ga_ref$weight <= weight)), 
                    paste0("percentile", result)]
    if (direction) ptile <- paste0("> ", ptile)
  }
  return(ptile)
}