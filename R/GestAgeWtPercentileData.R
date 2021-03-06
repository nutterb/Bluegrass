#' Percentile Weights for Gestational Age.
#'
#' A reference table for evaluating birth weights for gestational age.
#'
#' @format A data frame with 198 rows and 5 variables:
#' \describe{
#'   \item{ga}{Gestational age in weeks}
#'   \item{percentile}{A numeric value for the percentile, mainly used for sorting}
#'   \item{percentile_code}{A character value appropriate for use in \code{plyr::dcast}}
#'   \item{percentile_desc}{A character description of the percentile appropriate for printing}
#'   \item{weight}{The weight marking the designated percentile at that gestational age}
#' }
#' @source Data are extracted from the World Health Organization (WHO) website (see link below).  
#'   The link to download the MS Excel sheet is under the "Methodological aspects and other analysis"
#'   and titled "Weight percentiles calculator."
#'   
#'   \url{http://www.who.int/reproductivehealth/topics/maternal_perinatal/globalsurvey/en/}
"GestAgeWtPercentile"