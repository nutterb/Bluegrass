#' @name bootstrap_data
#' @title Lists of Bootstrapped Data Frames
#' 
#' @description Bootstrapping data frames is a critical part of developing 
#'   bootstrapped estimates from models.  This function provides a basic 
#'   interface to generate the bootstrapped data to feed to a modeling function.
#'
#' @param data The data frame to be bootstrapped
#' @param groups A character vector giving the names of the columns in the data
#'   frame that define groups or clusters that should be sampled together.
#'   Use \code{character(0)} or \code{NULL} for each row as a distinct entity.
#' @param B \code{numeric(1)}, an integer-ish value giving the number of
#'   data frames to return.
#' @param seed A numeric value with which the random number generator may 
#'   be seeded.  Using \code{character{0}} or \code{NULL} causes the 
#'   function to behave off of whatever exists in the environment at present.
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accepts objects that inherits the \code{data.frame} class.
#'   \item Objects returned have the same class as what was input.
#'   \item Returns a list of length \code{B}
#'   \item When \code{groups} has length, each data set returned has the same
#'     number of groups as was in the original data.
#'   \item cl An object that inherits the \code{cluster} class. Used for
#'     parallel computation. 
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @export

bootstrap_data <- function(data, groups = character(0), 
                           B = 40, seed = numeric(0),
                           cl = NULL)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = data, 
                          classes = "data.frame",
                          add = coll)
  
  if (!is.null(groups))
  {
    checkmate::assert_character(x = groups,
                                add = coll)
  }
  
  checkmate::assert_integerish(x = B,
                               lower = 0,
                               len = 1,
                               add = coll)
  
  if (!is.null(seed))
  {
    checkmate::assert_numeric(x = seed,
                              max.len = 1,
                              add = coll)
  }
  
  if (!is.null(cl))
  {
    checkmate::assert_class(x = cl,
                            classes = "cluster",
                            add = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  
  
  if (length(seed)) set.seed(seed)
  
  if (length(groups))
  {
    if (is.null(cl))
    {
      lapply(X = 1:B,
             FUN = function(i, d, g) single_cluster_bootstrap(data = d, 
                                                              groups = g),
             d = data,
             g = groups)
    }
    else
    {
      parallel::parLapply(
        cl = cl,
        X = 1:B,
        fun = function(i, d, g)
        {
          Bluegrass:::single_cluster_bootstrap(data = d,
                                               groups = g)
        },
        d = data,
        g = groups
      )
    }
  }
  else
  {
    if (is.null(cl))
    {
      lapply(1:B,
             function(i, d) dplyr::sample_n(d, nrow(d)),
             d = data)
    }
    else
    {
      parallel::parLapply(cl = cl,
                          X = 1:B,
                          fun = function(i, d) dplyr::sample_n(d, nrow(d)),
                          d = data)
    }
  }
}

#********************************************************************
#* UNEXPORTED FUNCTIONS
#********************************************************************

single_regular_bootstrap <- function(data)
{
  
}

single_cluster_bootstrap <- function(data, groups)
{
  data[["..bootstrap_cluster_id.."]] <- 
    data[groups] %>%
    apply(MARGIN = 1,
          FUN = paste,
          collapse = "-")
  
  distinct_cluster_id <- unique(data[["..bootstrap_cluster_id.."]])
  n_cluster <- length(distinct_cluster_id)
  
  cluster_select <- sample(x = distinct_cluster_id,
                           size = n_cluster,
                           replace = TRUE)
  
  lapply(cluster_select,
         function(cl, d = data) 
         {
           dplyr::filter(d, ..bootstrap_cluster_id.. %in% cl)
         }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-..bootstrap_cluster_id..)
}