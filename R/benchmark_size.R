#' @name benchmark_size
#' @title Benchmark Expressions Against Variably Sized Data
#'
#' @description Determining the optimal expression to use is not always
#' obvious, and the size of the data frame may make a difference in what
#' is the fastest solution. This function manages the benchmarking over
#' datasets of different sizes and returns the results to facilitate
#' a decision about which solution to use.
#'
#' @param x An atomic vector or a data frame
#' @param size \code{numeric}, sample sizes to iterate over.
#' @param times \code{numeric}, how many times to run the code in
#'   \code{microbenchmark}
#' @param unit \code{character}, specifies the unit of time to measure.
#' @param ... expressions to benchmark
#'   Use \code{ncore = 1} to run serially.
#'
#' @export

benchmark_size <- function(object, ..., size = c(10, 100, 1000),
                              times = 100, unit = "ms"){
  UseMethod("benchmark_size")
}

#' @rdname benchmark_on_data
#' @export

benchmark_size.default <- function(object, ..., size = c(10, 100, 1000),
                                      times = 100, unit = "ms"){
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_atomic(object,
                           add = coll)

  checkmate::assert_integerish(x = size,
                               add = coll)

  checkmate::assert_integerish(x = times,
                              len = 1,
                              lower = 1,
                              add = coll)

  unit <- checkmate::matchArg(x = unit,
                              choices = c("ns", "us", "ms", "s", "eps", "relative"),
                              add = coll)

  checkmate::assert_class(x = cl,
                          classes = "cluster",
                          null.ok = TRUE,
                          add = coll)

  m <- lapply(size,
              function(s, object, times, ...){
                object <- sample(object, size = s, replace = TRUE)
                microbenchmark::microbenchmark(
                  list = eval(substitute(alist(...))),
                  times = times
                )
              },
              object = object,
              times = times,
              ...)

  M <- lapply(m, summary, unit = unit)
  M <- mapply(function(m, size){
                m$size = size
                m$unit = unit
                m
              },
              m = M,
              size = size,
              SIMPLIFY = FALSE)
  do.call(rbind, M)
}

#' @rdname benchmark_size
#' @export

benchmark_size.data.frame <- function(object, ..., size = c(10, 100, 1000),
                                      times = 100, unit = "ms"){
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(x = object,
                               add = coll)

  checkmate::assert_integerish(x = size,
                               add = coll)

  checkmate::assert_integerish(x = times,
                               len = 1,
                               lower = 1,
                               add = coll)

  unit <- checkmate::matchArg(x = unit,
                              choices = c("ns", "us", "ms", "s", "eps", "relative"),
                              add = coll)

  checkmate::assert_class(x = cl,
                          classes = "cluster",
                          null.ok = TRUE,
                          add = coll)

  checkmate::reportAssertions(coll)

  m <- lapply(size,
              function(s, object, times, ...){
                object <-
                  object[sample(seq_len(nrow(object)),
                                size = s,
                                replace = TRUE), ]
                microbenchmark::microbenchmark(
                  list = eval(substitute(alist(...))),
                  times = times
                )
              },
              object = object,
              times = times,
              ...)

  M <- lapply(m, summary, unit = unit)
  M <- mapply(function(m, size){
                m$size = size
                m$unit = unit
                m
              },
              m = M,
              size = size,
              SIMPLIFY = FALSE)
  do.call(rbind, M)
}
