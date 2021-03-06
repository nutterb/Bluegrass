% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showDuplicated.R
\name{showDuplicated}
\alias{showDuplicated}
\title{Subset a Data Frame to Duplicated Subjects}
\usage{
showDuplicated(data, ...)
}
\arguments{
\item{data}{A data frame}

\item{...}{the variable names in the data frame that indicate distinct records.
These should be unquoted and may include any number of fields.}
}
\description{
Reduces a data frame to those subjects that are duplicated,
  but also includes the index record.
}
\details{
The data frame is first sorted by the fields in \code{...}.  The user
  may also specify fields to be sorted in descencing order via the \code{desc}
  function in \code{plyr}, but it is not strictly necessary.
  
  It is assumed that the variable names do not include parentheses.
}
\author{
Benjamin Nutter
}
