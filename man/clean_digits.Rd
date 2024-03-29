% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/one_liners.R
\name{clean_digits}
\alias{clean_digits}
\title{Clean the digits in a data frame}
\usage{
clean_digits(df, digits = 2)
}
\arguments{
\item{df}{A data frame with numeric columns}

\item{digits}{Number of significant figures to round the numeric columns to}
}
\value{
A data frame with all numeric columns rounded to the specified
number of significant figures
}
\description{
Clean the digits in a data frame
}
\examples{
head(iris)
head(clean_digits(iris, digits = 1))
}
\author{
Kevin Wang
}
