% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_RLE.R
\name{plot_RLE}
\alias{plot_RLE}
\title{RLE plot}
\usage{
plot_RLE(exprs_matrix)
}
\arguments{
\item{exprs_matrix}{a matrix}
}
\description{
RLE plot
}
\examples{
n = 30
p = 1000
exprs_matrix = cbind(
matrix(rnorm((n - 3)*p), nrow = p),
matrix(rnorm(3*p, mean = 5), nrow = p)
)
boxplot(exprs_matrix)
plot_RLE(exprs_matrix)
}
\author{
Kevin Wang
}
