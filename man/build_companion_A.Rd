% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_companion_A.R
\name{build_companion_A}
\alias{build_companion_A}
\title{Build the companion matrix for the autoregressive parameters in the state equation}
\usage{
build_companion_A(A_list)
}
\arguments{
\item{A_list}{A list containing the matrix of autoregressive parameters for each lag}
}
\value{
A block matrix with all the matrices of \code{A_list} in the first row
}
\description{
Build the companion matrix for the autoregressive parameters in the state equation
}
