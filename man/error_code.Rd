\name{error_code}
\alias{error_code}
\title{tests for value equal to a known error code}
\usage{
  error_code(data.in, expr = "x == -999")
}
\arguments{
  \item{\code{data.in}}{a \code{sensorQC} data.frame.}

  \item{\code{expr}}{a valid expression as a string (e.g.,
  x==-999)}
}
\value{
  a vector of booleans for flags for this test
}
\description{
  tests for value equal to a known error code
}
\details{
  a \code{sensorQC} function for creating flags based on
  error code matches.\cr
}
\author{
  Jordan S. Read
}
\keyword{error_code}

