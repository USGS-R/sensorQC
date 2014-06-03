\name{threshold}
\alias{threshold}
\title{threshold tests according to expected format}
\usage{
  threshold(data.in, expr = "x > 99")
}
\arguments{
  \item{\code{data.in}}{a \code{sensorQC} data.frame.}

  \item{\code{expr}}{a valid expression as a string}
}
\value{
  a vector of booleans for flags for this test
}
\description{
  threshold tests according to expected format
}
\details{
  a \code{sensorQC} function for creating flags based on
  threshold exceedence.\cr
}
\author{
  Jordan S. Read
}
\keyword{threshold}

