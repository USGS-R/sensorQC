\name{window.data}
\alias{window.data}
\title{window sensorQC data}
\usage{
  data.out <-
    window.data(data.in,method="auto",window=NULL)
}
\arguments{
  \item{\code{data.in}}{a data.frame of time series data}

  \item{\code{method}}{A string. "auto" or "manual"
  supported.}

  \item{\code{window}}{numeric, in seconds, specifying the
  window time width}
}
\value{
  a list of time series data and indices for breaks
}
\description{
  Breaks up time series data into window chunks.  \cr
}
\author{
  Jordan S. Read
}
\keyword{window}

