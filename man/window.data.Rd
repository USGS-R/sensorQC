\name{window.data}
\alias{window.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Breaks up time series data into window chunks}
\description{Breaks up time series data into window chunks}
\usage{
data <- load.sensor(filename,format="Pellerin")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.in}{a data.frame of time series data}
  \item{window}{a Date object}
}
\value{
a list of time series data and indices for breaks
}
\author{
Jordan S. Read
}

\keyword{window}
