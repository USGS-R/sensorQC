\name{window.data}
\alias{window.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Breaks up time series data into window chunks}
\description{Breaks up time series data into window chunks}
\usage{
data.out <- window.data(data.in,window=30)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.in}{a data.frame of time series data}
  \item{window}{numeric, in seconds, specifying the window time width}
}
\value{
a list of time series data and indices for breaks
}
\author{
Jordan S. Read
}

\keyword{window}
