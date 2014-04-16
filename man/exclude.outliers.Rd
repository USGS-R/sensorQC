\name{exclude.outliers}
\alias{exclude.outliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{removes outliers according to flags.}
\description{removes outliers according to flags}
\usage{
data.out <- exclude.outliers(data.in,data.flags,exclude.flags,write.log=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.in}{a time series data.frame}
  \item{data.flags}{a numeric vector (same length as ncols in data.in) containing data flags}
  \item{exclude.flags}{filter for the type of flags to remove vs ignore}
  \item{write.log}{boolean to determine whether log file is written}
}
\value{
A data.frame with DateTime and values
}
\author{
Jordan S. Read
}

\keyword{filter}
