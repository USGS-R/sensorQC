\name{get.outliers}
\alias{get.outliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{gets outlier values according to the method chosen}
\description{gets outlier values according to the method chosen}
\usage{
outliers <- get.outliers(data.in,method='median.absolute.deviation')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.in}{a numeric or vector array}
  \item{method}{a string which matches a valid outlier detection method}
}
\value{
boolean array, equal in size to data.in, with outlier=T or F
}
\author{
Jordan S. Read
}

\keyword{outliers}
