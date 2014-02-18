\name{get.outliers}
\alias{get.outliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{gets outlier values according to the method chosen}
\description{gets outlier values according to the method chosen}
\usage{
outliers <- get.outliers(data.in,method='median.absolute.deviation',reject.criteria=3,na.rm=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.in}{a numeric or vector array}
  \item{method}{a string which matches a valid outlier detection method}
  \item{reject.criteria}{a numeric value to determine the stringency of the outlier detection criteria. Miller (1991) proposes the values of 3 (very conservative), 2.5 (moderately conserva- tive) or even 2 (poorly conservative)}
  \item{na.rm}{boolean to determine if NAs should be removed before analysis. If TRUE, NAs will be outlier=TRUE.}
}
\value{
boolean array, equal in size to data.in, with outlier=T or F
}
\author{
Jordan S. Read
}

\keyword{outliers}
