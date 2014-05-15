\name{build.flags}
\alias{build.flags}
\title{Creates flag vector based on input data}
\usage{
  data.flags <- build.flags(data.in,params)
}
\arguments{
  \item{\code{data.in}}{a data.frame with columns for
  DateTime and sensor.obs}

  \item{\code{sqc}}{a sqc object with valid processing
  parameter names and associated values}
}
\value{
  a vector of flags of length equal to number of rows in
  data.in
}
\description{
  Creates flag vector with codes and methods according to
  params list.  \cr
}
\examples{
dates <- seq(as.POSIXct('1999-01-01'),by=1,length.out=14)
values <- c(runif(12,2,4),NA,NA)
data.in <- data.frame("DateTime"=dates,"sensor.obs"=values)

build.flags(data.in,params=NULL)
}
\author{
  Jordan S. Read
}
\keyword{flags}

