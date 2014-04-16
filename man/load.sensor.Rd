\name{load.sensor}
\alias{load.sensor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{loads sensor data into data.frame according to the file format specified.}
\description{Loads sensor data into data.frame}
\usage{
data.out <- load.sensor(filename,format="Pellerin")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{a string}
  \item{format}{a string which matches a valid sensor data format}
}
\value{
A data.frame with DateTime and values
}
\author{
Jordan S. Read
}

\keyword{load}
