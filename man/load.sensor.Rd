\name{load.sensor}
\alias{load.sensor}
\title{Loads sensor data into data.frame}
\usage{
  data.out <- load.sensor(filename,format="Pellerin")
}
\arguments{
  \item{\code{filename}}{a string}

  \item{\code{format}}{a string which matches a valid
  sensor data format}
}
\value{
  A data.frame with DateTime and values
}
\description{
  loads sensor data into data.frame according to the file
  format specified.  \cr
}
\examples{
filename <- '../examples/test_data.txt'
data.out <- load.sensor(filename,format="Pellerin")
print(data.out)
}
\author{
  Jordan S. Read
}
\keyword{sqc}

