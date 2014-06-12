\name{load.sqc}
\alias{load.sqc}
\title{load in configuration file for sensorQC}
\usage{
  load.sqc(deploy.name, folder = "../examples/")
}
\arguments{
  \item{\code{deploy.name}}{name of config file (proceeding
  *.yml extension).}

  \item{\code{folder}}{folder path for config file}
}
\value{
  a sqc object
}
\description{
  Loads text file for sensorQC. Assumes *.yml file
  extension for deployment.  \cr
}
\details{
  a \code{sensorQC} function loading config.\cr
}
\examples{
sqc <- load.sqc(deploy.name='pellerin',folder='../examples/')
}
\author{
  Jordan S. Read
}
\keyword{sqc}

