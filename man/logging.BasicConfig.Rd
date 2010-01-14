% File src/R/man/logging.BasicConfig.Rd

\name{logging.BasicConfig}
\alias{logging.BasicConfig}
\title{bootstrapping the logging package}
\description{

  create the root logger, attach it a console handler, set both at
  logging.level INFO.

}
\usage{
logging.BasicConfig()
}
\arguments{
}
\examples{
logging.BasicConfig()
logging.debug("not shown, basic is INFO")
logging.warn("shown and timestamped")
}