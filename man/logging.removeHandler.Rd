% File src/R/man/logging.removeHandler.Rd

\name{logging.removeHandler}
\alias{logging.removeHandler}
\title{remove a handler from a logger}
\description{
  
  removes the handler \var{name} from the logger.
  
}
\usage{
logging.removeHandler(name, logger='')
}
\arguments{
  \item{name}{the name of the handler}
  \item{logger}{the name of the logger to which to attach the new
    handler, defaults to the root logger}
}
