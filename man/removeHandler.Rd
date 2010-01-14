% File src/R/man/removeHandler.Rd
% $Id: removeHandler.Rd 11 2010-04-03 18:54:20Z mariotomo $

\name{removeHandler}
\alias{removeHandler}
\title{remove a handler from a logger}
\description{
  
  removes the handler \var{name} from the logger.
  
}
\usage{
removeHandler(name, logger='')
}
\arguments{
  \item{name}{the name of the handler}
  \item{logger}{the name of the logger to which to attach the new
    handler, defaults to the root logger}
}
