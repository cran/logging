% File src/R/man/removeHandler.Rd
% $Id: removeHandler.Rd 17 2010-04-08 06:04:24Z mariotomo $

\name{removeHandler}
\alias{removeHandler}
\alias{removeHandler.default}
\alias{removeHandler.character}
\title{remove a handler from a logger}
\description{
  
  removes the \var{handler} from the \var{logger}.
  
}
\usage{
removeHandler(handler, logger='')
\method{removeHandler}{default}(handler, logger='')
\method{removeHandler}{character}(handler, logger='')
}
\arguments{
  \item{handler}{the name or the action of the handler}
  \item{logger}{the name of the logger to which to attach the new
    handler, defaults to the root logger}
}
