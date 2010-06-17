% File src/R/man/removeHandler.Rd
% $Id: removeHandler.Rd 86 2011-08-03 13:16:48Z mariotomo $

\name{removeHandler}
\alias{removeHandler}
\title{remove a handler from a logger}
\description{
  
  removes the \var{handler} from the \var{logger}.
  
}
\usage{
removeHandler(handler, logger='')
}
\arguments{
  \item{handler}{the name or the action of the handler}
  \item{logger}{the name of the logger to which to attach the new
    handler, defaults to the root logger}
}
