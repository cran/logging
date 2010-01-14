% File src/R/man/log.Rd
% $Id: log.Rd 18 2010-04-08 09:44:43Z mariotomo $

\name{loginfo}
\alias{logfinest}
\alias{logfiner}
\alias{logfine}
\alias{logdebug}
\alias{loginfo}
\alias{logwarn}
\alias{logwarning}
\alias{logerror}
\title{entry points for logging actions}
\description{
  generate a log record and pass it to the logging system.

  a log record gets timestamped and will be independently formatted by
  each of the handlers handling it.

}
\usage{
logdebug(msg, ..., logger='')
loginfo(msg, ..., logger='')
logwarn(msg, ..., logger='')
logerror(msg, ..., logger='')
}
\arguments{
  \item{msg}{the textual message to be output, or the format for the
    ... arguments}
  \item{...}{if present, msg is interpreted as a format and the
    ... values are passed to it to form the actual message.}
  \item{logger}{the name of the logger to which we pass the record}
}
