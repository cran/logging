% File src/R/man/logging.log.Rd

\name{logging.log}
\alias{logging.log}
\alias{logging.debug}
\alias{logging.info}
\alias{logging.warn}
\alias{logging.warning}
\alias{logging.error}
\title{entry points for logging actions}
\description{
  generate a log record and pass it to the logging system.

  a log record gets timestamped and will be independently formatted by
  each of the handlers handling it.

}
\usage{
logging.debug(msg, ..., logger='')
logging.info(msg, ..., logger='')
logging.warn(msg, ..., logger='')
logging.error(msg, ..., logger='')

logging.log(level, msg, ..., logger='')
}
\arguments{
  \item{msg}{the textual message to be output, or the format for the
    ... arguments}
  \item{...}{if present, msg is interpreted as a format and the
    ... values are passed to it to form the actual message.}
  \item{logger}{the name of the logger to which we pass the record}
  \item{level}{the explicit numeric level of the log record}
}
