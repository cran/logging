% File src/R/man/logging.addHandler.Rd

\name{logging.addHandler}
\alias{logging.addHandler}
\title{add a handler to a logger}
\description{
  
  defines a function as a handler of log records going through the
  specified logger.
  
}
\details{

  a handler has a \var{name} and at least the three fields:
  \describe{
    \item{level}{all records at level lower than this are skipped.}
    \item{formatter}{a function getting a record and returning a string}
    \item{fun(msg, handler)}{a function accepting two parameters: a formatted
  log record and the handler itself.}}

  further a handler may have as many fields as you think you need.  keep
  in mind the handler (and all of its fields) are passed to the actual
  function.
  
}
\usage{
logging.addHandler(name, fun, ..., level=20, logger='', formatter=logging.formatter.default)
}
\arguments{
  \item{name}{the name of the handler}
  \item{fun}{the implementation of the handler}
  \item{...}{extra parameters for the handler function}
  \item{level}{the level of the handler, defaults to 20 (INFO)}
  \item{logger}{the name of the logger to which to attach the new
    handler, defaults to the root logger}
  \item{formatter}{a function taking a record and producing a string.}
}
