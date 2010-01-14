% File src/R/man/logging.handler-actions.Rd

\name{logging.handler-actions}
\alias{logging.handler-actions}
\alias{logging.handler.file}
\alias{logging.handler.stdout}
\title{predefined handler actions}
\description{
  
  when you define a handler, you specify its name and the associated
  action.  a few predefined actions are provided and this page documents
  them.  

}
\usage{
logging.handler.file(msg, handler)
logging.handler.stdout(msg, handler)
}
\arguments{
  \item{msg}{the final formatted message to be output}
  \item{handler}{the handler owning this action}
}
\details{

  a handler action is a function taking two parameters: the formatted
  log record and the handler to which the action is associated.  the
  second parameter is useful so you can register the same handler action
  to handlers with different properties.

}