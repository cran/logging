##***********************************************************************
## this program is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## this program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with the nens libraray.  If not, see
## <http://www.gnu.org/licenses/>.
##
## Library    : the nens R library
##
## Purpose    : most common functions
##
## Usage      : library(NenS)
##
## Project    : LizardScripter
##
## $Id: logger.R 8994 2010-01-15 07:52:42Z Mario $
##
## initial programmer :  Mario Frasca
## based on:             Brian Lee Yung Rowe's futile library
##
## initial date       :  20100105
##

## TODO: these constants must be exported and documented
logging.levels <- c(0, 10, 20, 30, 30, 40, 50, 50)
names(logging.levels) <- c('NOTSET', 'DEBUG', 'INFO', 'WARNING', 'WARN', 'ERROR', 'CRITICAL', 'FATAL')

## main log function, used by all other ones
## (entry points for messages)
logging.log <- function(level, msg, ..., logger='')
{
  ## get the logger of which we have the name.
  config <- logging.getLogger(logger)
  if (level < config$level) return(invisible())

  ## what is the record to be logged?
  record <- list()

  if (length(list(...)) > 0) msg <- sprintf(msg, ...)
  record$msg <- msg

  record$timestamp <- sprintf("%s", Sys.time())
  record$logger <- logger
  record$level <- names(which(logging.levels == level)[1])
  if(is.na(record$level))
    record$level <- paste("NumericLevel(", level, ")", sep='')

  ## pass the record to all handlers associated to logger
  for (handler in config$handlers) {
    if (level >= handler$level) {
      handler$fun(handler$formatter(record), handler)
    }
  }

  ## if not at root level, check the parent logger
  if(logger != ''){
    parts <- strsplit(logger, '\\.')[[1]]
    removed <- parts[-length(parts)]
    parent <- paste(removed, collapse='.')
    logging.log(level, msg, ..., logger=parent)
  }
  
  invisible()
}

## using logging.log
logging.debug <- function(msg, ..., logger='')
{
  logging.log(logging.levels['DEBUG'], msg, ..., logger=logger)
  invisible()
}

## using logging.log
logging.info <- function(msg, ..., logger='')
{
  logging.log(logging.levels['INFO'], msg, ..., logger=logger)
  invisible()
}

## using logging.log
logging.warn <- function(msg, ..., logger='')
{
  logging.log(logging.levels['WARN'], msg, ..., logger=logger)
  invisible()
}
logging.warning <- logging.warn

## using logging.log
logging.error <- function(msg, ..., logger='')
{
  logging.log(logging.levels['ERROR'], msg, ..., logger=logger)
  invisible()
}

## set properties of a logger
updateLogger <- function(name, ...) {
  if(name=='')
    name <- 'logging.ROOT'
  else
    name <- paste('logging.ROOT', name, sep='.')

  config <- list(...)
  if (! 'level' %in% config)
    config$level = logging.levels['INFO']

  exp <- parse(text=paste('logging.options(',name,' = config)', sep=''))
  eval(exp)
}

## Get a specific logger configuration
## remember: you can't alter an object in R
logging.getLogger <- function(name, ...)
{
  if(name=='')
    fullname <- 'logging.ROOT'
  else
    fullname <- paste('logging.ROOT', name, sep='.')

  if (! fullname %in% names(logging.options())){
    updateLogger(name, ...)
  }

  logging.options()[[fullname]]
}

## set the level of a handler or a logger
logging.setLevel <- function(name, level)
{
  updateLogger(name, level=level)
}

#################################################################################

## predefined handlers

## a handler is a function that accepts a logging.record and a
## configuration.

## a logging.record contains the real message, its level, the name of the
## logger that generated it, a timestamp.

## a configuration contains a formatter (a function taking a
## logging.record and returning a string), a numeric level (only records
## with level equal or higher than that are taken into account), an
## action (writing the formatted record to a stream).

logging.handler.stdout <- function(msg, handler)
{
  cat(msg)
}

logging.handler.file <- function(msg, handler)
{
  if (! 'file' %in% names(handler))
  {
    cat("Required argument 'file' is missing.\n")
    return()
  }
  cat(msg, file=handler$file, append=TRUE)
}

#################################################################################

## the single predefined formatter

logging.formatter.default <- function(record) {
  paste(record$timestamp, record$level, record$msg, '\n')
}

#################################################################################

logging.BasicConfig <- function() {
  updateLogger('', level=logging.levels['INFO'])
  logging.addHandler(logging.handler.stdout, name='basic.stdout')
  invisible()
}

## Add a new handler to the options config
## The following values need to be provided:
##   name - the name of the logger to which the logger is to be attached
##   level - log level for new handler
##   fun - the implementation for the handler. Either a function or a name of
##     a function
##   ... options to be stored as fields of new handler
logging.addHandler <- function(name, fun, ..., level=20, logger='', formatter=logging.formatter.default)
{
  handlers <- logging.getLogger(logger)[['handlers']]

  handler <- list(level=level, fun=fun, formatter=formatter)
  handlers[name] <- list(handler) # this does not alter the original list

  updateLogger(logger, handlers=handlers) # this replaces the original list
  
  invisible()
}

logging.removeHandler <- function(name, logger='') {
  handlers <- logging.getLogger(logger)[['handlers']]
  to.keep <- !(names(handlers) == name)
  updateLogger(logger, handlers=handlers[to.keep])
  invisible()
}

#################################################################################

## initialize the module

## The logger options manager
logging.options <- options.manager('logging.options')

logging.getLogger('', handlers=NULL, level=0)
