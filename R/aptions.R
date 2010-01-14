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
## $Id: aptions.R 8994 2010-01-15 07:52:42Z Mario $
##
## initial programmer :  Mario Frasca
## based on:             Brian Lee Yung Rowe's futile library
##
## initial date       :  20100105
##

## Examples of using options.manager
##  log.options <- options.manager('log.options', defaults=list(logger=''))
##  log.options(a=123, b=6234)
##  log.options()
##  log.options(a=123, b=6234)
##  reset.options(log.options, c=29)
##  log.options()
## Generates a function to retrieve options for a given name
options.manager <- function(option.name, defaults=NULL)
{
  function(...)
  {
    os <- getOption(option.name)
    if (is.null(os))
    {
      if (is.null(defaults)) os <- list() else os <- defaults
    }
    # This is here because there seem to be some issues with lazy evaluation
    # (maybe there is none in R?)
    else if (length(os) == 1 & any(is.na(os)) )
    {
      if (is.null(defaults)) os <- list() else os <- defaults
    }

    args <- list(...)
    if (length(args) == 0) return(os)

    # Getter
    if (any(is.null(names(args))))
    {
      ns <- sapply(args, '[')
      ns <- ns[ns %in% names(os)]
      if (length(ns) == 0) return(NULL)
      return(sapply(os[ns], '['))
    }

    # Setter
    for (x in names(args)) os[[x]] <- args[[x]]
    my.options <- list()
    my.options[[option.name]] <- os
    options(my.options)

    invisible()
  }
}

# Reset options for a given option set
reset.options <- function(option.name, ...) UseMethod('reset.options')
reset.options.default <- function(option.name, ...)
  reset.options.character(deparse(substitute(option.name)), ...)

reset.options.character <- function(option.name, ...)
{
  my.options <- list()
  my.options[[option.name]] <- NA
  options(my.options)

  args <- list(...)
  if (length(args) > 0)
  {
    ks <- names(args)
    vs <- sapply(args, '[')
    kvs <- paste(ks,vs, sep='=')
    line <- paste(kvs, collapse=',')

    exp <- parse(text=paste('new.options <- ',option.name,'(',line,')',sep=''))
    eval(exp)
  }
  invisible()
}
