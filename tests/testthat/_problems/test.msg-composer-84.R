# Extracted from test.msg-composer.R:84

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "logging", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(logging)
context("Testing message composer functionality [test.msg-composer]")
test_setup <- function() {
  test_env <- new.env(parent = emptyenv())
  test_env$logged <- NULL

  mock_action <- function(msg, handler, ...) {
    if (length(list(...)) && "dry" %in% names(list(...)))
      return(TRUE)
    test_env$logged <- c(test_env$logged, msg)
  }
  mock_formatter <- function(record) {
    paste(record$levelname, record$logger, record$msg, sep = ":")
  }

  logReset()
  addHandler(mock_action,
             formatter = mock_formatter)

  return(test_env)
}

# test -------------------------------------------------------------------------
expect_no_error(loginfo(as.Date("2022-01-01")))
