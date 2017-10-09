#' Pretend now is some other time.
#'
#' This method stubs the built-in \code{\link{Sys.time}}, 
#' \code{\link{Sys.Date}}, and \code{\link{date}} methods with an overwritten
#' value for use in unit tests.
#'
#' Either a \code{\link{Date}} or \code{\link{POSIXct}} object can be
#' provided, or an English string in the form "<number> <unit>s <from now/ago>"
#' such as:
#'
#' \itemize{
#'   \item{1 hour ago}
#'   \item{2 months from now}
#'   \item{10 seconds ago}
#' }
#' 
#' These are translated into \code{Sys.time() + as.difftime(number, units = unit)}
#' (or \code{-} in the case of "ago").
#'
#' @param time Date or POSIXct or character. For example,
#'   \code{Sys.time() + as.difftime(1, units = "days")} or
#'   \code{"1 day from now"}.
#' @param expr expression. The expression to evaluate while pretending
#'   the current time is \code{time}.
#' @return the value of \code{expr}.
#' @export
#' @examples
#' pretend_now_is(Sys.time() + as.difftime(1, units = "days"), {
#'   cat("It's UNIX time ", format(Sys.time(), "%Y-%m-%d HH:MM:SS %Z"), 
#'       " tomorrow!\n", sep = "")
#'  })
#'
#' now <- Sys.time()
#' pretend_now_is("10 minutes from now", {
#'   stopifnot(all.equal(
#'    as.integer(as.difftime(Sys.time() - now, units = "minutes")),
#'    10))
#' })
pretend_now_is <- function(time, expr) {
  time <- parse_time(time)

  eval.parent(substitute({
    testthatsomemore::package_stub("base", "Sys.Date", function() as.Date(time),
    testthatsomemore::package_stub("base", "date", function() format(time, "%a %b %d %H:%M:%S %Y"),
    testthatsomemore::package_stub("base", "Sys.time", function() time, {
      expr
  })))}))
}
#' Generic parse_time
#' @param time can be any of \code{POSIXct}, \code{Date}, or \code{character}.
#' @export
parse_time <- function(time) {
  UseMethod("parse_time")
}

#' S3 method for parse_time with \code{POSIXct} argument.
#' 
#' @param time is class \code{POSIXct}
#' @export
parse_time.POSIXct <- function(time) {
  time
}

#' S3 method for parse_time with \code{Date} argument.
#' 
#' @param time is class \code{Date}
#' @export 
parse_time.Date <- function(time) {
  as.POSIXct(time)
}

#' S3 method for parse_time with \code{character} argument.
#' 
#' @param time is class \code{character}
#' @export 
parse_time.character <- function(time) {
  strdate(time)
}

#' Default parse_time; throws error
#' @param time can be any of \code{POSIXct}, \code{Date}, or \code{character}.
#' @export 
parse_time.default <- function(time) {
  stop("Time provided to ", crayon::red("testthatsomemore::pretend_now_is"),
       " is in an invalid format. Must be a ", sQuote("POSIXct"), ", ",
       sQuote("Date"), ", or ", sQuote("character"), ".", call. = FALSE)
}
