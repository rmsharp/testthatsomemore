#' Convert a relative time character expression into an absolute date expression.
#'
#' Taken from non-building version of github.com/peterhurford/strdate/strdate.R
#'  Commented out use of checkr just to get it running.
#'  
#' @param time character. The time character expression to convert.
#' @param relative_to POSIXt. Relative to what time are we converting?
#' ###import checkr
#'
#' @export
strdate <- #checkr::ensure(
  #pre = list(time %is% simple_string, relative_to %is% POSIXt),
  function(time, relative_to = Sys.time()) {
    if (identical(time, "now")) { return(Sys.time()) }
    
    # http://blog.codinghorror.com/regular-expressions-now-you-have-two-problems/
    regex <- paste0("[[:space:]]*([[:digit:]]+)[[:space:]]*([[:alpha:]]+)",
                    "[[:space:]]*(from now|ago)[[:space:]]*")
    
    matches <- regexpr(regex, time, perl = TRUE, ignore.case = TRUE)
    if (matches == -1) {
      stop("Could not parse ", sQuote(time), " into a time.", call. = FALSE)
    }
    list2env(extract_time(matches, time), environment())
    
    number <- as.integer(number)
    unit   <- normalize_unit(tolower(unit))
    op     <- if (tolower(tense) == "from now") `+` else `-`
    
    list2env(legal_unit_number_pair(unit, number), environment())
    op(relative_to, as.difftime(number, units = unit))
  }
#)

normalize_unit <- function(unit) {
  if (substring(unit, nchar(unit)) != "s") {
    unit <- paste0(unit, "s")
  }
  
  if (unit == "seconds") "secs"
  else if (unit == "minutes") "mins"
  else unit
}

legal_unit_number_pair <- function(unit, number) {
  if (unit == "months") {
    unit   <- "days"
    number <- 30 * number
  } else if (unit == "years") {
    unit   <- "days"
    number <- 365 * number
  } else if (unit == "eons") {
    unit   <- "days"
    number <- 99999999 * number
  }
  list(unit = unit, number = number)
}

extract_time <- function(matches, time) {
  stats::setNames(nm = c("number", "unit", "tense"),
           Map(substring, time, s <- attr(matches, "capture.start"),
               s + attr(matches, "capture.length") - 1))
}