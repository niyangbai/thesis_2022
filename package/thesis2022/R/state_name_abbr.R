#' abbr to full name
#'
#' @param abbr
#'
#' @return
#' @export
#'
#' @examples
abbr2name <- function(abbr) {
  i <- which(state.abb == abbr)
  name <- state.name[i]
  return(name)
}

#' full name to abbr
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
name2abbr <- function(name) {
  i <- which(state.name == name)
  abbr <- state.abb[i]
  return(abbr)
}

"fips_code"
#' fips code to name
#'
#' @param fips
#'
#' @return
#' @export
#'
#' @examples
fips2name <- function(fips) {
  names <- paste0(fips_code$county_name, ", ", fips_code$state_name)
  i <- which(fips_code$fips == fips)
  name <- names[i]
  return(name)
}
