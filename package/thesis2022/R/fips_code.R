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
  i <- which(fips_code$fips == as.numeric(fips))
  name <- toString(fips_code$long_name[i])
  return(name)
}

#' fips code to state
#'
#' @param fips
#'
#' @return
#' @export
#'
#' @examples
fips2state <- function(fips) {
  i <- which(fips_code$fips == as.numeric(fips))
  name <- toString(fips_code$state_name[i])
  return(name)
}

#' fips code to county
#'
#' @param fips
#'
#' @return
#' @export
#'
#' @examples
fips2county <- function(fips) {
  i <- which(fips_code$fips == as.numeric(fips))
  name <- toString(fips_code$county_name[i])
  return(name)
}

#' fips code to region
#'
#' @param fips
#'
#' @return
#' @export
#'
#' @examples
fips2region <- function(fips) {
  i <- which(fips_code$fips == as.numeric(fips))
  region <- toString(fips_code$region_name[i])
  return(region)
}
