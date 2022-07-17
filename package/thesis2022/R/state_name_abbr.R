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

#' find area of a state
#'
#' @param state
#'
#' @return
#' @export
#'
#' @examples
state2area <- function(state) {
  i <- which(state.name == state)
  area <- as.numeric(state.area[i])
  return(area)
}
