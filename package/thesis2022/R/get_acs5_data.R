#' get_acs5_data
#'
#' @param period 
#' @param table 
#' @param subtable 
#'
#' @return
#' @export
#'
#' @examples
get_acs5_data <- function(period, table, subtable) {
  df <- data.frame()
  for (year in period) {
    year_tab <- data.frame()
    for (i in l) {
      g <- paste("group(", i, ")", sep="")
      subdf <- getCensus(
        name = "acs/acs5",
        vintage = year,
        vars = c("NAME", g),
        region = "county:*")
      e <- paste(i, "_", subtable, sep="")
      subdf <- subdf[c("NAME", e)]
      if (nrow(year_tab) == 0) {
        year_tab <- subdf
      } else {
        year_tab <- merge(year_tab, subdf, by="NAME")
      }
      year_tab$year <- year
    }
    df <- rbind(df, year_tab)
  }
  return(df)
}