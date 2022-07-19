#' get acs5 data
#'
#' @param table_name
#' @param period
#' @param data_code
#'
#' @return
#' @export
#'
#' @examples
get_acs5_data <- function(table_name, period, data_code, group_code = NA) {
  df <- data.frame()
  for (year in period) {
    data <- getCensus(
      name = table_name,
      vintage = year,
      vars = data_code,
      region = "county:*")
    data$year <- year
    if (nrow(df) == 0) {
      df <- data
    } else {
      df <- rbind(df, data)
    }
  }
  df$fips <- paste0(df$state, df$county)
  return(df)
}
