#' get_bea_data
#'
#' @param beaKey
#' @param table_name
#' @param line_code
#' @param period
#'
#' @return
#' @export
#'
#' @examples
get_bea_data <- function(beaKey, table_name, line_code, period) {
  df <- data.frame()
  for (i in period) {
    gdp <- list(
      "UserID" = beaKey,
      "Method" = "GetData",
      "datasetname" = "Regional",
      "TableName" = table_name,
      "GeoFips" = "COUNTY",
      "LineCode" = line_code,
      "Year" = i
    )
    data <- beaGet(gdp, asWide = FALSE)
    if (nrow(df) == 0) {
      df <- data
    } else {
      df <- rbind(df, data)
    }
  }
  return(df)
}
