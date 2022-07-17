#' get unemploymentd data from bls
#'
#' @param urls
#'
#' @return
#' @export
#'
#' @examples
get_unemp_from_bls <- function(urls) {
  df <- data.frame()
  for (i in urls) {
    data <- read.xlsx(i, startRow = 5)
    data <- data[-c(nrow(data):(nrow(data) - 1)),]
    if (nrow(df) == 0) {
      df <- data
    } else {
      df <- rbind(df, data)
    }
  }
  return(df)
}
