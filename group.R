rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)
minwage_state_year <- subset(minimum_wage, year %in% seq(2010, 2020))

find_dif <- function(start_year, end_year, data = minwage_state_year) {
  starts <- data[which(data[["year"]] == start_year),]
  ends <- data[which(data[["year"]] == end_year),]
  data <- merge(starts, ends, by = "state")
  df <- data["state"]
  df$dif <- data$State.Minimum.Wage.x - data$State.Minimum.Wage.y
  return(df)
}
control <- find_dif(2010, 2020)[find_dif(2010, 2020)[["dif"]] == 0, "state"]
treatment <- find_dif(2013, 2020)[(find_dif(2010, 2013)[["dif"]] == 0) & !(find_dif(2013, 2020)[["dif"]] == 0), "state"]

