rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)
minwage_state_year <- subset(minimum_wage, year %in% seq(2010, 2020))
control <- find_dif(2010, 2016)[find_dif(2010, 2016)[["dif"]] == 0, "state"]
treatment <- find_dif(2010, 2016)[(find_dif(2010, 2013)[["dif"]] == 0) & !(find_dif(2013, 2016)[["dif"]] == 0), "state"]

