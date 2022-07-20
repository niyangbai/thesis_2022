rm(list = ls())
setwd("D:/github/wagegap22")
library(usmap)
library(ggplot2)
library(wagegap22package)

# read data
load("data/cleaned_data.RData")

plot_usmap(data = df[which(df$year=='2017'), c("wage_gap_B_W", "fips")], values = "wage_gap_B_W", color = "black") +
  scale_fill_continuous(
    low = "red", high = "green", na.value="white", name = "Wage Gap between White and Black (2017)", label = scales::comma
  ) + theme(legend.position = "top")
