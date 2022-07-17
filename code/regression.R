rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)

# read data
load("data/cleaned_data.RData")

# reg black white 10 17
didreg_B_W <- lm(wage_gap_B_W ~ edu_B_W + gdp + uer + trt*time, data = df)
summary(didreg_B_W)
