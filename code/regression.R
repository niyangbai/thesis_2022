rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)

# read data
load("data/cleaned_data.RData")

# reg black white
didreg_B_W <- lm(wage_gap_B_W ~ trt*time + pop_B + pop_W + uer_B + uer_W + gdp_per_capita + density, data = df)
summary(didreg_B_W)

