rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)
library(Hmisc)

# read data
load("data/cleaned_data.RData")

# reg black white 10 17
didreg_B_W <- lm(wage_gap_B_W ~ gdp + edu_B_W + trt*time, data = df)
summary(didreg_B_W)
