rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)

# read data
load("data/cleaned_data.RData")

df$pop_B_W <- df$pop_B - df$pop_W
df$uer_B_W <- df$uer_B - df$uer_W

# reg black white
didreg_B_W <- lm(wage_gap_B_W ~ trt*time + pop_B_W + uer_B_W + gdp_per_capita + density, data = df)
summary(didreg_B_W)

