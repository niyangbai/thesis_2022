rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)

# read data
df <- read.csv("data/main_data.csv")

#label
library(Hmisc)
label(df$B_W) <- "Black and White wage gap"
label(df$A_W) <- "Asian and White wage gap"
label(df$time) <- "After treatment"
label(df$trt) <- "Treatment group"
label(df$min_wage) <- "State Minimum Wage"

# reg black white 10 17
didreg_B_W <- lm(B_W ~ trt*time, data = df)
summary(didreg_B_W)

# reg asian white 10 17
didreg_A_W <- lm(A_W ~ trt*time, data = df)
summary(didreg_A_W)
