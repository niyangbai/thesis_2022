rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)

# read data
load("data/cleaned_data.RData")

# log
df$wage_gap_B_W <- log(df$wage_gap_B_W)
df$share_B <- log(df$share_B)
df$share_W <- log(df$share_W)
df$share_F <- log(df$share_F)
df$emp_B <- log(df$emp_B)
df$emp_W <- log(df$emp_W)
df$edu_rate_B <- log(df$edu_rate_B)
df$edu_rate_W <- log(df$edu_rate_W)
df$gdp_per_capita <- log(df$gdp_per_capita)
df$density <- log(df$density)
df$age_rate_B <- log(df$age_rate_B)
df$age_rate_W <- log(df$age_rate_W)
df$po_rate_T <- log(df$po_rate_T)
df$po_rate_W <- log(df$po_rate_W)
df$po_rate_B <- log(df$po_rate_B)

# ignore inf
df <- do.call(data.frame, lapply(df, function(value) replace(value, is.infinite(value),NA)))

# reg black white
didreg_B_W <- lm(wage_gap_B_W ~ trt * time + edu_rate_B + edu_rate_W + share_B + share_W + emp_B + emp_W + age_B + age_W + density + gdp_per_capita + is_Midwest + is_Northeast + is_South, data = df)
summary(didreg_B_W)
