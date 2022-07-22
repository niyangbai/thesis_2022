rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)

# read data
load("data/main_cleaned_data.RData")

# #log
# nolog <- c("fips", "ID", "state", "year",
#            "wage_gap_B_W", "wage_gap_A_W", "time", "county",
#            "trt", "fuzzy_trt", "region", "min_wage")
# bo <- names(main_df)[unname(sapply(names(main_df), function(x) grepl("is_", x, fixed = TRUE)))]
# nolog <- append(nolog, bo)
# tolog <- names(main_df)[!(names(main_df) %in% nolog)]
# for (i in tolog) {
#   main_df[i] <- log(main_df[i])
# }
# 
# # ignore inf
# main_df <- do.call(data.frame, lapply(main_df, function(value) replace(value, is.infinite(value),NA)))

# omit NA
main_df <- na.omit(main_df)

# reg
# didreg_B_W <- lm(wage_gap_B_W ~ trt * time + edu_rate_B + edu_rate_W + emp_rate_B + emp_rate_W + pop_share_B + pop_share_W + poverty_rate_W + poverty_rate_B + density + gdp_per_capita + is_Midwest + is_Northeast + is_South, data = main_df)
# didreg_A_W <- lm(wage_gap_A_W ~ trt * time + edu_rate_A + edu_rate_W + emp_rate_A + emp_rate_W + pop_share_A + pop_share_W + poverty_rate_W + poverty_rate_A + density + gdp_per_capita + is_Midwest + is_Northeast + is_South, data = main_df)

# main_df$did <- main_df$trt * main_df$time

# didreg_B_W <- lm(wage_gap_B_W ~ trt * time + edu_W + edu_B + gdp + pop_W + pop_B + pop_T + emp_W + emp_B + poverty_B + poverty_W + is_Midwest + is_Northeast + is_South, data = main_df)
# didreg_A_W <- lm(wage_gap_A_W ~ trt * time + edu_W + edu_A + gdp + pop_W + pop_A + pop_T + emp_W + emp_A + poverty_A + poverty_W + is_Midwest + is_Northeast + is_South, data = main_df)
didreg_B_W <- lm(wage_gap_B_W ~ trt * time, data = main_df)
didreg_A_W <- lm(wage_gap_A_W ~ trt * time, data = main_df)
summary(didreg_B_W)
summary(didreg_A_W)

