rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)

# read data
load("data/main_cleaned_data.RData")

#log
nolog <- c("fips", "ID", "state", "year",
           "wage_gap_B_W", "wage_gap_A_W", "time", "county",
           "trt", "fuzzy_trt", "region", "min_wage")
bo <- names(main_df)[unname(sapply(names(main_df), function(x) grepl("is_", x, fixed = TRUE)))]
nolog <- append(nolog, bo)
tolog <- names(main_df)[!(names(main_df) %in% nolog)]
for (i in tolog) {
  main_df[i] <- log(main_df[i])
}

# ignore inf
main_df <- do.call(data.frame, lapply(main_df, function(value) replace(value, is.infinite(value),NA)))

# omit NA
main_df <- na.omit(main_df)


# 2010-2016
# didreg_B_W <- lm(wage_gap_B_W ~ trt * time + edu_B + edu_W + pop_W + pop_B + pop_T + pop_F + poverty_B + poverty_W + is_Midwest + is_Northeast + is_South, data = main_df)
# didreg_A_W <- lm(wage_gap_A_W ~ trt * time + edu_A + edu_W + pop_W + pop_A + pop_T + pop_F + poverty_A + poverty_W + is_Midwest + is_Northeast + is_South, data = main_df)
# 2016-2020
# didreg_B_W <- lm(wage_gap_B_W ~ trt * time + pop_W + pop_B + pop_T + edu_W + edu_B + poverty_B + poverty_W + gdp + area + emp_W + emp_B + is_Midwest + is_Northeast + is_South,  data = main_df)
# didreg_A_W <- lm(wage_gap_A_W ~ trt * time + pop_W + pop_A + pop_T + edu_W + edu_A + poverty_A + poverty_W + gdp + area + emp_W + emp_A + is_Midwest + is_Northeast + is_South,  data = main_df)



# reg
# main_df$did <- main_df$trt * main_df$time

# didreg_B_W <- lm(wage_gap_B_W ~ trt * time + edu_W + edu_B + gdp + pop_W + pop_B + pop_T + pop_F + emp_W + emp_B + poverty_B + poverty_W + area + is_Midwest + is_Northeast + is_South, data = main_df)
# didreg_A_W <- lm(wage_gap_A_W ~ trt * time + edu_W + edu_A + gdp + pop_W + pop_A + pop_T + pop_F + emp_W + emp_A + poverty_A + poverty_W + area + is_Midwest + is_Northeast + is_South, data = main_df)

# didreg_B_W <- lm(wage_gap_B_W ~ trt * time + edu_rate_B + edu_rate_W + emp_rate_B + emp_rate_W + gdp_per_capita + pop_share_W + pop_share_B + pop_share_F + density + poverty_rate_B + poverty_rate_W + is_Midwest + is_Northeast + is_South, data = main_df)
# didreg_A_W <- lm(wage_gap_A_W ~ trt * time + edu_rate_A + edu_rate_W + emp_rate_A + emp_rate_W + gdp_per_capita + pop_share_W + pop_share_A + pop_share_F + density + poverty_rate_A + poverty_rate_W + is_Midwest + is_Northeast + is_South, data = main_df)

# didreg_B_W <- lm(wage_gap_B_W ~ is_low * time, data = main_df[which(main_df$categories %in% c("low", "zero")),])
# didreg_A_W <- lm(wage_gap_A_W ~ is_low * time, data = main_df[which(main_df$categories %in% c("low", "zero")),])

didreg_B_W <- lm(wage_gap_B_W ~ trt * time, data = main_df)
didreg_A_W <- lm(wage_gap_A_W ~ trt * time, data = main_df)
summary(didreg_A_W)
summary(didreg_B_W)
summary(didreg_B_W)$coefficients[nrow(summary(didreg_B_W)$coefficients), "Pr(>|t|)"]
summary(didreg_A_W)$coefficients[nrow(summary(didreg_A_W)$coefficients), "Pr(>|t|)"]

