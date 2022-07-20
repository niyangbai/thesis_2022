rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)

# read data
load("data/main_cleaned_data.RData")

# log
nolog <- c("fips", "ID", "state", "year", 
           "wage_gap_B_W", "wage_gap_A_W", "time", "county",
           "trt", "fuzzy_trt", "region", "min_wage")
bo <- names(main_df)[unname(sapply(names(main_df), function(x) grepl("is_", x, fixed = TRUE)))]
nolog <- append(nolog, bo)
tolog <- names(main_df)[!(names(main_df) %in% nolog)]
main_df <- df_log(main_df, tolog)

# ignore inf
main_df <- do.call(data.frame, lapply(main_df, function(value) replace(value, is.infinite(value),NA)))

# reg
didreg_B_W <- lm(wage_gap_B_W ~ trt * time, data = main_df)
didreg_A_W <- lm(wage_gap_A_W ~ trt * time, data = main_df)
summary(didreg_B_W)
summary(didreg_A_W)