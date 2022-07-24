rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)

# read data
load("data/cleaned_data.RData")

#log
nolog <- c("fips", "ID", "county", "year", "state", "region",
           "wage_gap_B_W", "wage_gap_A_W",
           "treatment")
bo <- names(df)[unname(sapply(names(df), function(x) grepl("is_", x, fixed = TRUE)))]
nolog <- append(nolog, bo)
tolog <- names(df)[!(names(df) %in% nolog)]
for (i in tolog) {
  df[i] <- log(df[i])
}

# ignore inf
df <- do.call(data.frame, lapply(df, function(value) replace(value, is.infinite(value), NA)))

# omit NA
df <- na.omit(df)

# sign groups
main_df <- df[df$year %in% c(treatment, after),]
placebo_df <- df[df$year %in% c(before, treatment),]
main_df$time <- main_df$year == after
placebo_df$time <- placebo_df$year == treatment
label(main_df$time) <- "After treatment"
label(placebo_df$time) <- "After FAKE treatment"

# regression
didreg_B_W <- lm(wage_gap_B_W ~ treatment * time, data = main_df)
didreg_A_W <- lm(wage_gap_A_W ~ treatment * time, data = main_df)

# didreg_B_W <- lm(wage_gap_B_W ~ treatment * time, data = main_df)
# didreg_A_W <- lm(wage_gap_A_W ~ treatment * time, data = main_df)
summary(didreg_A_W)
summary(didreg_B_W)
summary(didreg_B_W)$coefficients[nrow(summary(didreg_B_W)$coefficients), "Pr(>|t|)"]
summary(didreg_A_W)$coefficients[nrow(summary(didreg_A_W)$coefficients), "Pr(>|t|)"]

