rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)

#read data
income_county_race <- read.csv("data/income_county_race.csv")

# label
library(Hmisc)
names(income_county_race)[match(paste0("B19013", LETTERS[1:9], "_001E"), names(income_county_race))] <- paste0("B19013", LETTERS[1:9])
label(income_county_race$B19013A) <- "White Alone"
label(income_county_race$B19013B) <- "Black or African American Alone"
label(income_county_race$B19013C) <- "American Indian and Alaska Native Alone"
label(income_county_race$B19013D) <- "Asian Alone"
label(income_county_race$B19013E) <- "Native Hawaiian and Other Pacific Islander Alone"
label(income_county_race$B19013F) <- "Some Other Race Alone"
label(income_county_race$B19013G) <- "Two or More Races"
label(income_county_race$B19013H) <- "White Alone, Not Hispanic or Latino"
label(income_county_race$B19013I) <- "Hispanic or Latino"

# subset
income_county_race <- income_county_race[,!(names(income_county_race) %in% c("B19013C", "B19013E", "B19013F", "B19013G", "B19013H", "B19013I"))]
income_10 <- income_county_race[which(income_county_race$year == 2010),]
income_17 <- income_county_race[which(income_county_race$year == 2017),]
income_county_race <- merge(income_10, income_17, by = "NAME")
income_county_race[income_county_race == -666666666] <- NA
income_county_race <- na.omit(income_county_race)
income_10 <- income_county_race[c("NAME", "year.x", "B19013A.x", "B19013B.x", "B19013D.x")]
income_17 <- income_county_race[c("NAME", "year.y", "B19013A.y", "B19013B.y", "B19013D.y")]
colnames(income_17) <- colnames(income_10)
income_county_race <- rbind(income_10, income_17)
wage_gap <- income_county_race["NAME"]
wage_gap$year <- income_county_race$year.x
wage_gap$B_W <- income_county_race$B19013B.x/income_county_race$B19013A.x
label(wage_gap$B_W) <- "Black and White wage gap"
wage_gap$A_W <- income_county_race$B19013D.x/income_county_race$B19013A.x
label(wage_gap$A_W) <- "Asian and White wage gap"
wage_gap$time <- wage_gap$year == 2017
label(wage_gap$time) <- "After treatment"

# separate
library(tidyr)
wage_gap <- separate(data = wage_gap, col = NAME, into = c("county", "state"), sep = ", ", remove = FALSE)

# min wage
minwage_state_year <- read.csv("data/minimum_wage.csv")
minwage_state_year <- subset(minwage_state_year, year %in% seq(2010, 2017))
minwage_state_year <- subset(minwage_state_year, select = c("year", "state", "State.Minimum.Wage"))
minwage_state_year_2010 <- subset(minwage_state_year, year == 2010)
names(minwage_state_year_2010)[names(minwage_state_year_2010) == "State.Minimum.Wage"] <- "2010.State.Minimum.Wage"
minwage_state_year_2017 <- subset(minwage_state_year, year == 2017)
names(minwage_state_year_2017)[names(minwage_state_year_2017) == "State.Minimum.Wage"] <- "2017.State.Minimum.Wage"
minwage_state_year <- merge(minwage_state_year_2010, minwage_state_year_2017, by = "state")
minwage_state_year <- minwage_state_year[,!(names(minwage_state_year) %in% c("year.x", "year.y"))]
minwage_state_year$dif <- round(minwage_state_year$`2017.State.Minimum.Wage` - minwage_state_year$`2010.State.Minimum.Wage`, 2)
minwage_state_year$trt <- !(minwage_state_year$dif == 0)
label(minwage_state_year$trt) <- "Treatment group"
# minwage_state_year <- onehotencoding(minwage_state_year, "dif")

# merge
df <- merge(wage_gap, minwage_state_year, by = "state")
df$min_wage <- NA
label(df$min_wage) <- "State Minimum Wage"
for (i in 1:nrow(df)) {
  if (df$year[i] == 2017) {
    df$min_wage[i] <- df$`2017.State.Minimum.Wage`[i]
  } else {
    df$min_wage[i] <- df$`2010.State.Minimum.Wage`[i]
  }
}
df <- df[,!(names(df) %in% c("2017.State.Minimum.Wage", "2010.State.Minimum.Wage", "dif"))]

# write csv
write.csv(df, "D:/github/thesis_2022/data/main_data.csv", row.names = FALSE)

# bachelor
bachelor_county_race <- read.csv("data/bachelor_county_race.csv")
names(bachelor_county_race)[match(paste0("B19301", LETTERS[1:9], "_001E"), names(bachelor_county_race))] <- paste0("B19301", LETTERS[1:9])
bachelor_county_race <- bachelor_county_race[,!(names(bachelor_county_race) %in% c("B19301C", "B19301E", "B19301F", "B19301G", "B19301H", "B19301I"))]

# # real
# df_real <- df
# df_real$coef <- df_real$Federal.Minimum.Wage.2020.Dollars/df_real$Federal.Minimum.Wage
# for (i in paste0("B19013", LETTERS[1:9])) {
#   df_real[i] <- df_real[i] * df_real$coef
# }
# names(df_real)[match(paste0("B19013", LETTERS[1:9]), names(df_real))] <- paste0("B19013", LETTERS[1:9], ".2020.Dollars")
# df_real = df_real[,!(names(df_real) %in% c("State.Minimum.Wage", "Federal.Minimum.Wage", "coef"))]

# # min wage difference
# df_10 <- subset(df, year == 2010)
# df_d <- subset(df, year == 2017)[c("state", "State.Minimum.Wage")]
# df_d$dif <- df_d$State.Minimum.Wage - df_10$State.Minimum.Wage
# df_d <- df_d[,!(names(df_d) == "State.Minimum.Wage")]
