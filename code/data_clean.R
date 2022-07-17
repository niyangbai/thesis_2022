rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)
library(Hmisc)
library(tidyr)

#read data
load("data/raw_data.RData")

# label
names(median_household_income_county_race)[match(paste0("B19013", LETTERS[1:9], "_001E"), names(median_household_income_county_race))] <- paste0("B19013", LETTERS[1:9])
label(median_household_income_county_race$B19013A) <- "White Alone"
label(median_household_income_county_race$B19013B) <- "Black or African American Alone"
label(median_household_income_county_race$B19013C) <- "American Indian and Alaska Native Alone"
label(median_household_income_county_race$B19013D) <- "Asian Alone"
label(median_household_income_county_race$B19013E) <- "Native Hawaiian and Other Pacific Islander Alone"
label(median_household_income_county_race$B19013F) <- "Some Other Race Alone"
label(median_household_income_county_race$B19013G) <- "Two or More Races"
label(median_household_income_county_race$B19013H) <- "White Alone, Not Hispanic or Latino"
label(median_household_income_county_race$B19013I) <- "Hispanic or Latino"

# subset
median_household_income_county_race <- median_household_income_county_race[,!(names(median_household_income_county_race) %in% c("B19013C", "B19013E", "B19013F", "B19013G", "B19013H", "B19013I"))]
income_10 <- median_household_income_county_race[which(median_household_income_county_race$year == 2010),]
income_17 <- median_household_income_county_race[which(median_household_income_county_race$year == 2017),]
median_household_income_county_race <- merge(income_10, income_17, by = "NAME")
median_household_income_county_race[median_household_income_county_race == -666666666] <- NA
median_household_income_county_race <- na.omit(median_household_income_county_race)
income_10 <- median_household_income_county_race[c("NAME", "year.x", "B19013A.x", "B19013B.x", "B19013D.x")]
income_17 <- median_household_income_county_race[c("NAME", "year.y", "B19013A.y", "B19013B.y", "B19013D.y")]
colnames(income_17) <- colnames(income_10)
median_household_income_county_race <- rbind(income_10, income_17)
wage_gap <- median_household_income_county_race["NAME"]
wage_gap$year <- median_household_income_county_race$year.x
wage_gap$wage_gap_B_W <- median_household_income_county_race$B19013B.x/median_household_income_county_race$B19013A.x
label(wage_gap$wage_gap_B_W) <- "Black and White wage gap"
wage_gap$wage_gap_A_W <- median_household_income_county_race$B19013D.x/median_household_income_county_race$B19013A.x
label(wage_gap$wage_gap_A_W) <- "Asian and White wage gap"
wage_gap$time <- wage_gap$year == 2017
label(wage_gap$time) <- "After treatment"

# separate
wage_gap <- separate(data = wage_gap, col = NAME, into = c("county", "state"), sep = ", ", remove = FALSE)
wage_gap$NAME <- paste(wage_gap$year, "-", wage_gap$NAME, sep = "")

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

# bachelor
names(bachelor_county_race)[match(paste0("B19301", LETTERS[1:9], "_001E"), names(bachelor_county_race))] <- paste0("B19301", LETTERS[1:9])
bachelor_county_race <- bachelor_county_race[,!(names(bachelor_county_race) %in% c("B19301C", "B19301E", "B19301F", "B19301G", "B19301H", "B19301I"))]
bachelor_county_race[bachelor_county_race == -666666666] <- NA
bachelor_county_race <- na.omit(bachelor_county_race)
edu <- bachelor_county_race["year"]
edu$NAME <- paste(bachelor_county_race$year, "-", bachelor_county_race$NAME, sep = "")
edu$edu_B_W <- bachelor_county_race$B19301B / bachelor_county_race$B19301A
label(edu$edu_B_W) <- "Black and white education gap"
edu$edu_A_W <- bachelor_county_race$B19301D / bachelor_county_race$B19301A
label(edu$edu_A_W) <- "Asian and white education gap"

# gdp
gdp_county <- subset(gdp_county, TimePeriod %in% c(2010, 2017))
gdp_county$NAME <- lapply(gdp_county$GeoFips, fips2name)
gdp_county[gdp_county == "character(0)"] <- NA
gdp_county$gdp <- gdp_county$DataValue
label(gdp_county$gdp) <- "GDP change"
gdp_county <- gdp_county[,c("TimePeriod", "NAME", "gdp")]
gdp_county <- na.omit(gdp_county)
gdp_county$NAME <- paste0(gdp_county$TimePeriod, "-", gdp_county$NAME)

# merge
df <- merge(wage_gap, minwage_state_year, by = "state")
df <- merge(df, edu, by = "NAME")
df <- merge(df, gdp_county, by = "NAME")
df$year <- df$year.x
df <- df[,!(names(df) %in% c("year.x", "year.y", "TimePeriod"))]
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
save(df, file = "D:/github/thesis_2022/data/cleaned_data.RData")
