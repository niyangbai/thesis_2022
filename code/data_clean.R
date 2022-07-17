rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)
library(Hmisc)
library(tidyr)

#read data
load("data/raw_data.RData")

# set time
before <- 2010
after <- 2017

# subset
names(median_household_income_county_race)[match(paste0("B19013", LETTERS[c(1 ,2, 4)], "_001E"), names(median_household_income_county_race))] <- paste0("B19013", LETTERS[c(1 ,2, 4)])
label(median_household_income_county_race$B19013A) <- "White Alone"
label(median_household_income_county_race$B19013B) <- "Black or African American Alone"
label(median_household_income_county_race$B19013D) <- "Asian Alone"

income_before <- median_household_income_county_race[which(median_household_income_county_race$year == before),]
income_after <- median_household_income_county_race[which(median_household_income_county_race$year == after),]
median_household_income_county_race <- merge(income_before, income_after, by = "fips")
median_household_income_county_race[median_household_income_county_race == -666666666] <- NA
median_household_income_county_race <- na.omit(median_household_income_county_race)
income_before <- median_household_income_county_race[c("fips", "year.x", "B19013A.x", "B19013B.x", "B19013D.x")]
income_after <- median_household_income_county_race[c("fips", "year.y", "B19013A.y", "B19013B.y", "B19013D.y")]
colnames(income_after) <- colnames(income_before)
median_household_income_county_race <- rbind(income_before, income_after)
wage_gap <- median_household_income_county_race["fips"]
wage_gap$year <- median_household_income_county_race$year.x
wage_gap$wage_gap_B_W <- median_household_income_county_race$B19013B.x/median_household_income_county_race$B19013A.x
wage_gap$wage_gap_A_W <- median_household_income_county_race$B19013D.x/median_household_income_county_race$B19013A.x
wage_gap$time <- wage_gap$year == after
wage_gap$ID <- paste0(wage_gap$year, wage_gap$fips)
wage_gap$state <- sapply(wage_gap$fips, fips2state)
wage_gap$county <- sapply(wage_gap$fips, fips2county)

# min wage
minwage_state_year <- read.csv("data/minimum_wage.csv")
minwage_state_year <- subset(minwage_state_year, year %in% seq(2010, 2017))
minwage_state_year <- subset(minwage_state_year, select = c("year", "state", "State.Minimum.Wage"))
minwage_state_year_before <- subset(minwage_state_year, year == before)
names(minwage_state_year_before)[names(minwage_state_year_before) == "State.Minimum.Wage"] <- "before.State.Minimum.Wage"
minwage_state_year_after <- subset(minwage_state_year, year == after)
names(minwage_state_year_after)[names(minwage_state_year_after) == "State.Minimum.Wage"] <- "after.State.Minimum.Wage"
minwage_state_year <- merge(minwage_state_year_before, minwage_state_year_after, by = "state")
minwage_state_year <- minwage_state_year[,!(names(minwage_state_year) %in% c("year.x", "year.y"))]
minwage_state_year$dif <- round(minwage_state_year$`after.State.Minimum.Wage` - minwage_state_year$`before.State.Minimum.Wage`, 2)
minwage_state_year$trt <- !(minwage_state_year$dif == 0)
# minwage_state_year <- onehotencoding(minwage_state_year, "dif")

# bachelor
names(bachelor_county_race)[match(paste0("B19301", LETTERS[c(1 ,2, 4)], "_001E"), names(bachelor_county_race))] <- paste0("B19301", LETTERS[c(1 ,2, 4)])
bachelor_county_race[bachelor_county_race == -666666666] <- NA
edu_county <- bachelor_county_race["year"]
edu_county$ID <- paste0(bachelor_county_race$year, bachelor_county_race$fips)
edu_county$edu_county_B_W <- bachelor_county_race$B19301B / bachelor_county_race$B19301A
edu_county$edu_county_A_W <- bachelor_county_race$B19301D / bachelor_county_race$B19301A

# gdp
real_gdp_county <- subset(real_gdp_county, TimePeriod %in% c(before, after))
real_gdp_county[real_gdp_county == "character(0)"] <- NA
real_gdp_county$gdp <- real_gdp_county$DataValue
real_gdp_county <- real_gdp_county[,c("TimePeriod", "GeoFips", "gdp")]
real_gdp_county$ID <- paste0(real_gdp_county$TimePeriod, real_gdp_county$GeoFips)

# unemployment rate
colnames(total_unemp_county)[2:3] <- c("state", "county")
total_unemp_county$fips <- paste0(total_unemp_county$state, total_unemp_county$county)
real_gdp_county[real_gdp_county == "character(0)"] <- NA
total_unemp_county$uer <- total_unemp_county$`(%)`
total_unemp_county$ID <- paste0(total_unemp_county$Year, total_unemp_county$fips)
total_unemp_county <- total_unemp_county[,c("ID", "uer")]
race_unemp_county$ID <- paste0(race_unemp_county$year, race_unemp_county$fips)
race_uer <- race_unemp_county["ID"]
race_uer$uer_W <- race_unemp_county$S2301_C04_012E
race_uer$uer_B <- race_unemp_county$S2301_C04_013E
race_uer$uer_A <- race_unemp_county$S2301_C04_015E
race_uer[race_uer == -666666666] <- NA

# Total population
total_pop_county$ID <- paste0(total_pop_county$year, total_pop_county$fips)
total_pop_county$total_pop <- total_pop_county$DP05_0001E
total_pop_county <- total_pop_county[c("ID", "total_pop")]

# Ethical population share
race_pop_share_county$ID <- paste0(race_pop_share_county$year, race_pop_share_county$fips)
race_share <- race_pop_share_county["ID"]
race_share$pop_W <- race_pop_share_county$DP05_0037PE
race_share$pop_B <- race_pop_share_county$DP05_0038PE
race_share$pop_A <- race_pop_share_county$DP05_0044PE

# merge
df <- merge(wage_gap, minwage_state_year, by = "state")
df <- merge(df, edu_county, by = "ID")
df <- merge(df, real_gdp_county, by = "ID")
df <- merge(df, total_unemp_county, by = "ID")
df <- merge(df, total_pop_county, by = "ID")
df <- merge(df, race_share, by = "ID")
df <- merge(df, race_uer, by = "ID")
df$year <- df$year.x
df <- df[,!(names(df) %in% c("year.x", "year.y", "TimePeriod", "GeoFips"))]
df$min_wage <- NA
for (i in 1:nrow(df)) {
  if (df$year[i] == after) {
    df$min_wage[i] <- df$`after.State.Minimum.Wage`[i]
  } else {
    df$min_wage[i] <- df$`before.State.Minimum.Wage`[i]
  }
}
df <- df[,!(names(df) %in% c("after.State.Minimum.Wage", "before.State.Minimum.Wage", "dif"))]

# calculation
# gdp per capita
df$gdp_per_capita <- df$gdp / df$total_pop

# density
df$area <- as.numeric(sapply(df$state, state2area))
df$density <- df$total_pop / df$area

# label
label(df$wage_gap_B_W) <- "Black and White wage gap"
label(df$wage_gap_A_W) <- "Asian and White wage gap"
label(df$time) <- "After treatment"
label(df$edu_county_B_W) <- "Black and white edu_countycation gap"
label(df$edu_county_A_W) <- "Asian and white edu_countycation gap"
label(df$gdp) <- "Total Real GDP (2012)"
label(df$uer) <- "Total unemployment rate"
label(df$min_wage) <- "State Minimum Wage"
label(df$trt) <- "Treatment group"
label(df$total_pop) <- "Total Population"
label(df$gdp_per_capita) <- "GDP per Capita (2012)"
label(df$pop_W) <- "White Population Share"
label(df$pop_B) <- "Black Population Share"
label(df$pop_A) <- "Asian Population Share"
label(df$pop_A) <- "Asian Population Share"
label(df$area) <- "State Area (sq mi)"
label(df$density) <- "Population Density"
label(df$uer_W) <- "Unemployment rate (White)"
label(df$uer_B) <- "Unemployment rate (Black)"
label(df$uer_A) <- "Unemployment rate (Asian)"

# write csv
save(df, file = "D:/github/thesis_2022/data/cleaned_data.RData")
