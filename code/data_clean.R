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
bf_median_earning <- median_earning_county[which(median_earning_county$year == before),]
af_median_earning <- median_earning_county[which(median_earning_county$year == after),]
median_earning_county <- merge(bf_median_earning, af_median_earning, by = "fips")
median_earning_county[median_earning_county == -666666666] <- NA
median_earning_county <- na.omit(median_earning_county)
bf_median_earning <- median_earning_county[c("fips", "year.x", "B20017A_001E.x", "B20017B_001E.x", "B20017D_001E.x")]
af_median_earning <- median_earning_county[c("fips", "year.y", "B20017A_001E.y", "B20017B_001E.y", "B20017D_001E.y")]
colnames(af_median_earning) <- colnames(bf_median_earning)
median_earning_county <- rbind(af_median_earning, bf_median_earning)
wage_gap <- median_earning_county["fips"]
wage_gap$year <- median_earning_county$year.x
wage_gap$wage_gap_B_W <- median_earning_county$B20017B_001E.x / median_earning_county$B20017A_001E.x
wage_gap$wage_gap_A_W <- median_earning_county$B20017D_001E.x / median_earning_county$B20017A_001E.x
wage_gap$time <- wage_gap$year == after
wage_gap$ID <- paste0(wage_gap$year, wage_gap$fips)
wage_gap$state <- sapply(wage_gap$fips, fips2state)
wage_gap$county <- sapply(wage_gap$fips, fips2county)

# age
age_pop_county$ID <- paste0(age_pop_county$year, age_pop_county$fips)
age <- age_pop_county["ID"]
age$age_W <- age_pop_county$B01002A_001E
age$age_B <- age_pop_county$B01002B_001E
age$age_A <- age_pop_county$B01002D_001E

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
minwage_state_year$fuzzy_trt <- minwage_state_year$dif / max(minwage_state_year$dif)
minwage_state_year <- onehotencoding(minwage_state_year, "dif")

# bachelor
names(bachelor_county_race)[match(paste0("B19301", LETTERS[c(1 ,2, 4)], "_001E"), names(bachelor_county_race))] <- paste0("B19301", LETTERS[c(1 ,2, 4)])
bachelor_county_race[bachelor_county_race == -666666666] <- NA
edu_county <- bachelor_county_race["year"]
edu_county$ID <- paste0(bachelor_county_race$year, bachelor_county_race$fips)
edu_county$edu_B_W <- abs(bachelor_county_race$B19301B / bachelor_county_race$B19301A - 1)
edu_county$edu_A_W <- abs(bachelor_county_race$B19301D / bachelor_county_race$B19301A - 1)

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
df <- merge(df, age, by = "ID")
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
label(df$edu_B_W) <- "Black and white edu_countycation gap"
label(df$edu_A_W) <- "Asian and white edu_countycation gap"
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
label(df$age_W) <- "Median Age (White)"
label(df$age_B) <- "Median Age (Black)"
label(df$age_A) <- "Median Age (Asian)"

# write csv
save(df, file = "D:/github/thesis_2022/data/cleaned_data.RData")
