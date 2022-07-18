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
# minwage_state_year <- onehotencoding(minwage_state_year, "dif")

# education
names(highschool_county_race)[match(paste0("C15002", LETTERS[c(1 ,2, 4)], "_004E"), names(highschool_county_race))] <- paste0("C15002", LETTERS[c(1 ,2, 4)])
highschool_county_race[highschool_county_race == -666666666] <- NA
edu_county <- highschool_county_race["year"]
edu_county$ID <- paste0(highschool_county_race$year, highschool_county_race$fips)
edu_county$edu_B <- highschool_county_race$C15002B
edu_county$edu_A <- highschool_county_race$C15002D
edu_county$edu_W <- highschool_county_race$C15002A

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

# Ethical population
race_pop_county$ID <- paste0(race_pop_county$year, race_pop_county$fips)
race_pop <- race_pop_county["ID"]
race_pop$pop_W <- race_pop_county$DP05_0037E
race_pop$pop_B <- race_pop_county$DP05_0038E
race_pop$pop_A <- race_pop_county$DP05_0044E
race_pop$pop_T <- race_pop_county$DP05_0001E

# merge
df <- merge(wage_gap, minwage_state_year, by = "state")
df <- merge(df, edu_county, by = "ID")
df <- merge(df, real_gdp_county, by = "ID")
df <- merge(df, total_unemp_county, by = "ID")
df <- merge(df, total_pop_county, by = "ID")
df <- merge(df, race_pop, by = "ID")
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
df$gdp_per_capita <- df$gdp / df$pop_T

# density
df$area <- as.numeric(sapply(df$state, state2area))
df$density <- df$pop_T / df$area

# pop share
df$share_W <- df$pop_W / df$pop_T
df$share_B <- df$pop_B / df$pop_T
df$share_A <- df$pop_A / df$pop_T

# high school rate
df$edur_A <- df$edu_A / df$pop_A
df$edur_B <- df$edu_B / df$pop_B
df$edur_W <- df$edu_W / df$pop_W

# label
label(df$wage_gap_B_W) <- "Black and White wage gap"
label(df$wage_gap_A_W) <- "Asian and White wage gap"
label(df$time) <- "After treatment"
label(df$edu_B) <- "High School (Black)"
label(df$edu_A) <- "High School (Asian)"
label(df$edu_W) <- "High School (White)"
label(df$gdp) <- "Total Real GDP (2012)"
label(df$uer) <- "Total unemployment rate"
label(df$min_wage) <- "State Minimum Wage"
label(df$trt) <- "Treatment group"
label(df$fuzzy_trt) <- "Fuzzy Treatment"
label(df$pop_T) <- "Total Population"
label(df$gdp_per_capita) <- "GDP per Capita (2012)"
label(df$pop_W) <- "White Population"
label(df$pop_B) <- "Black Population"
label(df$pop_A) <- "Asian Population"
label(df$area) <- "State Area (sq mi)"
label(df$density) <- "Population Density"
label(df$uer_W) <- "Unemployment rate (White)"
label(df$uer_B) <- "Unemployment rate (Black)"
label(df$uer_A) <- "Unemployment rate (Asian)"
label(df$age_W) <- "Median Age (White)"
label(df$age_B) <- "Median Age (Black)"
label(df$age_A) <- "Median Age (Asian)"
label(df$share_W) <- "White Share"
label(df$share_B) <- "Black Share"
label(df$share_A) <- "Asian Share"
label(df$edur_A) <- "Asian High School Rate"
label(df$edur_B) <- "Black High School Rate"
label(df$edur_W) <- "White High School Rate"

# write csv
save(df, file = "D:/github/thesis_2022/data/cleaned_data.RData")
