rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)
library(Hmisc)
library(tidyr)

#read data
load("data/raw_data.RData")

# set time
before <- 2012
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

# min wage
minwage_state_year <- subset(minimum_wage, year %in% seq(2010, 2017))
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

# age
age_pop_county$ID <- paste0(age_pop_county$year, age_pop_county$fips)
age <- age_pop_county["ID"]
age$age_W <- age_pop_county$B01002A_001E
age$age_B <- age_pop_county$B01002B_001E
age$age_A <- age_pop_county$B01002D_001E

# education
highschool_county_race$ID <- paste0(highschool_county_race$year, highschool_county_race$fips)
edu <- highschool_county_race["ID"]
edu$edu_W <- highschool_county_race$C15002A_003E + highschool_county_race$C15002A_008E
edu$edu_B <- highschool_county_race$C15002B_003E + highschool_county_race$C15002B_008E
edu$edu_A <- highschool_county_race$C15002D_003E + highschool_county_race$C15002D_008E

# gdp
real_gdp_county <- subset(real_gdp_county, TimePeriod %in% c(before, after))
real_gdp_county[real_gdp_county == "character(0)"] <- NA
real_gdp_county$gdp <- real_gdp_county$DataValue
real_gdp_county <- real_gdp_county[,c("TimePeriod", "GeoFips", "gdp")]
real_gdp_county$ID <- paste0(real_gdp_county$TimePeriod, real_gdp_county$GeoFips)

# employment rate
colnames(total_unemp_county)[2:3] <- c("state", "county")
total_unemp_county$fips <- paste0(total_unemp_county$state, total_unemp_county$county)
real_gdp_county[real_gdp_county == "character(0)"] <- NA
total_unemp_county$uer <- total_unemp_county$`(%)`
total_unemp_county$ID <- paste0(total_unemp_county$Year, total_unemp_county$fips)
total_unemp_county <- total_unemp_county[,c("ID", "uer")]

race_emp_county$ID <- paste0(race_emp_county$year, race_emp_county$fips)
race_emp <- race_emp_county["ID"]
race_emp$emp_W <- race_emp_county$S2301_C03_012E
race_emp$emp_B <- race_emp_county$S2301_C03_013E
race_emp$emp_A <- race_emp_county$S2301_C03_015E
race_emp[race_emp == -666666666] <- NA

# poverty
poverty_county$ID <- paste0(poverty_county$year, poverty_county$fips)
poverty <- poverty_county["ID"]
poverty$poverty_W <- poverty_county$B17001A_001E
poverty$poverty_B <- poverty_county$B17001B_001E
poverty$poverty_A <- poverty_county$B17001D_001E
poverty$poverty_T <- poverty_county$B17001_001E

# Ethical population
race_pop_county$ID <- paste0(race_pop_county$year, race_pop_county$fips)
race_pop <- race_pop_county["ID"]
race_pop$pop_W <- race_pop_county$B01001A_001E
race_pop$pop_B <- race_pop_county$B01001B_001E
race_pop$pop_A <- race_pop_county$B01001D_001E
race_pop$pop_T <- race_pop_county$B01001_001E
race_pop$pop_F <- race_pop_county$B01001_026E

# county area
area <- landarea[c("fips", "area")]

# merge
df <- merge(wage_gap, minwage_state_year, by = "state")
df <- merge(df, edu, by = "ID")
df <- merge(df, real_gdp_county, by = "ID")
df <- merge(df, total_unemp_county, by = "ID")
df <- merge(df, race_pop, by = "ID")
df <- merge(df, race_emp, by = "ID")
df <- merge(df, age, by = "ID")
df <- merge(df, poverty, by = "ID")
df <- merge(df, area, by = "fips")
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
df$density <- df$pop_T / df$area

# pop share
df$share_W <- df$pop_W / df$pop_T
df$share_B <- df$pop_B / df$pop_T
df$share_A <- df$pop_A / df$pop_T
df$share_F <- df$pop_F / df$pop_T

# poverty rate by race
df$po_rate_T <- df$poverty_T / df$pop_T
df$po_rate_W <- df$poverty_W / df$pop_W
df$po_rate_B <- df$poverty_B / df$pop_B
df$po_rate_A <- df$poverty_A / df$pop_A

# edu rate by race
df$edu_rate_A <- df$edu_A / df$pop_A
df$edu_rate_B <- df$edu_B / df$pop_B
df$edu_rate_W <- df$edu_W / df$pop_W

# region dummy
df$region <- sapply(df$fips, fips2region)
df <- onehotencoding(df, "region")

# label
label(df$wage_gap_B_W) <- "Black and White wage gap"
label(df$wage_gap_A_W) <- "Asian and White wage gap"
label(df$time) <- "After treatment"
label(df$edu_B) <- "High School (Black)"
label(df$edu_A) <- "High School (Asian)"
label(df$edu_W) <- "High School (White)"
label(df$edu_rate_B) <- "High School Rate (Black)"
label(df$edu_rate_A) <- "High School Rate (Asian)"
label(df$edu_rate_W) <- "High School Rate (White)"
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
label(df$pop_F) <- "Female Population"
label(df$area) <- "State Area (sq mi)"
label(df$density) <- "Population Density"
label(df$emp_W) <- "Employment rate (White)"
label(df$emp_B) <- "Employment rate (Black)"
label(df$emp_A) <- "Employment rate (Asian)"
label(df$age_W) <- "Median Age (White)"
label(df$age_B) <- "Median Age (Black)"
label(df$age_A) <- "Median Age (Asian)"
label(df$share_W) <- "White Share"
label(df$share_B) <- "Black Share"
label(df$share_A) <- "Asian Share"
label(df$share_F) <- "Female Share"
label(df$poverty_W) <- "Poverty (White)"
label(df$poverty_B) <- "Poverty (Black)"
label(df$poverty_A) <- "Poverty (Asian)"
label(df$poverty_T) <- "Poverty (Total)"
label(df$po_rate_T) <- "Poverty Rate (Total)"
label(df$po_rate_W) <- "Poverty Rate (White)"
label(df$po_rate_B) <- "Poverty Rate (Black)"
label(df$po_rate_A) <- "Poverty Rate (Asian)"

# write csv
save(df, file = "D:/github/wagegap22/data/cleaned_data.RData")
