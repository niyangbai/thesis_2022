rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)
library(Hmisc)
library(tidyr)
library(dplyr)

# set time
before <- 2010
treatment <- 2013
after <- 2020

#read data
load("data/raw_data.RData")

# subset
bf_median_earning <- median_earning_county[which(median_earning_county$year == before),]
trt_median_earning <- median_earning_county[which(median_earning_county$year == treatment),]
af_median_earning <- median_earning_county[which(median_earning_county$year == after),]
median_earning_county <- merge(bf_median_earning, trt_median_earning, by = "fips")
median_earning_county <- merge(median_earning_county, af_median_earning, by = "fips")
median_earning_county[median_earning_county == -666666666] <- NA
# median_earning_county[median_earning_county == 2499] <- NA
# median_earning_county[median_earning_county == 250001] <- NA
median_earning_county <- na.omit(median_earning_county)
bf_median_earning <- median_earning_county[c("fips", "year.x", "B20017A_001E.x", "B20017B_001E.x", "B20017D_001E.x")]
trt_median_earning <- median_earning_county[c("fips", "year.y", "B20017A_001E.y", "B20017B_001E.y", "B20017D_001E.y")]
af_median_earning <- median_earning_county[c("fips", "year", "B20017A_001E", "B20017B_001E", "B20017D_001E")]
colnames(trt_median_earning) <- colnames(af_median_earning)
colnames(bf_median_earning) <- colnames(af_median_earning)
median_earning_county <- rbind(bf_median_earning, trt_median_earning, af_median_earning)
wage_gap <- median_earning_county[c("fips", "year")]
wage_gap$state <- sapply(wage_gap$fips, fips2state)
wage_gap$county <- sapply(wage_gap$fips, fips2county)
wage_gap$wage_gap_B_W <- median_earning_county$B20017B_001E / median_earning_county$B20017A_001E
wage_gap$wage_gap_A_W <- median_earning_county$B20017D_001E / median_earning_county$B20017A_001E
wage_gap$ID <- paste0(wage_gap$year, wage_gap$fips)


# min wage
minwage_state_year <- subset(minimum_wage, year %in% seq(2010, 2020))
control_states <- find_difs(before, after)[find_difs(before, after)[["dif"]] == 0, "state"]
treatment_states <- find_difs(before, after)[(find_difs(before, treatment)[["dif"]] == 0) & !(find_difs(treatment, after)[["dif"]] == 0), "state"]

# age
age_pop_county$ID <- paste0(age_pop_county$year, age_pop_county$fips)
age_pop_county[age_pop_county == -666666666] <- NA
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
real_gdp_county <- subset(real_gdp_county, TimePeriod %in% seq(2010, 2020))
real_gdp_county$ID <- paste0(real_gdp_county$TimePeriod, real_gdp_county$GeoFips)
real_gdp_county$gdp <- real_gdp_county$DataValue
real_gdp_county <- real_gdp_county[,c("ID", "gdp")]


# labor force
race_emp_county$ID <- paste0(race_emp_county$year, race_emp_county$fips)
race_emp <- race_emp_county["ID"]
race_emp$emp_W <- race_emp_county$S2301_C03_012E
race_emp$emp_B <- race_emp_county$S2301_C03_013E
race_emp$emp_A <- race_emp_county$S2301_C03_015E
race_emp[race_emp == -666666666] <- NA

# poverty
poverty_county$ID <- paste0(poverty_county$year, poverty_county$fips)
poverty <- poverty_county["ID"]
poverty$poverty_W <- poverty_county$B17001A_002E
poverty$poverty_B <- poverty_county$B17001B_002E
poverty$poverty_A <- poverty_county$B17001D_002E
poverty$poverty_T <- poverty_county$B17001_002E

# Ethical population
race_pop_county$ID <- paste0(race_pop_county$year, race_pop_county$fips)
race_pop <- race_pop_county["ID"]
race_pop$pop_W <- race_pop_county$B01001A_001E
race_pop$pop_B <- race_pop_county$B01001B_001E
race_pop$pop_A <- race_pop_county$B01001D_001E
race_pop$pop_T <- race_pop_county$B01001_001E
race_pop$pop_F <- race_pop_county$B01001_026E
race_pop$pop_F_W <- race_pop_county$B01001A_026E
race_pop$pop_F_B <- race_pop_county$B01001B_026E
race_pop$pop_F_A <- race_pop_county$B01001D_026E

# county area
area <- landarea[c("fips", "area")]

# merge
df <- merge(wage_gap, edu, by = "ID", all.x = TRUE)
df <- merge(df, real_gdp_county, by = "ID", all.x = TRUE)
df <- merge(df, race_pop, by = "ID", all.x = TRUE)
df <- merge(df, race_emp, by = "ID", all.x = TRUE)
df <- merge(df, age, by = "ID", all.x = TRUE)
df <- merge(df, poverty, by = "ID", all.x = TRUE)
df <- merge(df, area, by = "fips", all.x = TRUE)
df <- df[!is.na(df$gdp),]

# region dummy
df$region <- sapply(df$fips, fips2region)

# calculation
df$emp_A <- df$emp_A * df$pop_A / 100
df$emp_W <- df$emp_W * df$pop_W / 100
df$emp_B <- df$emp_B * df$pop_B / 100

# one hot encoding
df <- onehotencoding(df, "region")

# sign treatment group
df$treatment <- df$state %in% treatment_states

# label
label(df$wage_gap_B_W) <- "Black and White wage gap"
label(df$wage_gap_A_W) <- "Asian and White wage gap"
label(df$edu_B) <- "High School (Black)"
label(df$edu_A) <- "High School (Asian)"
label(df$edu_W) <- "High School (White)"
label(df$emp_W) <- "Employment (White)"
label(df$emp_A) <- "Employment (Asian)"
label(df$emp_B) <- "Employment (Black)"
label(df$gdp) <- "Total Real GDP (2012)"
label(df$pop_T) <- "Total Population"
label(df$pop_W) <- "White Population"
label(df$pop_B) <- "Black Population"
label(df$pop_A) <- "Asian Population"
label(df$pop_F) <- "Female Population"
label(df$pop_F_W) <- "Female Population (White)"
label(df$pop_F_B) <- "Female Population (Black)"
label(df$pop_F_A) <- "Female Population (Asian)"
label(df$area) <- "State Area (sq mi)"
label(df$age_W) <- "Median Age (White)"
label(df$age_B) <- "Median Age (Black)"
label(df$age_A) <- "Median Age (Asian)"
label(df$poverty_W) <- "Poverty (White)"
label(df$poverty_B) <- "Poverty (Black)"
label(df$poverty_A) <- "Poverty (Asian)"
label(df$poverty_T) <- "Poverty (Total)"
label(df$region) <- "Geografical Region"
label(df$treatment) <- "Treatment group"


# # calculation
# df$edu_rate_B <- df$edu_B / df$pop_B
# df$edu_rate_W <- df$edu_W / df$pop_W
# df$edu_rate_A <- df$edu_A / df$pop_A
# df$emp_rate_B <- df$emp_B / df$pop_B
# df$emp_rate_A <- df$emp_A / df$pop_A
# df$emp_rate_W <- df$emp_W / df$pop_W
# df$gdp_per_capita <- df$gdp / df$pop_T
# df$pop_share_W <- df$pop_W / df$pop_T
# df$pop_share_A <- df$pop_A / df$pop_T
# df$pop_share_B <- df$pop_B / df$pop_T
# df$pop_share_F <- df$pop_F / df$pop_T
# df$pop_share_F_W <- df$pop_F_W / df$pop_W
# df$pop_share_F_B <- df$pop_F_B / df$pop_B
# df$pop_share_F_A <- df$pop_F_A / df$pop_A
# df$density <- df$area / df$pop_T
# df$poverty_rate_A <- df$poverty_A / df$pop_A
# df$poverty_rate_W <- df$poverty_W / df$pop_W
# df$poverty_rate_B <- df$poverty_B / df$pop_B
# df$poverty_rate_T <- df$poverty_T / df$pop_T

# write csv
save(df, before, treatment, after, file = "D:/github/wagegap22/data/cleaned_data.RData")