rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)
library(Hmisc)
library(tidyr)
library(dplyr)

# !12-16
# 11-16, 10-15, 16-19
# 16-20, 10-16
# set time
before <- 2010
after <- 2016

#read data
load("data/raw_data.RData")

# subset
bf_median_earning <- median_earning_county[which(median_earning_county$year == before),]
af_median_earning <- median_earning_county[which(median_earning_county$year == after),]
median_earning_county <- merge(bf_median_earning, af_median_earning, by = "fips")
median_earning_county[median_earning_county == -666666666] <- NA
median_earning_county[median_earning_county == 2499] <- NA
median_earning_county[median_earning_county == 250001] <- NA
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
minwage_state_year <- subset(minimum_wage, year %in% seq(before, after))
minwage_state_year <- subset(minwage_state_year, select = c("year", "state", "State.Minimum.Wage"))
minwage_state_year_before <- subset(minwage_state_year, year == before)
names(minwage_state_year_before)[names(minwage_state_year_before) == "State.Minimum.Wage"] <- "before.State.Minimum.Wage"
minwage_state_year_after <- subset(minwage_state_year, year == after)
names(minwage_state_year_after)[names(minwage_state_year_after) == "State.Minimum.Wage"] <- "after.State.Minimum.Wage"
minwage_state_year <- merge(minwage_state_year_before, minwage_state_year_after, by = "state")
minwage_state_year <- minwage_state_year[,!(names(minwage_state_year) %in% c("year.x", "year.y"))]
minwage_state_year$dif <- round(minwage_state_year$`after.State.Minimum.Wage` - minwage_state_year$`before.State.Minimum.Wage`, 2)
minwage_state_year$trt <- !(minwage_state_year$dif == 0)
# minwage_state_year$fuzzy_trt <- minwage_state_year$dif / max(minwage_state_year$dif)
minwage_state_year$categories <- cut(minwage_state_year$dif, breaks = c(-1, -0.001, 0, 1, 2, Inf), labels = c("low", "zero", "low", "medium", "high"))
minwage_state_year <- onehotencoding(minwage_state_year, "categories")

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
real_gdp_county <- subset(real_gdp_county, TimePeriod %in% c(before, after))
real_gdp_county$gdp <- real_gdp_county$DataValue
real_gdp_county <- real_gdp_county[,c("TimePeriod", "GeoFips", "gdp")]
real_gdp_county$ID <- paste0(real_gdp_county$TimePeriod, real_gdp_county$GeoFips)

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
df <- merge(wage_gap, minwage_state_year, by = "state")
df <- merge(df, edu, by = "ID", all.x = TRUE)
df <- merge(df, real_gdp_county, by = "ID", all.x = TRUE)
df <- merge(df, race_pop, by = "ID", all.x = TRUE)
df <- merge(df, race_emp, by = "ID", all.x = TRUE)
df <- merge(df, age, by = "ID", all.x = TRUE)
df <- merge(df, poverty, by = "ID", all.x = TRUE)
df <- merge(df, area, by = "fips", all.x = TRUE)
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

# region dummy
df$region <- sapply(df$fips, fips2region)

# calculation
df$emp_A <- df$emp_A * df$pop_A / 100
df$emp_W <- df$emp_W * df$pop_W / 100
df$emp_B <- df$emp_B * df$pop_B / 100

# one hot encoding
df <- onehotencoding(df, "region")

# label
label(df$wage_gap_B_W) <- "Black and White wage gap"
label(df$wage_gap_A_W) <- "Asian and White wage gap"
label(df$time) <- "After treatment"
label(df$edu_B) <- "High School (Black)"
label(df$edu_A) <- "High School (Asian)"
label(df$edu_W) <- "High School (White)"
label(df$emp_W) <- "Employment (White)"
label(df$emp_A) <- "Employment (Asian)"
label(df$emp_B) <- "Employment (Black)"
label(df$gdp) <- "Total Real GDP (2012)"
label(df$min_wage) <- "State Minimum Wage"
label(df$trt) <- "Treatment group"
label(df$fuzzy_trt) <- "Fuzzy Treatment"
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
main_df <- df

# # calculation
# main_df$edu_rate_B <- main_df$edu_B / main_df$pop_B
# main_df$edu_rate_W <- main_df$edu_W / main_df$pop_W
# main_df$edu_rate_A <- main_df$edu_A / main_df$pop_A
# main_df$emp_rate_B <- main_df$emp_B / main_df$pop_B
# main_df$emp_rate_A <- main_df$emp_A / main_df$pop_A
# main_df$emp_rate_W <- main_df$emp_W / main_df$pop_W
# main_df$gdp_per_capita <- main_df$gdp / main_df$pop_T
# main_df$pop_share_W <- main_df$pop_W / main_df$pop_T
# main_df$pop_share_A <- main_df$pop_A / main_df$pop_T
# main_df$pop_share_B <- main_df$pop_B / main_df$pop_T
# main_df$pop_share_F <- main_df$pop_F / main_df$pop_T
# main_df$pop_share_F_W <- main_df$pop_F_W / main_df$pop_W
# main_df$pop_share_F_B <- main_df$pop_F_B / main_df$pop_B
# main_df$pop_share_F_A <- main_df$pop_F_A / main_df$pop_A
# main_df$density <- main_df$area / main_df$pop_T
# main_df$poverty_rate_A <- main_df$poverty_A / main_df$pop_A
# main_df$poverty_rate_W <- main_df$poverty_W / main_df$pop_W
# main_df$poverty_rate_B <- main_df$poverty_B / main_df$pop_B
# main_df$poverty_rate_T <- main_df$poverty_T / main_df$pop_T

# placebo test
placebo_df <- df

# write csv
save(main_df, 
     placebo_df,
     file = "D:/github/wagegap22/data/main_cleaned_data.RData")