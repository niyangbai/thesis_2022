rm(list = ls())
setwd("D:/github/wagegap22")
library(wagegap22package)
library(openxlsx)
library(censusapi)
library(bea.R)
library(blsR)

# https://www.socialexplorer.com/data/ACS2015/metadata/?ds=ACS15
# https://apps.bea.gov/iTable/iTable.cfm?reqid=70
# https://data.census.gov/cedsci/table

Sys.setenv(CENSUS_KEY="4e3f1cfec3b1cf56e92475e26ed86fdccea1fa62")
beaKey <- "780D17CC-7441-4E52-93BF-2B3D89DBDF74"
blsKey <- "937fa907e9364beba6dad4a5994f42eb"

period <- 2010:2019

median_household_income <- paste0("B19013", LETTERS[c(1, 2, 4)], "_001E")
per_capita_income <- paste0("B19301", LETTERS[c(1, 2, 4)], "_001E")
median_earning <- paste0("B20017", LETTERS[c(1, 2, 4)], "_001E")
highschool <- c("C15002A_003E", "C15002A_008E", "C15002B_003E", "C15002B_008E", "C15002D_003E", "C15002D_008E")
race_pop <- c("B01001_001E", "B01001_026E", "B01001A_001E", "B01001B_001E", "B01001D_001E", "B01001A_026E", "B01001B_026E", "B01001D_026E")
race_emp <- c("S2301_C03_012E", "S2301_C03_013E", "S2301_C03_015E")
age_pop <- c("B01002A_001E", "B01002B_001E", "B01002D_001E")
poverty <- c("B17001_002E", "B17001A_002E", "B17001B_002E", "B17001D_002E")

acs5 <- "acs/acs5"
subject <- "acs/acs5/subject"

real_gdp <- "CAGDP1"
line_code_gdp <- 1
urls <- paste0("https://www.bls.gov/lau/laucnty", period - 2000, ".xlsx")

real_gdp_county <- get_bea_data(beaKey, real_gdp, line_code_gdp, period)
median_household_income_county_race <- get_acs5_data(acs5, period, median_household_income)
per_capita_income_county_race <- get_acs5_data(acs5, period, per_capita_income)
highschool_county_race <- get_acs5_data(acs5, period, highschool)
race_pop_county <- get_acs5_data(acs5, period, race_pop)
race_emp_county <- get_acs5_data(subject, period, race_emp)
age_pop_county <- get_acs5_data(acs5, period, age_pop)
median_earning_county <- get_acs5_data(acs5, period, median_earning)
poverty_county <- get_acs5_data(acs5, period, poverty)

# save data
save(highschool_county_race,
     real_gdp_county,
     median_household_income_county_race,
     race_pop_county,
     median_earning_county,
     per_capita_income_county_race,
     age_pop_county,
     poverty_county,
     race_emp_county,
     file = "D:/github/wagegap22/data/raw_data.RData")

# data <- getCensus(
#         name = "acs/acs5",
#         vintage = 2016,
#         vars = "group(C15002A)",
#         region = "county:*")