rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)
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

period <- 2010:2017

median_household_income <- paste0("B19013", LETTERS[c(1, 2, 4)], "_001E")
per_capita_income <- paste0("B19301", LETTERS[c(1, 2, 4)], "_001E")
median_earning <- paste0("B20017", LETTERS[c(1, 2, 4)], "_001E")
highschool <- paste0("C15002", LETTERS[c(1, 2, 4)], "_004E")
race_pop_share <- c("DP05_0037PE", "DP05_0038PE", "DP05_0044PE")
total_pop <- c("DP05_0001E")
race_unemp <- c("S2301_C04_012E", "S2301_C04_013E", "S2301_C04_015E")
age_pop <- c("B01002A_001E", "B01002B_001E", "B01002D_001E")

econ_table_name <- "acs/acs5"
pop_table_name <- "acs/acs5/profile"
emp_table_name <- "acs/acs5/subject"

real_gdp <- "CAGDP9"
line_code_gdp <- 1
urls <- paste0("https://www.bls.gov/lau/laucnty", period - 2000, ".xlsx")

median_household_income_county_race <- get_acs5_data(econ_table_name, period, median_household_income)
per_capita_income_county_race <- get_acs5_data(econ_table_name, period, per_capita_income)
highschool_county_race <- get_acs5_data(econ_table_name, period, highschool)
race_pop_share_county <- get_acs5_data(pop_table_name, period, race_pop_share)
race_unemp_county <- get_acs5_data(emp_table_name, period, race_unemp)
total_pop_county <- get_acs5_data(pop_table_name, period, total_pop)
real_gdp_county <- get_bea_data(beaKey, real_gdp, line_code_gdp, period)
total_unemp_county <- get_unemp_from_bls(urls)
age_pop_county <- get_acs5_data(econ_table_name, period, age_pop)
median_earning_county <- get_acs5_data(econ_table_name, period, median_earning)

# save data
save(highschool_county_race,
     real_gdp_county,
     median_household_income_county_race,
     race_pop_share_county,
     total_pop_county,
     median_earning_county,
     per_capita_income_county_race,
     total_unemp_county,
     age_pop_county,
     race_unemp_county,
     file = "D:/github/thesis_2022/data/raw_data.RData")


# data <- getCensus(
#         name = "acs/acs5",
#         vintage = 2016,
#         vars = "B20017A_001E",
#         region = "county:*")
