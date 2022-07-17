rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)
library(openxlsx)
library(censusapi)
library(bea.R)
library(blsR)

# https://www.socialexplorer.com/data/ACS2015/metadata/?ds=ACS15
# https://apps.bea.gov/iTable/iTable.cfm?reqid=70

Sys.setenv(CENSUS_KEY="4e3f1cfec3b1cf56e92475e26ed86fdccea1fa62")
beaKey <- "780D17CC-7441-4E52-93BF-2B3D89DBDF74"
blsKey <- "937fa907e9364beba6dad4a5994f42eb"

period <- 2010:2017

median_household_income <- paste0("B19013", LETTERS[c(1, 2, 4)], "_001E")
per_capita_income <- paste0("B19301", LETTERS[c(1, 2, 4)], "_001E")
bachelor <- paste0("C15010", LETTERS[c(1, 2, 4)], "_001E")
pop <- c("DP05_0037E", "DP05_0038E", "DP05_0044E")

econ_table_name <- "acs/acs5"
pop_table_name <- "acs/acs5/profile"

cagdp <- "CAGDP11"
line_code_gdp <- 1
urls <- paste0("https://www.bls.gov/lau/laucnty", period - 2000, ".xlsx")

median_household_income_county_race <- thesis2022::get_acs5_data(econ_table_name, period, median_household_income)
per_capita_income_county_race <- thesis2022::get_acs5_data(econ_table_name, period, per_capita_income)
bachelor_county_race <- thesis2022::get_acs5_data(econ_table_name, period, per_capita_income)
pop_county_race <- thesis2022::get_acs5_data(pop_table_name, period, pop)
gdp_county <- thesis2022::get_bea_data(beaKey, cagdp, line_code_gdp, period)
unemp_county <- get_unemp_from_bls(urls)

# save data
save(bachelor_county_race,
     gdp_county,
     median_household_income_county_race,
     pop_county_race,
     per_capita_income_county_race,
     unemp_county,
     file = "D:/github/thesis_2022/data/raw_data.RData")
