rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)
library(censusapi)
library(bea.R)

# acs5
# https://www.socialexplorer.com/data/ACS2015/metadata/?ds=ACS15

Sys.setenv(CENSUS_KEY="4e3f1cfec3b1cf56e92475e26ed86fdccea1fa62")
beaKey <-"780D17CC-7441-4E52-93BF-2B3D89DBDF74"

period <- 2010:2017

median_household_income <- paste0("B19013", LETTERS[1:9])
per_capita_income <- paste0("B19301", LETTERS[1:9])
bachelor <- paste0("C15010", LETTERS[1:9])
subtable <- "001E"
cagdp <- "CAGDP2"
line_code <- 1

median_household_income_county_race <- thesis2022::get_acs5_data(period, median_household_income, subtable)
per_capita_income_county_race <- thesis2022::get_acs5_data(period, per_capita_income, subtable)
bachelor_county_race <- thesis2022::get_acs5_data(period, per_capita_income, subtable)
gdp_county <- thesis2022::get_bea_data(beaKey, cagdp, line_code, period)

# save data
save(bachelor_county_race, gdp_county, median_household_income_county_race, per_capita_income_county_race, file = "D:/github/thesis_2022/data/raw_data.RData")
