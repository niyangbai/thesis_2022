rm(list = ls())
setwd("D:/github/thesis_2022")
library(thesis2022)

# income by states and race
# https://www.socialexplorer.com/data/ACS2015/metadata/?ds=ACS15

library(censusapi)
Sys.setenv(CENSUS_KEY="4e3f1cfec3b1cf56e92475e26ed86fdccea1fa62")

period <- 2010:2017
median_household_income <- paste0("B19013", LETTERS[1:9])
per_capita_income <- paste0("B19301", LETTERS[1:9])
subtable <- "001E"

median_household_income_county_race <- thesis2022::get_acs5_data(period, median_household_income, subtable)
per_capita_income_county_race <- thesis2022::get_acs5_data(period, per_capita_income, subtable)

# write csv
write.csv(income_county_race, "D:/github/thesis_2022/data/income_county_race.csv", row.names = FALSE)
