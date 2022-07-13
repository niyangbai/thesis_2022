rm(list = ls())
setwd("D:/github/thesis_2022")

# income by states and race
library(censusapi)
Sys.setenv(CENSUS_KEY="4e3f1cfec3b1cf56e92475e26ed86fdccea1fa62")

period <- 2010:2017
l <- paste0("B19013", LETTERS[1:9])
income_county_race <- data.frame()
for (year in period) {
  year_tab <- data.frame()
  for (i in l) {
    g <- paste("group(", i, ")", sep="")
    acs_income_group <- getCensus(
      name = "acs/acs5",
      vintage = year,
      vars = c("NAME", g),
      region = "county:*")
    e <- paste(i, "_001E", sep="")
    acs_income_group <- acs_income_group[c("NAME", e)]
    if (nrow(year_tab) == 0) {
      year_tab <- acs_income_group
    } else {
      year_tab <- merge(year_tab, acs_income_group, by="NAME")
    }
    year_tab$year <- year
  }
  income_county_race <- rbind(income_county_race, year_tab)
}

# write csv
write.csv(income_county_race, "D:/github/thesis_2022/data/income_county_race.csv", row.names = FALSE)