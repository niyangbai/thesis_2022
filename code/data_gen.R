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

# label
library(Hmisc)
names(income_county_race)[match(paste0("B19013", LETTERS[1:9], "_001E"), names(income_county_race))] <- paste0("B19013", LETTERS[1:9])
label(income_county_race$B19013A) <- "White Alone"
label(income_county_race$B19013B) <- "Black or African American Alone"
label(income_county_race$B19013C) <- "American Indian and Alaska Native Alone"
label(income_county_race$B19013D) <- "Asian Alone"
label(income_county_race$B19013E) <- "Native Hawaiian and Other Pacific Islander Alone"
label(income_county_race$B19013F) <- "Some Other Race Alone"
label(income_county_race$B19013G) <- "Two or More Races"
label(income_county_race$B19013H) <- "White Alone, Not Hispanic or Latino"
label(income_county_race$B19013I) <- "Hispanic or Latino"

# separate
library(tidyr)
income_county_race <- separate(data = income_county_race, col = NAME, into = c("county", "state"), sep = ", ", remove = FALSE)

# min wage
minwage_state_year <- read.csv("data/minimum_wage.csv")
minwage_state_year <- subset(minwage_state_year, year %in% seq(2010, 2017))
minwage_state_year <- subset(minwage_state_year, select = c("year", "state", "State.Minimum.Wage"))
minwage_state_year_2010 <- subset(minwage_state_year, year == 2010)
names(minwage_state_year_2010)[names(minwage_state_year_2010) == "State.Minimum.Wage"] <- "2010.State.Minimum.Wage"
minwage_state_year_2017 <- subset(minwage_state_year, year == 2017)
names(minwage_state_year_2017)[names(minwage_state_year_2017) == "State.Minimum.Wage"] <- "2017.State.Minimum.Wage"
minwage_state_year_dif <- merge(minwage_state_year_2010, minwage_state_year_2017, by = "state")
minwage_state_year_dif$dif <- minwage_state_year_dif$`2017.State.Minimum.Wage` - minwage_state_year_dif$`2010.State.Minimum.Wage`

# merge
minwage_state_year$yearstate <- paste(minwage_state_year$year, "-", minwage_state_year$state, sep = "")
income_county_race$yearstate <- paste(income_county_race$year, "-", income_county_race$state, sep = "")
df <- merge(income_county_race, minwage_state_year, by = "yearstate")
df$year <- df$year.y
df$state <- df$state.y
df <- df[,!(names(df) %in% c("yearstate", "year.x", "year.y", "state.x", "state.y"))]

# subset
df_clean <- df
df_clean[df_clean == -666666666] <- NA
df_clean <- na.omit(df_clean)


# # real
# df_real <- df
# df_real$coef <- df_real$Federal.Minimum.Wage.2020.Dollars/df_real$Federal.Minimum.Wage
# for (i in paste0("B19013", LETTERS[1:9])) {
#   df_real[i] <- df_real[i] * df_real$coef
# }
# names(df_real)[match(paste0("B19013", LETTERS[1:9]), names(df_real))] <- paste0("B19013", LETTERS[1:9], ".2020.Dollars")
# df_real = df_real[,!(names(df_real) %in% c("State.Minimum.Wage", "Federal.Minimum.Wage", "coef"))]

# # min wage difference
# df_10 <- subset(df, year == 2010)
# df_d <- subset(df, year == 2017)[c("state", "State.Minimum.Wage")]
# df_d$dif <- df_d$State.Minimum.Wage - df_10$State.Minimum.Wage
# df_d <- df_d[,!(names(df_d) == "State.Minimum.Wage")]

# min wage plot
library(usmap)
library(ggplot2)
plot_usmap(data = df[which(df$year=='2017'),], values = "State.Minimum.Wage", color = "black") +
  scale_fill_continuous(
    low = "white", high = "blue", name = "Minimum Wage (2017)", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = df[which(df$year=='2010'),], values = "State.Minimum.Wage", color = "black") +
  scale_fill_continuous(
    low = "white", high = "red", name = "Minimum Wage (2010)", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = df_d, values = "dif", color = "black", labels = TRUE) +
  scale_fill_continuous(
    low = "white", high = "red", name = "Minimum Wage (2010)", label = scales::comma
  ) + theme(legend.position = "right")
