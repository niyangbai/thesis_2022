rm(list = ls())
setwd("D:/github/thesis_2022")

# income by states and race
library(censusapi)
Sys.setenv(CENSUS_KEY="4e3f1cfec3b1cf56e92475e26ed86fdccea1fa62")

period <- 2010:2017
l <- paste0("B19013", LETTERS[1:9])
income_state_race <- data.frame()
for (year in period) {
  year_tab <- data.frame()
  for (i in l) {
    g <- paste("group(", i, ")", sep="")
    acs_income_group <- getCensus(
      name = "acs/acs5", 
      vintage = year,
      vars = c("NAME", g), 
      region = "state:*")
    e <- paste(i, "_001E", sep="")
    acs_income_group <- acs_income_group[c("NAME", e)]
    if (nrow(year_tab) == 0) {
      year_tab <- acs_income_group
    } else {
      year_tab <- merge(year_tab, acs_income_group, by="NAME")
    }
    year_tab$year <- year
  }
  income_state_race <- rbind(income_state_race, year_tab)
}

# min wage
minwage_state_year <- read.csv("data/minimum_wage.csv")
minwage_state_year <- subset(minwage_state_year, Year %in% seq(2010, 2017))

# merge
minwage_state_year$yearstate <- paste(minwage_state_year$year, "-", minwage_state_year$state, sep = "")
income_state_race$yearstate <- paste(income_state_race$year, "-", income_state_race$NAME, sep = "")
df <- merge(income_state_race, minwage_state_year, by = "yearstate")
df$year <- df$year.y
df <- df[,!(names(df) %in% c("yearstate","NAME", "year.x", "year.y"))]

# min wage difference
df_10 <- subset(df, year == 2010)
df_d <- subset(df, year == 2017)[c("state", "State.Minimum.Wage")]
df_d$dif <- df_d$State.Minimum.Wage - df_10$State.Minimum.Wage
df_d <- df_d[,!(names(df_d) == "State.Minimum.Wage")]

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

