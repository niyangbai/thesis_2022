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

plot_usmap(data = minwage_state_year_dif, values = "dif", color = "black", labels = TRUE) +
  scale_fill_continuous(
    low = "white", high = "red", name = "Minimum Wage (2010)", label = scales::comma
  ) + theme(legend.position = "right")
