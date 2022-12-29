# rm(list = ls())
library(tidyverse)
library(dplyr)
library(tidyr)
library(gapminder)


# summarize
# plot ()
# fit
# predict()

glimpse(gapminder)


plot(gapminder)
ireland <- gapminder |>
    filter(country == "Ireland") 

ireland |>
    ggplot(aes(x=year, y=lifeExp)) +
    # geom_point() +
    geom_line() +
    geom_line(aes(y=sqrt(gdpPercap), color = "red")) +
    theme_dark()


length(unique(gapminder$year))
glimpse(ireland)
