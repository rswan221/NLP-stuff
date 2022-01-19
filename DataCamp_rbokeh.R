### Doing this because there's a broken link in DataCamp that doesn't allow me 
# to see the graphs that are created

library(gapminder)
library(tidyverse)
library(rbokeh)

# Filter gapminder data by year 1982
dat_1982 <- gapminder %>%
  filter(year == 1982)

# Plot life expectancy Vs GDP per Capita using data_1982
figure(legend_location = "bottom_right",
       title = "Life Expectancy Vs. GDP per Capita in 1982") %>%
  ly_points(x = gdpPercap, y = lifeExp, data = dat_1982,
            color = continent, hover = c(continent, country, pop))

# Filter the dataset for the continent Africa and year 1967
data_africa <- gapminder %>%
  filter(continent == "Africa", year == 1967)


# Plot life expectancy Vs GDP per Capita using data_africa
figure(legend_location = "bottom right",
       title = "Life Expectancy Vs. GDP per Capita in Africa - 1967") %>%
  ly_points(x = gdpPercap, y = lifeExp, data = data_africa,
            hover = c(country, pop))

# Add a new column with gdp in millions
gapminder <- gapminder %>%
  mutate(gdp_millions = gdpPercap * pop/10^6)

# View the first 6 entries in gapminder after adding gdp_millions
head(gapminder)

# Extract the entries for Rwanda
data_rwanda <- gapminder %>%
  filter(country == "Rwanda")

# Explore data_rwanda
data_rwanda

# Plot GDP over time
figure(data = data_rwanda) %>%
  ly_lines(x = year, y = gdp_millions, width = 2)

str(economics)

figure() %>%
  ly_lines(x = economics$date, y = economics$pce)

figure(ylab = "unemployment %") %>%
  ly_lines(data = economics, x = date, y = 100*unemploy/pop)

dat_1992 <- gapminder %>%
  filter(year == "1992")

plot_1992 <- figure(legend_location = "bottom_right") %>%
  ly_points(dat_1992, y = lifeExp, x = gdpPercap, color = continent)

plot_1992

data_countries <- gapminder %>%
  filter(country == c("Kenya","Ghana","Zambia"))

#data_countries -- Just to check that it works, it does

figure(data = data_countries) %>%
  ly_lines(x = year, y = gdpPercap, color = country) %>%
  ly_points(x = year, y = gdpPercap, color = country)

data_countries2 <- gapminder %>%
  filter(country %in% c("China", "India"))

# Create a line plot with lifeExp vs. year 
fig_countries <- figure(legend_location = "bottom_right") %>% 
  ly_lines(data_countries, x = year, y = lifeExp, color = country) %>%
  ly_points(data_countries, x = year, y = lifeExp, color = country)

# View fig_countries
fig_countries



