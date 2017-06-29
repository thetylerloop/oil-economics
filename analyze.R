library(dplyr)
library(ggplot2)
library(lookupr)
library(readr)
library(zoo)

BASE_CPI_YEAR = 2016

us_cpi <- from_lookup("year", "cpi")
us_base_cpi <- us_cpi[us_cpi$year == BASE_CPI_YEAR, ]$cpi

oil <- read_csv("data/fred_wti_crude.csv") %>%
  mutate(
    year = as.character(year(as.Date(DATE)))
  ) %>%
  select(
    year,
    usd_per_barrel = MCOILWTICO
  ) %>%
  # Convert to annual series to remove seasonal effects
  group_by(year) %>%
  summarise(
    oil_price = mean(usd_per_barrel)
  ) %>%
  left_join(us_cpi, by = c("year" = "year")) %>%
  mutate(
    oil_price = oil_price * (us_base_cpi / cpi)
  ) %>%
  select(-cpi)
  
sales_tax <- read_csv("data/sales_tax.csv") %>%
  mutate(
    year = as.character(year(as.Date(date, "%m/%d/%y")))
  ) %>%
  group_by(year) %>%
  summarise(
    gross_sales = sum(gross_sales),
    subject_to_tax = sum(subject_to_tax),
    establishments = mean(establishments)
  )

merged <- sales_tax %>%
  left_join(oil, by = c("year" = "year"))

model <- lm(subject_to_tax ~ oil_price, data = merged)

ggplot(merged, aes(x = subject_to_tax, y = oil_price, label = year)) +
  geom_point() +
  geom_text(nudge_y = 3)
