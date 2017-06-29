library(dplyr)
library(ggplot2)
library(lookupr)
library(readr)
library(zoo)

# Sources
# - Smith County sales tax data: Texas Comptroller (https://mycpa.cpa.state.tx.us/allocation/HistSales)
# - WTI crude oil prices: Fed FRED series MCOILWTICO
# - Oil and Gas employment: BLS QCEW series ENU48423105211
# - Oil and Gas total wages: BLS QCEW series ENU48423305211
# - Oil and Gas average annual pay: BLS QCEW series ENU48423505211
# - Smith County employment: BLS QCEW series ENU4842310510
# - Smith County total wages: BLS QCEW series ENU4842330510
# - Smith County average annual pay: BLS QCEW series ENU4842350510 

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
  ) %>%
  left_join(us_cpi, by = c("year" = "year")) %>%
  mutate(
    gross_sales = gross_sales * (us_base_cpi / cpi),
    subject_to_tax = subject_to_tax * (us_base_cpi / cpi)
  ) %>%
  select(-cpi)

merged <- sales_tax %>%
  left_join(oil, by = c("year" = "year"))

model <- lm(subject_to_tax ~ oil_price, data = merged)

ggplot(merged, aes(x = subject_to_tax, y = oil_price, label = year)) +
  geom_point() +
  geom_text(nudge_y = 3)
