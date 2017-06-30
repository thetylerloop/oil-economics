library(dplyr)
library(ggplot2)
library(lubridate)
library(lookupr)
library(readr)
library(readxl)
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
# - Smith County unemployment rate: BLS LAU series LAUCN484230000000003
# - Smith County employed population: BLS LAU series LAUCN484230000000005

BASE_CPI_YEAR = 2016

us_cpi <- from_lookup("year", "cpi") %>%
  mutate(year = as.numeric(year))
us_base_cpi <- us_cpi[us_cpi$year == BASE_CPI_YEAR, ]$cpi

# Load oil price data
oil <- read_csv("data/fred_wti_crude.csv") %>%
  mutate(
    year = year(as.Date(DATE))
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
  # Adjust to 2016 dollars
  mutate(
    oil_price = oil_price * (us_base_cpi / cpi)
  ) %>%
  select(-cpi)
  
# Load sales tax data
sales_tax <- read_csv("data/sales_tax.csv") %>%
  mutate(
    year = year(as.Date(date, "%m/%d/%y"))
  ) %>%
  # Convert to annual series to remove seasonal effects
  group_by(year) %>%
  summarise(
    gross_sales = sum(gross_sales),
    subject_to_tax = sum(subject_to_tax),
    establishments = mean(establishments)
  ) %>%
  left_join(us_cpi, by = c("year" = "year")) %>%
  # Adjust to 2016 dollars
  mutate(
    gross_sales = gross_sales * (us_base_cpi / cpi),
    subject_to_tax = subject_to_tax * (us_base_cpi / cpi)
  ) %>%
  select(-cpi)

# Load QCEW data series
all_employment <- read_excel("data/all_employment.xlsx", skip = 12) %>%
  select(year = Year, all_employment = Value)

all_total_wages <- read_excel("data/all_total_wages.xlsx", skip = 12) %>%
  select(year = Year, all_total_wages = Value)

all_annual_pay <- read_excel("data/all_annual_pay.xlsx", skip = 12) %>%
  select(year = Year, all_annual_pay = Value)

oil_employment <- read_excel("data/naics_211_employment.xlsx", skip = 12) %>%
  select(year = Year, oil_employment = Value)

oil_total_wages <- read_excel("data/naics_211_total_wages.xlsx", skip = 12) %>%
  select(year = Year, oil_total_wages = Value)

oil_annual_pay <- read_excel("data/naics_211_average_annual_pay.xlsx", skip = 12) %>%
  select(year = Year, oil_annual_pay = Value)

qcew <- all_employment %>%
  full_join(all_total_wages) %>%
  full_join(all_annual_pay) %>%
  full_join(oil_employment) %>%
  full_join(oil_total_wages) %>%
  full_join(oil_annual_pay) %>%
  mutate(
    oil_share_employment = oil_employment / all_employment * 100,
    oil_share_total_wages = oil_total_wages / all_total_wages * 100
  )

merged <- sales_tax %>%
  left_join(oil, by = "year") %>%
  left_join(qcew, by = "year")

model <- lm(subject_to_tax ~ all_total_wages, data = merged)

ggplot(merged, aes(x = subject_to_tax, y = oil_price, label = year)) +
  geom_point() +
  geom_text(nudge_y = 3)
