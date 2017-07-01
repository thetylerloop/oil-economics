library(dplyr)
library(ggplot2)
library(lubridate)
library(lookupr)
library(readr)
library(readxl)
library(stringr)
library(zoo)

# Sources
# - CPI, not seasionally adjusted: BLS series CUUR0000SA0
# - Smith County sales tax data: Texas Comptroller (https://mycpa.cpa.state.tx.us/allocation/HistSales)
# - WTI crude oil prices: Fed FRED series MCOILWTICO
# - Smith County average weekly wages (private): BLS QCEW series ENU4842340510
# - Smith County unemployment rate: BLS LAU series LAUCN484230000000003
# - Smith County labor force: BLS LAU series LAUCN484230000000006

# Load CPI
cpi <- read_excel("data/cpi.xlsx", skip = 11) %>%
  mutate(
    month = as.numeric(str_sub(Period, 2, 3)),
    date = paste(Year, month, 1, sep = "-"),
    quarter = as.yearqtr(date, format = "%Y-%m-%d")
  ) %>%
  # Convert from monthly to quarterly
  group_by(quarter) %>%
  summarise(
    cpi = mean(Value)
  )

base_cpi <- cpi[cpi$quarter == as.yearqtr("2016-01-01", format = "%Y-%m-%d"),]$cpi

# Load oil price data
oil <- read_csv("data/fred_wti_crude.csv") %>%
  mutate(
    quarter = as.yearqtr(as.Date(DATE))
  ) %>%
  group_by(quarter) %>%
  summarise(
    oil_price = mean(MCOILWTICO)
  ) %>%
  left_join(cpi) %>%
  # Adjust to 2016 dollars
  mutate(
    oil_price = oil_price * (base_cpi / cpi)
  ) %>%
  select(-cpi)
  
# Load sales tax data
sales_tax <- read_csv("data/sales_tax.csv") %>%
  mutate(
    quarter = as.yearqtr(as.Date(date, "%m/%d/%y"))
  ) %>%
  left_join(cpi) %>%
  # Adjust to 2016 dollars
  mutate(
    gross_sales = gross_sales * (base_cpi / cpi),
    subject_to_tax = subject_to_tax * (base_cpi / cpi)
  ) %>%
  select(quarter, gross_sales, subject_to_tax, establishments)

# Load QCEW data series
labor_force <- read_excel("data/all_labor_force.xlsx", skip = 11) %>%
  mutate(
    month = as.numeric(str_sub(Period, 2, 3)),
    date = paste(Year, month, 1, sep = "-"),
    quarter = as.yearqtr(date, format = "%Y-%m-%d")
  ) %>%
  # Convert from monthly to quarterly
  group_by(quarter) %>%
  summarise(
    labor_force = mean(Value)
  )

unemployment_rate <- read_excel("data/all_unemployment_rate.xlsx", skip = 11) %>%
  mutate(
    month = as.numeric(str_sub(Period, 2, 3)),
    date = paste(Year, month, 1, sep = "-"),
    quarter = as.yearqtr(date, format = "%Y-%m-%d")
  ) %>%
  # Convert from monthly to quarterly
  group_by(quarter) %>%
  summarise(
    unemployment_rate = mean(Value)
  )

weekly_wages <- read_excel("data/all_weekly_wages.xlsx", skip = 11) %>%
  mutate(
    qtr = as.numeric(str_sub(Period, 2, 3)),
    date = paste(Year, qtr, sep = "-"),
    quarter = as.yearqtr(date, format = "%Y-%q")
  ) %>%
  # Convert from monthly to quarterly
  group_by(quarter) %>%
  summarise(
    weekly_wages = mean(Value)
  ) %>%
  left_join(cpi) %>%
  # Adjust to 2016 dollars
  mutate(
    weekly_wages = weekly_wages * (base_cpi / cpi)
  ) %>%
  select(-cpi)

merged <- sales_tax %>%
  left_join(oil) %>% 
  left_join(labor_force) %>%
  left_join(unemployment_rate) %>%
  left_join(weekly_wages) %>%
  # Standardize model inputs
  mutate(
    gross_sales = gross_sales / 1000000,
    subject_to_tax = subject_to_tax / 1000000,
    labor_force = labor_force / 1000
  )

model <- lm(subject_to_tax ~ labor_force + unemployment_rate + oil_price + weekly_wages, merged)
summary(model)  

ggplot(merged, aes(x = subject_to_tax, y = unemployment_rate)) +
  geom_point()
