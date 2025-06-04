## This environment will be for cleaning data for this project

library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)

NIH_income <- read_csv('data/CA_income_NIH.csv')
EDD_income <- read_csv('data/CA_income_EDD.csv')
prices_2025 <- read_csv('data/house_prices_2025.csv')
prices_hist <- read_csv('data/house_prices_historical.csv')


## Cleaning data for the income tables

glimpse(EDD_income)

nih_inc <- NIH_income %>% ## avg from 2019-2023
  select(1,3) %>%
  mutate(county = str_sub(County, 1, -8), ## don't want 'county' in name
         income = `Value (Dollars)`) %>%
  select(3,4) %>% slice(3:58) %>% ## don't want total CA avg income
  arrange(county)

edd_inc <- EDD_income %>% ## just 2022
  arrange(`Income Type`) %>%
  slice(1:59) %>%
  select(cols=1,2,5,6) %>%
  mutate(income = as.numeric(gsub("[$,]", "", Income)),
         population = Population,
         year = Year,
         county = str_sub(cols,1, -8)) %>%
  select(8,7,5,6) %>%
  arrange(population) %>% slice(1:58) %>% 
  arrange(county)
