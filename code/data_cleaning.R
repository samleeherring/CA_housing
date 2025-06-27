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
  select(3,4) %>% slice(3:60) %>% ## don't want total CA avg income
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

## Cleaning data for the housing prices tables
## First, I'll do the 2025 dataset in 2 sections: region & county

by_region_2025 <- prices_2025 %>%
  select(1:3,5,6) %>%
  rename(region = `State/Region/County`,
         apr25 = `Apr-25`,
         mar25 = `Mar-25`,
         apr24 = `Apr-24`,
         type = 5) %>%
  mutate(apr25 = as.numeric(gsub("[$,]", "", apr25)),
         mar25 = as.numeric(gsub("[$,]", "", mar25)),
         apr24 = as.numeric(gsub("[$,]", "", apr24))) %>%
  slice(1:9) %>%
  mutate(price = (apr25+mar25+apr24)/3) %>%
  select(1,6)

by_county_2025 <- prices_2025 %>%
  select(1:3,5,6) %>%
  rename(region = `State/Region/County`,
         apr25 = `Apr-25`,
         mar25 = `Mar-25`,
         apr24 = `Apr-24`,
         type = 5) %>%
  mutate(apr25 = as.numeric(gsub("[$,]", "", apr25)),
         mar25 = as.numeric(gsub("[$,]", "", mar25)),
         apr24 = as.numeric(gsub("[$,]", "", apr24))) %>%
  filter(type == 'county') %>%
  mutate(price = (apr25+mar25+apr24)/3) %>%
  select(1,6)

## Now for the historical table, I could've cleaned this one better in Sheets,
## mais c'est la vie 

by_county_hist <- prices_hist %>%
  pivot_longer(cols= 2:64, names_to = 'counties', values_to = 'prices') %>%
  rename(mo_yr = `Mon-Yr`) %>%
  mutate(prices = as.numeric(gsub("[$,]", "", prices)))
## this is a loooong tibble (3,024 x 3), but it might look good on a graph??
 
## Need to figure out a disparity 
##hmm <-
by_county_hist_2 %>%
  mutate(counties = tolower(counties)) %>%
  filter(counties != 'ca') %>%
  distinct(counties) %>%
  anti_join(., nih_inc_2, by=c('counties'='county'))
# ok so there's no hypen in the NIH tibble for contra costa

nih_inc_2 %>%
  anti_join(., hmm, by=c('county'='counties'))
# and apparently the housing one is just missing 5 other counties??
# alpine, colusa, inyo, modoc, and sierra
# ok figured it out, those are counties containing state/fed lands/parks, so not may homes
# college brain right here