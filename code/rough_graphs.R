## Exploratory visualizations to double check data format and view trends

source('code/data_cleaning.R')

library(ggplot2)
library(ggtext)
library(tidyverse)
library(stringr)
library(lubridate)


region_labels <- c('LA Metro' = 'Los Angeles Metropolitan Area',
                   'Central Coast' = 'Central Coast',
                   'MidCal' = 'Central Valley',
                   'Far North' = 'Far North',
                   'IE' = "Inland Empire",
                   'Yay Area' = 'S.F. Bay Area',
                   'SoCal' ='Southern California')

region_label <- c('LA Metro', 'Central Coast', 'MidCal', 'Far North', 'I.E.',
                   'Yay Area', 'SoCal')

by_region_2025 %>%
  slice(3:9) %>%
  ggplot(aes(x=region, y=price, fill=region)) +
  geom_col(show.legend = F) +
  
  scale_x_discrete(breaks = waiver(), labels = region_label) +
  scale_y_continuous(breaks = seq(300000, 1500000, 300000),
                     labels=c('300k', '600k', '900k','1.2mil', '1.5mil'),
                     expand = c(-0.08,100000), limits = c(0,1600000)) +
  scale_fill_viridis_d(option = 'turbo')+
  
  labs(
    title = 'Median price of homes in California by region',
    subtitle = 'Averages include both Single-family Homes & Condominium sales',
    x = NULL,
    y = 'Price ($USD)',
    tag = 'Data from:  CA Assc. of Realtors'
  ) +
  
  theme(
    plot.title = element_textbox_simple(margin = margin(b=5), size = 15),
    plot.subtitle = element_text(color = '#2b2b2b', size = 10),
    panel.background = element_rect(fill = '#f6f6f6'),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in'),
    plot.tag = element_text(size = 9, color = 'darkgrey'),
    plot.tag.position = 'bottomright',
    plot.tag.location = 'plot',
    axis.line = element_line(),
    axis.text.x = element_text(angle = -45, vjust = 0.5, hjust = 0),
    )

ggsave('plots/price_by_region_rough.png', width = 6, height = 5, units = 'in')


## Creating a simple line chart to see trends in housing prices over last 5y

## Need to rearrange the data sets a bit
by_county_hist_2 <- prices_hist %>%
  pivot_longer(cols= 2:56, names_to = 'counties', values_to = 'prices') %>%
  rename(mo_yr = `Mon-Yr`) %>%
  mutate(prices = as.numeric(gsub("[$,]", "", prices))) %>%
  select(1,10,11)  %>% 
  drop_na()

by_region_hist <- prices_hist %>%
  pivot_longer(cols= 56:64, names_to = 'zregions', values_to = 'zprices') %>%
  rename(mo_yr = `Mon-Yr`) %>%
  select(1,56,57)  %>% 
  mutate(prices = as.numeric(gsub("[$,]", "", zprices))) %>%
  rename(regions = zregions) %>%
  filter(regions != 'Condo') %>%
  select(1,2,4) %>%
  drop_na()

## ok graph time
#p <-
by_county_hist_2 %>%
  group_by(counties) %>% #distinct(counties) %>% print(n=63)
  filter(counties != 'CA') %>%
  #slice(1:53) %>%
  mutate(mo_yr = gsub("-", "", mo_yr)) %>%
  mutate(mo_yr = tolower(mo_yr)) %>%
  mutate(date = my(mo_yr)) %>%
  select(2:4) %>%
  
  ggplot(aes(x=date, y=prices, color=counties, group = counties)) +
  geom_point(show.legend = F) +
  geom_path(show.legend = F) +
  #coord_cartesian(clip = 'off') +
  scale_y_continuous(breaks = seq(500000, 2500000, 500000),
                     labels=c('500k', '1mil', '1.5mil','2mil', '2.5mil'),
                     expand = c(-0.08,100000), limits = c(0,2700000)) +
  scale_x_date(expand = c(0.01,0.01)) +
  scale_color_discrete() +
  
  labs(
    title = 'Median prices of homes in CA by county',
    subtitle = 'Well this looks just awful. Way too many groups for one graph.',
    x=NULL,
    y='Median price of homes ($USD)'
  ) +
  
  theme(
    plot.title = element_textbox_simple(margin = margin(b=5), size = 17),
    plot.title.position = 'plot',
    plot.subtitle = element_text(color = '#2b2b2b', size = 10),
    panel.background = element_rect(fill = '#f6f6f6'),
    plot.background = element_rect(fill = '#f6f6f6'),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in'),
    plot.tag = element_text(size = 9, color = 'darkgrey'),
    axis.line = element_line(),
    axis.text.x = NULL
  )
ggsave('plots/price_by_county_rough.png', width = 6, height = 5, units = 'in')

## Setting up the regional historical data frame the same as the county one
by_region_hist_2 <- by_region_hist %>%
  mutate(mo_yr = gsub("-", "", mo_yr)) %>%
  mutate(mo_yr = tolower(mo_yr)) %>%
  mutate(date = my(mo_yr)) %>%
  #rename(counties = regions) %>%
  select(2:4)
  
# p +
#   geom_smooth(data = by_region_hist_2, aes(x=date, y=prices, color=regions),
#               se=F, inherit.aes = F, show.legend = F) +
#   scale_color_manual(name = 'Regional Trends',
#                      values = c('palegreen3', 'mediumpurple', 'indianred1',
#                                 'dodgerblue', 'goldenrod1', 'black', 'grey'),
#                      labels = c('LA Metro', 'Central Coast', 'Central Valley',
#                                 'Far North', 'Inland Empire', 'S.F. Bay Area', 'SoCal'),
#                      guide = guide_legend(override.aes = list(shape=15, size = 4))) 

## Graphing trends by region over the last 5y
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

by_region_hist_2 %>%
  group_by(regions) %>% 
  
  ggplot(aes(x=date, y=prices, color=regions, group = regions)) +
  geom_smooth(se=T, size=1.5) +
  #coord_cartesian(clip = 'off') +
  scale_y_continuous(breaks = seq(300000, 1500000, 300000),
                     labels=c('300k', '600K', '900K','1.2mil', '1.5mil'),
                     expand = c(-0.08,100000), limits = c(200000,1600000)) +
  scale_x_date(expand = c(0,0.01)) +
  scale_color_discrete(type=okabe) +
  guides(color = guide_legend(title = 'Regional Trends')) +
  
  labs(
    title = 'Trends in median prices of homes in CA by region',
    subtitle = "That's better, can actually make out the shapes here.",
    x=NULL,
    y='Median price of homes ($USD)',
    tag = 'Data from:  CA Assc. of Realtors'
  ) +
  
  theme(
    plot.title = element_textbox_simple(margin = margin(b=5), size = 17),
    plot.title.position = 'plot',
    plot.subtitle = element_text(color = '#2b2b2b', size = 10),
    panel.background = element_rect(fill = '#f6f6f6'),
    plot.background = element_rect(fill = '#f6f6f6'),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in'),
    plot.tag = element_text(size = 9, color = 'darkgrey'),
    plot.tag.position = 'bottomright',
    plot.tag.location = 'plot',
    legend.background = element_rect(fill='lightgrey'),
    axis.line = element_line(),
    axis.text.x = NULL
  )
ggsave('plots/trends_by_region_rough.png', width = 6, height = 5, units = 'in')

## Taking a look at some of the income/population data
edd_inc %>% arrange(-income)
  ggplot(aes(y=county, x=income, fill=population)) +
  #ggplot(aes(y=county, x=population, fill=income)) +
  geom_col(show.legend = F, position = 'stack') +
  scale_y_discrete(breaks = waiver()) +
  scale_x_continuous(breaks = seq(30000, 180000, 30000),
                     labels=c('30k', '60k', '90k','120k', '150k', '180k'),
                     expand = c(0,0), limits = c(0,185000)) +
  #scale_fill_gradient(low = 'red', high = '')+

  labs(
    title = "Median income in California by county (2022)",
    x = "Income ($USD)",
    y = NULL,
    tag = 'Data: Employment Development Department'
  ) +
  
  theme(
    plot.title = element_textbox_simple(margin = margin(b=5), size = 15),
    panel.background = element_rect(fill = '#f6f6f6'),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in'),
    plot.tag = element_text(size = 9, color = 'darkgrey'),
    plot.tag.position = 'bottomright',
    plot.tag.location = 'plot',
    axis.line = element_line(),
  )

nih_inc %>% 
  ggplot(aes(y=county, x=income)) +
  #ggplot(aes(y=county, x=population, fill=income)) +
  geom_col(show.legend = F, position = 'stack') +
  scale_y_discrete(breaks = waiver()) +
  scale_x_continuous(breaks = seq(40000, 160000, 40000),
                     labels=c('40k', '80k', '120k','160k'),
                     expand = c(0,0), limits = c(0,165000)) +
  #scale_fill_gradient(low = 'red', high = '')+
  
  labs(
    title = "Median income in California by county (2019-2023)",
    x = "Income ($USD)",
    y = NULL,
    tag = "Data: Nat'l Institue of Health"
  ) +
  
  theme(
    plot.title = element_textbox_simple(margin = margin(b=5), size = 15),
    panel.background = element_rect(fill = '#f6f6f6'),
    plot.margin = margin(10,5,10,5),
    panel.spacing = unit(0.3, 'in'),
    plot.tag = element_text(size = 9, color = 'darkgrey'),
    plot.tag.position = 'bottomright',
    plot.tag.location = 'plot',
    axis.line = element_line(),
  )
## I think I'll go with this one because it shows an average throughout covid &
## I like that better than the 2022 snapshot
ggsave('plots/income_by_county_rough.png', width = 6, height = 7, units = 'in')

## For when it's mappy time

library(usmap)
library(maps)
library(mapproj)
library(glue)

map('county', 'california', fill = TRUE, col = palette())
us_map('california')

nih_inc_2 <- nih_inc %>% mutate(county=tolower(county))

## Income by county map test
map_data(map='county', region='california') %>% 
  #distint(subregions) checking for 58 counties
  inner_join(., nih_inc_2, by = c('subregion'='county')) %>%
  ggplot(aes(x=long, y=lat, group=group, fill=income)) +
  geom_polygon(color='grey') +
  scale_fill_continuous(name = 'Income Level',
                        breaks = c(75000, 100000, 125000, 150000),
                        labels=c('$75k', '$100k', '$125k', '$150k')) +
  
  labs(
    title = 'Median income by county in California',
    x = NULL,
    y = NULL
  ) +
  
  theme(
    plot.title = element_textbox_simple(margin = margin(b=5), size = 20),
    panel.background = element_rect(fill = '#f6f6f6'),
    #plot.margin = margin(10,5,10,5),
    #panel.spacing = unit(0.3, 'in'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'inside',
    legend.position.inside = c(.7, .8)
  )
## Good enough for a rough plot
ggsave('plots/income_map_rough.png', width = 5, height = 6, units = 'in')



## Home prices map of CA by county
by_county_hist_2 %>% distinct(counties) %>% print(n=56)

## Checking to make sure all data is present
by_county_hist_2 %>% mutate(counties = tolower(counties)) %>% filter(counties != 'ca') %>%
  anti_join(., nih_inc_2, by = c('counties'='county')) %>%
  print(n=50) # for some reason, Contra-Costa county is being omitted, back to cleaning

county_3 <- by_county_hist_2 %>%
  mutate(counties = tolower(counties)) %>%
  filter(counties != 'ca') %>% #no need CA avg
  mutate(counties = gsub("-", " ", counties)) #fixing contra-costa to work w/ dataset


map_data(map='county', region='california') %>% 
  #distint(subregions) checking for 58 counties
  inner_join(., county_3, by = c('subregion'='counties')) %>%
  ggplot(aes(x=long, y=lat, group=group, fill=prices)) +
  geom_polygon(color='grey') +
  scale_fill_continuous(name = 'Home Price',
                        breaks = c(500000, 1000000, 1500000, 2000000),
                        labels=c('$500k', '$1mil', '$1.5mil', '$2mil')) +
  
  labs(
    title = 'Median price of a single-family home by county in California',
    x = NULL,
    y = NULL
  ) +
  
  theme(
    plot.title = element_textbox_simple(margin = margin(b=5), size = 20),
    panel.background = element_rect(fill = '#f6f6f6'),
    #plot.margin = margin(10,5,10,5),
    #panel.spacing = unit(0.3, 'in'),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'inside',
    legend.position.inside = c(.7, .8)
  )
## Good enough for a rough plot
ggsave('plots/homes_map_rough.png', width = 5, height = 6, units = 'in')


#Ok so I need to do this by year... or perhaps make a gif!
county_3 %>% slice_max(prices)
county_3 %>% slice_min(prices)

