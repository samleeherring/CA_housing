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


