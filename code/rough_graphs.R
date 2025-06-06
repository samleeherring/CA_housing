## Exploratory visualizations to double check data format and view trends

source('code/data_cleaning.R')

library(ggplot2)
library(ggtext)
library(tidyverse)
library(stringr)

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
