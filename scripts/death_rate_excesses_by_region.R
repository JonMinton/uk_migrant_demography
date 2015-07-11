# Script to look at changing population structure and death rates by region

rm(list=ls())

require(readr)

require(tidyr)
require(plyr)
require(dplyr)

require(ggplot2)
require(lattice)



# Data 

dta <- read_csv(file="data/tidied/englandwales_population.csv")

dta %>% mutate(death_rate = (deaths + 0.5) /(population + 0.5)) %>% 
  filter(year %in% c(2002, 2007, 2012)) %>% 
  ggplot(data = .) +
  geom_line(aes(x=age, y=log(death_rate), group=sex, colour=sex)) + 
  facet_grid(year ~ ons_region_name)

dta %>% mutate(death_rate = deaths / population) %>% 
  contourplot(log(death_rate, base = 10) ~ age + year | sex + ons_region_name, 
              data = .,
              at = seq(from = -4, to =-1, by = 0.2),
              region = T,
              aspect = "iso"
  )


# Compared to lowest rate

dta %>% 
  filter(age >= 35) %>% 
  mutate(
  death_rate = (deaths + 0.5) / (population + 0.5),
  lg_death_rate = log(death_rate, base = 10)
  ) %>% 
  select(ons_region_name, sex, age, year, lg_death_rate) %>% 
  group_by(sex, age, year) %>% 
  mutate(
    min_lg_death_rate = min(lg_death_rate),
    excess_lg_death_rate = lg_death_rate - min(lg_death_rate)
  ) %>% ungroup %>% 
  arrange(ons_region_name, sex, year, age) %>% 
  ggplot(mapping = aes(x=age, y= excess_lg_death_rate, group = sex, colour = sex)) +
  theme_minimal() +
  facet_grid(ons_region_name ~ year) +
  stat_smooth() + 
  geom_hline(mapping=aes(x=0)) +
  coord_cartesian(xlim=c(35, 85))

# Suggestions 
# Group together some years  - 
# 2010:2013
# 2006:2009
# 2002:2005

dta %>% 
  filter(age >= 35) %>% 
  mutate(
    death_rate = (deaths + 0.5) / (population + 0.5),
    lg_death_rate = log(death_rate, base = 10)
  ) %>% 
  select(ons_region_name, sex, age, year, lg_death_rate) %>% 
  mutate(year_group = cut(
    year,
    breaks = c(2002, 2005, 2009, 2013),
    labels = c("2002-2005", "2006-2009", "2010-2013"),
    include.lowest=T
    )
  ) %>% 
  group_by(sex, age, year_group) %>% 
  mutate(
    min_lg_death_rate = min(lg_death_rate),
    excess_lg_death_rate = lg_death_rate - min(lg_death_rate)
  ) %>% ungroup %>% 
  arrange(ons_region_name, sex, year_group, age) %>% 
  ggplot(mapping = aes(x=age, y= excess_lg_death_rate, group = sex, colour = sex)) +
  theme_minimal() +
  stat_smooth() + 
  geom_hline(mapping=aes(x=0)) +
  facet_grid(year_group ~ ons_region_name)+
  coord_cartesian(xlim=c(35, 85)) 



# Next suggestion - see if we can do this for Scotland too.
