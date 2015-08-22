rm(list=ls())

# 22 / 5/ 2015

# Introduction ------------------------------------------------------------

# This project will use data from the ONS Components of Change (CoC) dataset,
# used to produce annual small area population estimates for England, to produce 
# visualisations of the age-structure of in-migrants and out-migrants in England 
#  as a whole, and regions within England

# The code to rearrange the data from the format in the CoC download is within the 
# la_mort repository


# prereqs -----------------------------------------------------------------


require(readr)
# for scotland population data



require(plyr)
require(stringr)
require(tidyr)
require(dplyr)
require(car)

require(RColorBrewer)
require(rgl)
require(ggplot2)
require(lattice)
require(r2stl)
require(scales)


# Data --------------------------------------------------------------------

dta <- read_csv(file="data/tidied/england_la_count.csv") 

polls <- read_csv(file = "data/voting_2015/ispos_mori_polls.csv")
polls <- polls %>% select(age_group = age, min_age, max_age, con, lab, ld, combined_other, turnout)

population <- dta %>% 
  filter(year == max(year)) %>% 
  select(year, population, age) %>% 
  group_by(year, age) %>% 
  summarise(population = sum(population)) %>% 
  mutate(
    age_group = 
    recode(age,
  "
  18:24 = '`18-24';
  25:34 = '`25-34';
  35:44 = '`35-44';
  45:54 = '`45-54';
  55:64 = '`55-64';
  65:90 = '`65+'; 
  else = NA
  "
  )
)

pop_votes <- population %>% 
  left_join(polls) 

pop_votes$turnout[pop_votes$age < 18] <- 0

pop_votes <- pop_votes %>% 
  mutate(
    pop_con = population * turnout * con/100,
    pop_lab = population * turnout * lab/100,
    pop_ld = population * turnout * ld/100,
    pop_other = population * turnout *combined_other/100,
    pop_none = population * ( 1 - turnout)
  )




pop_votes  %>%
  filter(age < 90) %>% 
  ggplot(.) + 
  geom_ribbon(
    aes(x=age, ymin=0, ymax=population),
    fill="lightgrey"
  ) + 
  geom_ribbon(
    aes(x=age, ymax=pop_lab , ymin=0), 
    fill="red"
  ) + 
  geom_ribbon(
    aes(x=age, ymax=pop_con + pop_lab , ymin=pop_lab ), 
    fill="blue"
  ) + 
  geom_ribbon(
    aes(x=age, ymax=pop_con + pop_lab + pop_other, ymin=pop_con + pop_lab ), 
    fill="purple"
  ) + 
  theme_minimal() +
  scale_y_continuous(labels=comma) + 
  geom_vline(xintercept = 18) +
  geom_vline(xintercept = 16, linetype= "dashed") +
  labs(title = expression(bold("UK Voting behaviours in 2015 election by age")),
       x= "Age in years", y= "Population size"
       )

  
ggsave(filename = "figures/voting_behaviours_2015.png", dpi = 300, 
       height = 15, width = 15, units = "cm"
       )
  
