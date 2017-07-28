# Material for Nankai lecture

rm(list=ls())



# prereqs -----------------------------------------------------------------

pacman::p_load(
  tidyverse,
  RColorBrewer,
  ggplot2,
  scales
)

dta_uk <- read_csv("data/tidied/UK_population_migration_inc_Scotland.csv") 



# 

# Data Preparation --------------------------------------------------------


levels(
  dta_uk$ons_region_name
)[levels(dta_uk$ons_region_name)=="Yorkshire and The Humber"] <- "Yorkshire\n/Humber"

dta_uk$ons_region_name <- factor(dta_uk$ons_region_name, 
                                 levels =  c(
                                   "London", "South East", "South West",
                                   "East of England", "East Midlands", "West Midlands",
                                   "North East", "North West", "Yorkshire\n/Humber",
                                   "Wales", "Scotland"
                                 )
)


# Main latticeplot --------------------------------------------------------

# Scotland only 

dta_uk %>% 
  filter(ons_region_name == "Scotland") %>% 
  filter(year %in% c(2002, 2013)) %>% 
  ggplot(.) + 
  geom_ribbon(
    aes(x=age, ymin=0, ymax=population),
    fill="lightgrey"
  ) + 
  geom_ribbon(
    aes(x=age, ymax=0, ymin=-internal_out), 
    fill="lightblue"
  ) + 
  geom_ribbon(
    aes(x=age, ymax=-internal_out, ymin=-(internal_out + international_out)), 
    fill="darkblue"
  ) + 
  geom_ribbon(
    aes(x=age, ymin=0, ymax=internal_in), 
    fill="red"
  ) + 
  geom_ribbon(
    aes(x=age, ymin=internal_in, ymax=(internal_in + international_in)), 
    fill="darkred") + 
  facet_wrap( ~ year) +
  theme_minimal() +
  scale_y_continuous(labels=comma) +
  labs(title="Migration and Population Structure in Scotland", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=-20000, ymax=150000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=-20000, ymax=150000, alpha=0.2) 

ggsave("figures/nankai_lecture_material/Scotland_two_years.png", 
       dpi=150,
       width=30, height=15, 
       units="cm"
)


# Main latticeplot --------------------------------------------------------

# Scotland only 

dta_uk %>% 
  filter(ons_region_name %in% c("London", "South East", "Wales", "North West", "North East", "Scotland")) %>% 
  filter(year  == 2013) %>% 
  ggplot(.) + 
  geom_ribbon(
    aes(x=age, ymin=0, ymax=population),
    fill="lightgrey"
  ) + 
  geom_ribbon(
    aes(x=age, ymax=0, ymin=-internal_out), 
    fill="lightblue"
  ) + 
  geom_ribbon(
    aes(x=age, ymax=-internal_out, ymin=-(internal_out + international_out)), 
    fill="darkblue"
  ) + 
  geom_ribbon(
    aes(x=age, ymin=0, ymax=internal_in), 
    fill="red"
  ) + 
  geom_ribbon(
    aes(x=age, ymin=internal_in, ymax=(internal_in + international_in)), 
    fill="darkred") + 
  facet_wrap( ~ ons_region_name) +
  theme_minimal() +
  scale_y_continuous(labels=comma) +
  labs(title="Migration and Population Structure in different regions", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=-20000, ymax=150000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=-20000, ymax=150000, alpha=0.2) +
  theme(strip.text = element_text(size = rel(2.0)),
        title = element_text(size = rel(2.0)))

ggsave("figures/nankai_lecture_material/UK_Regions_many_years.png", 
       dpi=300,
       width=30, height=30, 
       units="cm"
)

