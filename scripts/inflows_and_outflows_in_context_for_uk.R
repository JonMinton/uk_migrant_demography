# UK migration in context including Scotland


rm(list=ls())


dta_uk <- read.csv("data/tidied/UK_population_migration_inc_Scotland.csv") %>% tbl_df

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

dta_uk  %>% 
  group_by(age, year, ons_region_name)  %>% 
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
  facet_grid(ons_region_name ~ year) +
  theme_minimal() +
  scale_y_continuous(labels=comma) +
  labs(title="Inflows and outflows in context", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=-20000, ymax=150000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=-20000, ymax=150000, alpha=0.2) 

ggsave("figures/regions/uk_inflow_outflow_context.png", 
       dpi=600,
       width=40, height=40, 
       units="cm"
)