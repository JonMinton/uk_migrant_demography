# UK migration in context including Scotland


rm(list=ls())


dta_uk <- read.csv("data/tidied/UK_population_migration_inc_Scotland.csv") %>% tbl_df



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


# Proportionate  ----------------------------------------------------------


#The purpose of this section is to do the above, but scaled to 100 % 

dta_uk_prop <- dta_uk
dta_uk_prop <- dta_uk_prop %>% mutate_each(funs(. / population), -ons_region_name, -age, -year)




dta_uk_prop  %>% 
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
  annotate("rect", xmin=0, xmax=18, ymin=-0.1, ymax=1, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=-0.1, ymax=1, alpha=0.2) 

ggsave("figures/regions/uk_inflow_outflow_context_proportion.png", 
       dpi=600,
       width=40, height=40, 
       units="cm"
)




# PDf Book  -------------------------------------------------------------

levels(
  dta_uk$ons_region_name
)[levels(dta_uk$ons_region_name)=="Yorkshire\n/Humber"] <- "Yorkshire and The Humber"


animate_year <- function(x){
  this_year <- x$year[1]
  this_region <- x$ons_region_name[1]
  
  a <- x  %>% 
    group_by(age, year, ons_region_name)  %>% 
    filter(year == this_year) %>% 
    filter(ons_region_name == this_region) %>% 
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
    
    theme_minimal() +
    scale_y_continuous(limits = c(-35000, 180000), labels=comma) +
    scale_x_continuous(limits = c(0, 91), breaks = seq(5, 90, by = 5)) + 
    labs(title=paste0(this_region, ", ", this_year), y="Count", x="Age") +
    annotate("rect", xmin=0, xmax=18, ymin=-20000, ymax=150000, alpha=0.2) +
    annotate("rect", xmin=60, xmax=91, ymin=-20000, ymax=150000, alpha=0.2) 
  
  a
}

pdf("figures/animation/pdfbook.pdf", width = 7, height = 7)

d_ply(dta_uk, .(ons_region_name, year), animate_year, .print =TRUE, .progress = "text")

dev.off()




# England/Wales migration in context

# prereqs -----------------------------------------------------------------

require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

require(openxlsx) 
# for scotland population data

require(RColorBrewer)
require(rgl)
require(ggplot2)
require(lattice)
require(r2stl)
require(scales)


rm(list=ls())


dta_uk <- read.csv("data/tidied/UK_population_migration_inc_Scotland.csv") %>% tbl_df

dta_uk <- dta_uk %>% filter(ons_region_name != "Scotland")

levels(
  dta_uk$ons_region_name
)[levels(dta_uk$ons_region_name)=="Yorkshire and The Humber"] <- "Yorkshire\n/Humber"

dta_uk$ons_region_name <- factor(dta_uk$ons_region_name, 
                                 levels =  c(
                                   "London", "South East", "South West",
                                   "East of England", "East Midlands", "West Midlands",
                                   "North East", "North West", "Yorkshire\n/Humber",
                                   "Wales"
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
  labs(title="Inflows and outflows in context", y="Count", x="Age in single years") +
  annotate("rect", xmin=0, xmax=18, ymin=-20000, ymax=150000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=-20000, ymax=150000, alpha=0.2) 

ggsave("figures/regions/engwales_inflow_outflow_context.png", 
       dpi=600,
       width=40, height=40, 
       units="cm"
)


