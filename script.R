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

require(openxlsx) 
require(readr)
# for scotland population data



require(plyr)
require(stringr)
require(tidyr)
require(dplyr)


require(RColorBrewer)
require(rgl)
require(ggplot2)
require(lattice)
require(r2stl)
require(scales)

# Data --------------------------------------------------------------------

dta <- read_csv(file="data/tidied/england_la_count.csv") 

region_lookups <- read_csv(file="data/support/lookups_between_ons_and_ecodes.csv") 

region_lookups <- region_lookups %>%
  select(ons_code, ons_region_code, ons_region_name)

dta_england <- dta  %>% 
  select(country, sex, age, year, internal_in, internal_out, international_in, international_out, population)  %>% 
  filter(country=="E") %>%
  group_by(sex, age, year)  %>% 
  filter(!is.na(internal_in) & !is.na(internal_out) & !is.na(international_in) & !is.na(international_out))  %>% 
  summarise(
    internal_in = sum(internal_in), 
    internal_out = sum(internal_out), 
    international_in = sum(international_in), 
    international_out=sum(international_out),
    population=sum(population)
    )  %>% 
  mutate(
    international_net = international_in - international_out, 
    internal_net = internal_in - internal_out)  

dta_wales  <- dta  %>% 
  select(country, sex, age, year, internal_in, internal_out, international_in, international_out, population)  %>% 
  filter(country=="W") %>%
  group_by(sex, age, year)  %>% 
  filter(!is.na(internal_in) & !is.na(internal_out) & !is.na(international_in) & !is.na(international_out))  %>% 
  summarise(
    internal_in = sum(internal_in), 
    internal_out = sum(internal_out), 
    international_in = sum(international_in), 
    international_out=sum(international_out),
    population=sum(population)
  )  %>% 
  mutate(
    ons_region_name = "Wales",
    international_net = international_in - international_out, 
    internal_net = internal_in - internal_out)  %>% 
  select(
    ons_region_name, sex, age, year, internal_in,
    internal_out, international_in, international_out, population, 
    international_net, internal_net
    )

dta_regions <- dta %>%
  select(lad2013_code, country, age, sex, year, internal_in, internal_out, international_in, international_out, population) %>%
  filter(country=="E") %>%
  left_join(region_lookups, by=c("lad2013_code"="ons_code")) %>%
  group_by(ons_region_name, sex, age, year)  %>% 
  filter(!is.na(internal_in) & !is.na(internal_out) & !is.na(international_in) & !is.na(international_out))  %>% 
  summarise(
    internal_in = sum(internal_in), 
    internal_out = sum(internal_out), 
    international_in = sum(international_in), 
    international_out=sum(international_out),
    population=sum(population)
  )  %>% 
  mutate(
    international_net = international_in - international_out, 
    internal_net = internal_in - internal_out
    )  

#Add Wales as a 'region'
dta_regions <- bind_rows(dta_regions, dta_wales)

# Now to look at Scotland 

# most interesting challenge will be populations, directly 
# from excel sheet

wb <- loadWorkbook(file="data/scotland/cae8114-single-year-of-age.xlsx")

years <- 1981:2014

dta_list <- lapply(as.character(years), function(x) read.xlsx(wb, sheet=x, startRow=3))
names(dta_list) <- years

convert_to_df <- function(x){
  y <- x %>% 
    filter(Persons=="Scotland") %>% 
    select(-Persons, -All.Ages) %>% 
    gather(key="age", value="population_count") 
  
  y$sex <- c("total", "male", "female")
  
  y$age <- str_replace_all(y$age, "[^0-9]", "") # non-numeric chars removed
  y$age <- as.numeric(as.character(y$age))
  
  return(y)
}

dta_df <- ldply(dta_list, convert_to_df, .id="year")
dta_df$region <- "Scotland"

dta_df$year <- as.numeric(as.character(dta_df$year))

write_csv(dta_df, path="data/tidied/scotland_population.csv")  

 # internal migration

internal_mgrtn <- read_csv("data/scotland/internal_migs_by_age_2002_2013_ins_outs.csv") 

international_mgrtn <- read_csv("data/scotland/os_migs_by_age_2002_2013.csv") 

dta_scot_pop <- dta_df %>% filter(sex=="total") %>% 
  select(ons_region_name=region, age, year, population=population_count) %>% tbl_df

dta_scot_internal <- internal_mgrtn %>% 
  select(ons_region_name=country, age, year, internal_in=inmigrant_count, internal_out = outmigrant_count)


dta_scot_international <- international_mgrtn %>% 
  select(ons_region_name=country, age, year, international_in=inmigrant_count, international_out = outmigrant_count)


dta_scot <- dta_scot_pop %>% 
  inner_join(dta_scot_internal) %>% 
  inner_join(dta_scot_international) %>% 
  select(ons_region_name, age, year, internal_in, 
         internal_out, international_in, international_out, population)

dta_scot$population <- as.numeric(dta_scot$population)
# Scotland data not available by sex, so for comparison data needs to be needs to be 
# aggregated by sex

dta_regions_sex_combined <- dta_regions %>% 
  group_by(ons_region_name, age, year) %>% 
  summarise_each(funs(sum), -sex)

dta_regions_sex_combined <- bind_rows(
  dta_regions_sex_combined, 
  dta_scot
  )

write_csv(dta_regions_sex_combined, path="data/tidied/UK_population_migration_inc_Scotland.csv")
# small multiples ---------------------------------------------------------


dta_england %>% 
  ggplot(data=.) + 
  geom_line(aes(x=age, y=internal_net, colour=sex, group=sex)) + 
  facet_wrap(~ year, nrow=4) + 
  geom_hline(aes(yintercept=0), linetype="dashed") +
  theme_minimal() +
  scale_colour_manual(name="Sex", values=c("red", "blue")) + 
  labs(title="Internal net migration", y="Count", x="Age")

ggsave("figures/internal_net.png", 
       dpi=300,
       width=20, height=25, 
       units="cm"
       )

dta_england %>% 
  ggplot(data=.) + 
  geom_bar(aes(x=age, y=internal_out, fill=sex, colour=sex, group=sex), stat="identity") + 
  facet_wrap(~ year, nrow=4) + 
  theme_minimal() +
  scale_fill_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_colour_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_y_continuous(labels=comma) +
  labs(title="Internal out-migration", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=150000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=0, ymax=150000, alpha=0.2)

ggsave("figures/internal_out.png", 
       dpi=300,
       width=20, height=25, 
       units="cm"
      )

dta_england %>% 
  ggplot(data=.) + 
  geom_bar(aes(x=age, y=internal_in, fill=sex, colour=sex, group=sex), stat="identity") + 
  facet_wrap(~ year, nrow=4) + 
  theme_minimal() +
  scale_fill_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_colour_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_y_continuous(labels=comma) +
  labs(title="Internal in-migration", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=150000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=0, ymax=150000, alpha=0.2)

ggsave("figures/internal_in.png", 
       dpi=300,
       width=20, height=25, 
       units="cm"
)


dta_england %>% 
  ggplot(data=.) + 
  geom_line(aes(x=age, y=international_net, colour=sex, group=sex)) + 
  facet_wrap(~ year, nrow=4) + 
  geom_hline(aes(yintercept=0), linetype="dashed") +
  theme_minimal() + 
  scale_y_continuous(labels=comma) +
  scale_colour_manual(name="Sex", values=c("red", "blue")) +
  labs(title="International net migration", y="Count", x="Age")

ggsave("figures/international_net.png", 
       dpi=300,
       width=20, height=25, 
       units="cm"
)


dta_england %>% 
  ggplot(data=.) + 
  geom_bar(aes(x=age, y=international_in, fill=sex, colour=sex, group=sex), stat="identity") + 
  facet_wrap(~ year, nrow=4) + 
  theme_minimal() +
  scale_y_continuous(labels=comma) + 
  scale_fill_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_colour_manual(name="Sex", values=c("red4", "steelblue4")) +
  labs(title="International in-migration", y="Count", x="Age")  + 
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=40000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=0, ymax=40000, alpha=0.2)


ggsave("figures/international_in.png", 
       dpi=300,
       width=20, height=25, 
       units="cm"
)


dta_england %>% 
  ggplot(data=.) + 
  geom_bar(aes(x=age, y=international_out, fill=sex, colour=sex, group=sex), stat="identity")  + 
  facet_wrap(~ year, nrow=4) + 
  theme_minimal() +
  scale_y_continuous(labels=comma) + 
  scale_fill_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_colour_manual(name="Sex", values=c("red4", "steelblue4")) +
  labs(title="International out-migration", y="Count", x="Age") + 
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=20000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=0, ymax=20000, alpha=0.2)

ggsave("figures/international_out.png", 
       dpi=300,
       width=20, height=25, 
       units="cm"
)



dta_england %>%
  mutate(mig_prop = international_in / population) %>%
  ggplot(data=.) + 
  geom_line(aes(x=age, y=mig_prop, colour=sex, group=sex)) +
  facet_wrap(~ year, nrow=4) + 
  theme_minimal() +
  scale_colour_manual(name="Sex", values=c("red", "blue")) +
  labs(
    title="Proportion of population who are \ninternational migrants", 
    y="Proportion", x="Age"
    ) 

ggsave("figures/international_immigrant_prop.png", 
       dpi=300,
       width=20, height=25, 
       units="cm"
)


dta_england %>%
  mutate(mig_prop = international_out / population) %>%
  ggplot(data=.) + 
  geom_line(aes(x=age, y=mig_prop, colour=sex, group=sex)) + 
  facet_wrap(~ year, nrow=4) + 
  theme_minimal() +
  scale_colour_manual(name="Sex", values=c("red", "blue")) +
  labs(
    title="Proportion of population who \nemmigrate internationally", 
    y="Proportion", x="Age"
    ) 
ggsave("figures/international_outmigrant_prop.png", 
       dpi=300,
       width=20, height=25, 
       units="cm"
)


dta_england %>% 
  ggplot(data=.) + 
  geom_bar(aes(x=age, y=population, fill=sex, colour=sex, group=sex), stat="identity")  + 
  facet_wrap(~ year, nrow=4) + 
  theme_minimal() +
  scale_y_continuous(labels=comma) + 
  scale_fill_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_colour_manual(name="Sex", values=c("red4", "steelblue4")) +
  labs(title="Population", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=800000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=0, ymax=800000, alpha=0.2) 
  
ggsave("figures/english_population.png", 
       dpi=300,
       width=20, height=25, 
       units="cm"
)



# Population in context ---------------------------------------------------

dta_england  %>% 
  group_by(age, year)  %>% 
  summarise_each( funs(sum), -sex) %>%
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
  facet_wrap(~year) +
  theme_minimal() +
  scale_y_continuous(labels=comma) +
  labs(title="Inflows and outflows in context", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=-200000, ymax=800000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=-200000, ymax=800000, alpha=0.2) 
ggsave("figures/english_inflow_outflow_context.png", 
       dpi=300,
       width=20, height=25, 
       units="cm"
)

# contour plots -----------------------------------------------------------

png("figures/contour_population.png",
     width=30, 
     height=30,
     res=300,
     units="cm"
     )

dta_england %>%
  mutate(population = population / 1000) %>%
  filter(age <90) %>%
  contourplot(
    population ~ age + year | sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=25,
    col.regions=colorRampPalette(brewer.pal(3, "Blues"))(200),
    scales=list(
      alternating=3,
      x=list(cex=1.4),
      y=list(cex=1.4)
      ),
    col="blue",
    label=list(cex=1.2, fontface="bold"),
    strip=strip.custom(bg="lightgrey"),
    par.strip.text=list(fontface="bold"),
    xlab=list(label="Age in years", cex=1.4),
    ylab=list(label="Year", cex=1.4)
    )
dev.off()

png("figures/contour_international_in.png",
    width=30, 
    height=30,
    res=300,
    units="cm"
)

dta_england %>%
  mutate(international_in = international_in / 1000) %>%
  filter(age <90) %>%
    contourplot(
    international_in ~ age + year |  sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=12,
    col.regions=colorRampPalette(brewer.pal(3, "Blues"))(200),
    scales=list(
      alternating=3,
      x=list(cex=1.4),
      y=list(cex=1.4)
    ),
    col="blue",
    label=list(cex=1.2, fontface="bold"),
    strip=strip.custom(bg="lightgrey"),
    par.strip.text=list(fontface="bold"),
    xlab=list(label="Age in years", cex=1.4),
    ylab=list(label="Year", cex=1.4)
  )
dev.off()

png("figures/contour_international_out.png",
    width=30, 
    height=30,
    res=300,
    units="cm"
)
dta_england %>%
  mutate(international_out = international_out / 1000) %>%
  filter(age <90) %>%
  contourplot(
    international_out ~ age + year |  sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=12,
    col.regions=colorRampPalette(brewer.pal(3, "Reds"))(200),
    scales=list(
      alternating=3,
      x=list(cex=1.4),
      y=list(cex=1.4)
    ),
    col="red",
    label=list(cex=1.2, fontface="bold"),
    strip=strip.custom(bg="lightgrey"),
    par.strip.text=list(fontface="bold"),
    xlab=list(label="Age in years", cex=1.4),
    ylab=list(label="Year", cex=1.4)
  )
dev.off()

png("figures/contour_internal_in.png",
    width=30, 
    height=30,
    res=300,
    units="cm"
)
dta_england %>%
  mutate(internal_in = internal_in / 1000) %>%
  filter(age < 90) %>%
  contourplot(
    internal_in ~ age + year |  sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=12,
    col.regions=colorRampPalette(brewer.pal(3, "Blues"))(200),
    scales=list(
      alternating=3,
      x=list(cex=1.4),
      y=list(cex=1.4)
    ),
    col="blue",
    label=list(cex=1.2, fontface="bold"),
    strip=strip.custom(bg="lightgrey"),
    par.strip.text=list(fontface="bold"),
    xlab=list(label="Age in years", cex=1.4),
    ylab=list(label="Year", cex=1.4)
  )
dev.off()


png("figures/contour_internal_out.png",
    width=30, 
    height=30,
    res=300,
    units="cm"
)
dta_england %>%
  mutate(internal_out = internal_out / 1000) %>%
  filter(age < 90) %>%
  contourplot(
    internal_out ~ age + year |  sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=12,
    col.regions=colorRampPalette(brewer.pal(3, "Reds"))(200),
    scales=list(
      alternating=3,
      x=list(cex=1.4),
      y=list(cex=1.4)
    ),
    col="red",
    label=list(cex=1.2, fontface="bold"),
    strip=strip.custom(bg="lightgrey"),
    par.strip.text=list(fontface="bold"),
    xlab=list(label="Age in years", cex=1.4),
    ylab=list(label="Year", cex=1.4)
  )
dev.off()



# rgl ---------------------------------------------------------------------


make_matrix <- function(input){
  clnms <- names(input)[-1]
  rwnms <- input$age
  output <- as.matrix(input)
  output <- output[,-1]
  rownames(output) <- rwnms
  colnames(output) <- clnms
  
  return(output)
}

f_international_in <- dta_england  %>% 
  filter(sex=="female")  %>% 
  select(age, year, international_in)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=international_in)   %>% 
  make_matrix

persp3d(f_international_in, col="white", axes=F)

f_international_in %>%
  r2stl(
  x=as.numeric(rownames(.)),
  y=as.numeric(colnames(.)),
  z=.,
  filename="stls/f_international_in.stl", 
  z.expand=T
  )

m_international_in <- dta_england  %>% 
  filter(sex=="male")  %>% 
  select(age, year, international_in)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=international_in)   %>% 
  make_matrix

persp3d(m_international_in, col="white", axes=F)

m_international_in %>%
  r2stl(
    x=as.numeric(rownames(.)),
    y=as.numeric(colnames(.)),
    z=.,
    filename="stls/m_international_in.stl", 
    z.expand=T
  )


f_population <- dta_england  %>% 
  filter(sex=="female")  %>% 
  select(age, year, population)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=population)   %>% 
  make_matrix

persp3d(f_population, col="white", axes=F)
f_population %>%
  r2stl(
    x=as.numeric(rownames(.)),
    y=as.numeric(colnames(.)),
    z=.,
    filename="stls/f_population.stl", 
    z.expand=T
  )


m_population <- dta_england  %>% 
  filter(sex=="female")  %>% 
  select(age, year, population)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=population)   %>% 
  make_matrix

persp3d(m_population, col="white", axes=F)

m_population %>%
  r2stl(
    x=as.numeric(rownames(.)),
    y=as.numeric(colnames(.)),
    z=.,
    filename="stls/m_population.stl", 
    z.expand=T
  )



f_international_out <- dta_england  %>% 
  filter(sex=="female")  %>% 
  select(age, year, international_out)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=international_out)   %>% 
  make_matrix

persp3d(f_international_out, col="white", axes=F)

f_international_out %>%
  r2stl(
    x=as.numeric(rownames(.)),
    y=as.numeric(colnames(.)),
    z=.,
    filename="stls/f_international_out.stl", 
    z.expand=T
  )

m_international_out <- dta_england  %>% 
  filter(sex=="male")  %>% 
  select(age, year, international_out)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=international_out)   %>% 
  make_matrix

persp3d(m_international_out, col="white", axes=F)

m_international_out %>%
  r2stl(
    x=as.numeric(rownames(.)),
    y=as.numeric(colnames(.)),
    z=.,
    filename="stls/m_international_out.stl", 
    z.expand=T
  )


f_internal_in <- dta_england  %>% 
  filter(sex=="female")  %>% 
  select(age, year, internal_in)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=internal_in)   %>% 
  make_matrix

persp3d(f_internal_in, col="white", axes=F)

f_internal_in %>%
  r2stl(
    x=as.numeric(rownames(.)),
    y=as.numeric(colnames(.)),
    z=.,
    filename="stls/f_internal_in.stl", 
    z.expand=T
  )

m_internal_in <- dta_england  %>% 
  filter(sex=="male")  %>% 
  select(age, year, internal_in)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=internal_in)   %>% 
  make_matrix

persp3d(m_internal_in, col="white", axes=F)

m_internal_in %>%
  r2stl(
    x=as.numeric(rownames(.)),
    y=as.numeric(colnames(.)),
    z=.,
    filename="stls/m_internal_in.stl", 
    z.expand=T
  )


f_internal_out <- dta_england  %>% 
  filter(sex=="female")  %>% 
  select(age, year, internal_out)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=internal_out)   %>% 
  make_matrix

persp3d(f_internal_out, col="white", axes=F)

f_internal_out %>%
  r2stl(
    x=as.numeric(rownames(.)),
    y=as.numeric(colnames(.)),
    z=.,
    filename="stls/f_internal_out.stl", 
    z.expand=T
  )

m_internal_out <- dta_england  %>% 
  filter(sex=="male")  %>% 
  select(age, year, internal_out)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=internal_out)   %>% 
  make_matrix

persp3d(m_internal_out, col="white", axes=F)

m_internal_out %>%
  r2stl(
    x=as.numeric(rownames(.)),
    y=as.numeric(colnames(.)),
    z=.,
    filename="stls/m_internal_out.stl", 
    z.expand=T
  )


# Info for scaling:

#   sex           internal_in internal_out international_in international_out population
#  female       91840        91835            20661             12110     426452
#    male       75851        75805            22227             11672     420409

