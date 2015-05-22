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

require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

require(RColorBrewer)
require(rgl)
require(ggplot2)
require(lattice)
require(r2stl)


# Data --------------------------------------------------------------------

dta <- read.csv(file="data/tidied/england_la_count.csv") %>%
  tbl_df


dta_england <- dta  %>% 
  select(country, sex, age, year, internal_in, internal_out, international_in, international_out, population)  %>% 
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

dta_england %>% 
  ggplot(data=.) + 
  geom_line(aes(x=age, y=internal_net, colour=sex, group=sex)) + facet_wrap(~ year, nrow=4)


dta_england %>%
  contourplot(
    population ~ age + year | sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=25,
    col.regions=colorRampPalette(brewer.pal(6, "Spectral"))(200)
  )

dta_england %>%
  contourplot(
    international_in ~ age + year |  sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=25,
    col.regions=colorRampPalette(brewer.pal(6, "Spectral"))(200)
  )

dta_england %>%
  contourplot(
    international_out ~ age + year |  sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=25,
    col.regions=colorRampPalette(brewer.pal(6, "Spectral"))(200)
  )

dta_england %>%
  contourplot(
    internal_in ~ age + year |  sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=25,
    col.regions=colorRampPalette(brewer.pal(6, "Spectral"))(200)
  )

dta_england %>%
  contourplot(
    internal_out ~ age + year |  sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=25,
    col.regions=colorRampPalette(brewer.pal(6, "Spectral"))(200)
  )
