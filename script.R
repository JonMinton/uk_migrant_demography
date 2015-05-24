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
require(scales)


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


# small multiples ---------------------------------------------------------



dta_england %>% 
  ggplot(data=.) + 
  geom_line(aes(x=age, y=internal_net, colour=sex, group=sex)) + 
  facet_wrap(~ year, nrow=4) + 
  geom_hline(aes(yintercept=0), linetype="dashed") +
  theme_minimal() +
  scale_colour_manual(name="Sex", values=c("red", "blue")) + 
  labs(title="Internal net migration", y="Count", x="Age")

ggsave("figures/internal_net.tiff", 
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

ggsave("figures/internal_out.tiff", 
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

ggsave("figures/internal_in.tiff", 
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

ggsave("figures/international_net.tiff", 
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


ggsave("figures/international_in.tiff", 
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

ggsave("figures/international_out.tiff", 
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

ggsave("figures/international_immigrant_prop.tiff", 
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
ggsave("figures/international_outmigrant_prop.tiff", 
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
  
ggsave("figures/english_population.tiff", 
       dpi=300,
       width=20, height=25, 
       units="cm"
)



# contour plots -----------------------------------------------------------


dta_england %>%
  mutate(population = population / 1000) %>%
  filter(age <90) %>%
  contourplot(
    population ~ age + year | sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=25,
    col.regions=colorRampPalette(brewer.pal(6, "Reds"))(200),
    scales=list(alternating=3),
    col="blue",
    strip=strip.custom(bg="lightgrey"),
    par.strip.text=list(fontface="bold")  
    )

dta_england %>%
  contourplot(
    international_in ~ age + year |  sex,
    data=.,
    region=T,
    layout=c(1,2), cuts=25,
    col.regions=colorRampPalette(brewer.pal(6, "Spectral"))(200),
    scales=list(alternating=3)
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

m_international_in <- dta_england  %>% 
  filter(sex=="male")  %>% 
  select(age, year, international_in)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=international_in)   %>% 
  make_matrix

persp3d(m_international_in, col="white", axes=F)


f_population <- dta_england  %>% 
  filter(sex=="female")  %>% 
  select(age, year, population)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=population)   %>% 
  make_matrix

persp3d(f_population, col="white", axes=F)

m_population <- dta_england  %>% 
  filter(sex=="female")  %>% 
  select(age, year, population)  %>% 
  ungroup  %>% 
  select(-sex)  %>% 
  spread(key=year, value=population)   %>% 
  make_matrix

persp3d(m_population, col="white", axes=F)


