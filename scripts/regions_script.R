

dta_regions %>% 
  ggplot(data=.) + 
  geom_line(aes(x=age, y=internal_net, colour=sex, group=sex)) + 
  facet_grid(ons_region_name~ year) + 
  geom_hline(aes(yintercept=0), linetype="dashed") +
  theme_minimal() +
  scale_colour_manual(name="Sex", values=c("red", "blue")) + 
  labs(title="Internal net migration", y="Count", x="Age")

ggsave("figures/regions/internal_net.png", 
       dpi=300,
       width=28, height=25, 
       units="cm"
)

dta_regions %>% 
  ggplot(data=.) + 
  geom_bar(aes(x=age, y=internal_out, fill=sex, colour=sex, group=sex), stat="identity") + 
  facet_grid(ons_region_name~ year) + 
  theme_minimal() +
  scale_fill_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_colour_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_y_continuous(labels=comma) +
  labs(title="Internal out-migration", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=20000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=0, ymax=20000, alpha=0.2)

ggsave("figures/regions/internal_out.png", 
       dpi=300,
       width=28, height=25, 
       units="cm"
)

dta_regions %>% 
  ggplot(data=.) + 
  geom_bar(aes(x=age, y=internal_in, fill=sex, colour=sex, group=sex), stat="identity") + 
  facet_grid(ons_region_name~ year) + 
  theme_minimal() +
  scale_fill_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_colour_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_y_continuous(labels=comma) +
  labs(title="Internal in-migration", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=20000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=0, ymax=20000, alpha=0.2)

ggsave("figures/regions/internal_in.png", 
       dpi=300,
       width=28, height=25, 
       units="cm"
)


dta_regions %>% 
  ggplot(data=.) + 
  geom_line(aes(x=age, y=international_net, colour=sex, group=sex)) + 
  facet_grid(ons_region_name ~ year) + 
  geom_hline(aes(yintercept=0), linetype="dashed") +
  theme_minimal() + 
  scale_y_continuous(labels=comma) +
  scale_colour_manual(name="Sex", values=c("red", "blue")) +
  labs(title="International net migration", y="Count", x="Age")

ggsave("figures/regions/international_net.png", 
       dpi=300,
       width=28, height=25, 
       units="cm"
)


dta_regions %>% 
  ggplot(data=.) + 
  geom_bar(aes(x=age, y=international_in, fill=sex, colour=sex, group=sex), stat="identity") + 
  facet_grid(ons_region_name ~ year) + 
  theme_minimal() +
  scale_y_continuous(labels=comma) + 
  scale_fill_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_colour_manual(name="Sex", values=c("red4", "steelblue4")) +
  labs(title="International in-migration", y="Count", x="Age")  + 
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=10000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=0, ymax=10000, alpha=0.2)


ggsave("figures/regions/international_in.png", 
       dpi=300,
       width=28, height=25, 
       units="cm"
)


dta_regions %>% 
  ggplot(data=.) + 
  geom_bar(aes(x=age, y=international_out, fill=sex, colour=sex, group=sex), stat="identity")  + 
  facet_grid(ons_region_name ~ year) + 
  theme_minimal() +
  scale_y_continuous(labels=comma) + 
  scale_fill_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_colour_manual(name="Sex", values=c("red4", "steelblue4")) +
  labs(title="International out-migration", y="Count", x="Age") + 
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=6000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=0, ymax=6000, alpha=0.2)

ggsave("figures/regions/international_out.png", 
       dpi=300,
       width=28, height=25, 
       units="cm"
)



dta_regions %>%
  mutate(mig_prop = international_in / population) %>%
  ggplot(data=.) + 
  geom_line(aes(x=age, y=mig_prop, colour=sex, group=sex)) +
  facet_grid(ons_region_name ~ year) + 
  theme_minimal() +
  scale_colour_manual(name="Sex", values=c("red", "blue")) +
  labs(
    title="Proportion of population who are \ninternational migrants", 
    y="Proportion", x="Age"
  ) 

ggsave("figures/regions/international_immigrant_prop.png", 
       dpi=300,
       width=28, height=25, 
       units="cm"
)


dta_regions %>%
  mutate(mig_prop = international_out / population) %>%
  ggplot(data=.) + 
  geom_line(aes(x=age, y=mig_prop, colour=sex, group=sex)) + 
  facet_grid(ons_region_name~ year) + 
  theme_minimal() +
  scale_colour_manual(name="Sex", values=c("red", "blue")) +
  labs(
    title="Proportion of population who \nemmigrate internationally", 
    y="Proportion", x="Age"
  ) 

ggsave("figures/regions/international_outmigrant_prop.png", 
       dpi=300,
       width=28, height=25, 
       units="cm"
)


dta_regions %>% 
  ggplot(data=.) + 
  geom_bar(aes(x=age, y=population, fill=sex, colour=sex, group=sex), stat="identity")  + 
  facet_grid(ons_region_name~ year) + 
  theme_minimal() +
  scale_y_continuous(labels=comma) + 
  scale_fill_manual(name="Sex", values=c("red4", "steelblue4")) +
  scale_colour_manual(name="Sex", values=c("red4", "steelblue4")) +
  labs(title="Population", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=0, ymax=150000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=0, ymax=150000, alpha=0.2) 

ggsave("figures/regions/english_population.png", 
       dpi=300,
       width=28, height=25, 
       units="cm"
)



# Population in context ---------------------------------------------------

dta_regions  %>% 
  group_by(age, year, ons_region_name)  %>% 
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
  facet_grid(ons_region_name ~ year) +
  theme_minimal() +
  scale_y_continuous(labels=comma) +
  labs(title="Inflows and outflows in context", y="Count", x="Age") +
  annotate("rect", xmin=0, xmax=18, ymin=-20000, ymax=150000, alpha=0.2) +
  annotate("rect", xmin=60, xmax=91, ymin=-20000, ymax=150000, alpha=0.2) 

ggsave("figures/regions/english_inflow_outflow_context.png", 
       dpi=600,
       width=30, height=25, 
       units="cm"
)

# contour plots -----------------------------------------------------------

png("figures/contour_population.png",
    width=30, 
    height=30,
    res=300,
    units="cm"
)

dta_regions %>%
  mutate(population = population / 1000) %>%
  filter(age <90) %>%
  contourplot(
    population ~ year + age | ons_region_name + sex,
    data=.,
    region=T,
    cuts=25,
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
