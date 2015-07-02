# Visualisation of UK regional migration and population structures

Dr Jonathan Minton
Univeristy of Glasgow, UK 
jonathan.minton@glasgow.ac.uk 

##Introduction 
Welcome to the github repository containing the material needed to produce high resolution images showing the changing population structure in different English and Welsh regions, and the contribution of internal and international migration to the age structures in different regions. 

The data used are the Components of Change (CoC) dataset produced by the UK ONS. This data contains population counts, death counts, and counts of internal and international migration counts at local authority (LA) level, by single age (mid year population estimates) and by gender, for each year between 2002 and 2013 inclusive. Internal migration is defined as a movement between local authorities. 
For regional estimates, the estimates from LAs were aggregated up. This means that some of the internal migrations will be intra-regional. 

##Types of visualisation 
Two main types of visualisation are produced. Firstly, small multiples or Trellis plots, which show the age-structure of populations aind migration flows for a range of regions and for each available year. Secondly, shaded contour plots (SCPs), which present population structures and and migration flows as a series of 'maps' on Lexis surfaces, which are tabular arrangements of data with year across one axis and age across the other axis. 


##Software used 
All analyses were produced using R. The small multiples/trellis plots were produced using the ggplot2 package, and the SCPs were produced using the lattice package. 

## Further reading
- [Minton, Vanderbloemen & Dorling (2013)](http://ije.oxfordjournals.org/content/42/4/1164.full), 'Visualising Europe's Demographic Scars using Coplots and contour plots', International Journal of Epidemiology
- [Minton (2014)](http://www.sciencedirect.com/science/article/pii/S1877584514000173), 'Real Geographies and Virtual Landscapes', Spatial and Spatiotemporal Epidemiology

## Outputs form this work
- [International Conference on Population Geographies (30 June-3 July 2015)](http://popgeog.org/2014/10/eighth-international-conference-on-population-geographies-brisbane-australia-30-june-2015-to-3-july-2015/), Brisbane Australia


## Coming up 
I now have comparable data for Scotland, kindly supplied by the National Records of Scotland, and will update the figures to include this data as well as the English & Welsh CoC data. 


