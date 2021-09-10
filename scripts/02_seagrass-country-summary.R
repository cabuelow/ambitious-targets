# produce summary for information for seagrass countries (number of sites, and temporal range of observations)

library(tidyverse)
library(scales)
library(sf)

# data

seag <- read.csv('outputs/seagrass-capped.csv')
landEEZ <- st_read('data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')

# calculate summaries

seag.stat <- select(data.frame(seag %>% st_as_sf(coords = c('lat', 'long'), crs = '+proj=moll') %>% 
                                 st_transform(crs = 4326) %>% 
                                 st_join(st_make_valid(landEEZ)) %>% 
                                 mutate(n_obs = rep(1, nrow(seag))) %>% 
                                 group_by(SOVEREIGN1) %>% 
                                 summarise(n_obs = sum(n_obs), min_year = min(year), max_year = max(maxyear))), - geometry)

write.csv(seag.stat, 'outputs/seag-country-sample-size.csv', row.names = F)