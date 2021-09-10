# assess and plot the feasibility of required restoration rates

library(tidyverse)
library(patchwork)
library(sf)

# data

seag <- read.csv('outputs/seagrass-capped.csv')
landEEZ <- st_read('data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')
seag.sf <- seag %>% 
  st_as_sf(coords = c('lat', 'long'), crs = '+proj=moll') %>% 
  st_transform(crs = 4326) %>% st_join(st_make_valid(landEEZ))

# projected restoration and rates

rest.rates.s <- read.csv('outputs/seagrass-restoration-rates.csv') %>% 
  rename(study_site = site) %>% left_join(select(data.frame(seag.sf), study_site, SOVEREIGN1, ISO_SOV1))
rest.rates.m <- read.csv('outputs/mangrove-restoration-rates.csv')

# calculate number of ha's restored/year in a country
# take the average number of restoration projects/year (Duarte 2020)
# and mutiply by median size of restoration projects (Bayaktarov 2016)
# seagrass = 1.056541 ha, mangrove = 62943.95 ha

rest.obs <- read.csv('data/DuarteRestorationDataset-1.csv') %>%
  mutate(Latitude = as.numeric(as.character(Latitude)),
         Longitude = as.numeric(as.character(Longitude))) %>% 
  st_as_sf(coords= c('Longitude', 'Latitude'), crs = 4326) %>%
  st_join(st_make_valid(landEEZ)) %>% 
  data.frame %>% select(-geometry) %>% 
  mutate(proj = 1)

seag.rest <- rest.obs %>%
  filter(Habitat == 'Seagrass meadows') %>% 
  group_by(SOVEREIGN1, ISO_SOV1, Year) %>% 
  summarise(num = sum(proj)) %>% 
  group_by(SOVEREIGN1, ISO_SOV1) %>% 
  summarise(mean = mean(num)) %>% 
  mutate(rest_ha_yr_obs = mean * 1.056541)

mang.rest <- rest.obs %>%
  filter(Habitat == 'Mangrove') %>% 
  group_by(SOVEREIGN1, ISO_SOV1, Year) %>% 
  summarise(num = sum(proj)) %>% 
  group_by(SOVEREIGN1, ISO_SOV1) %>% 
  summarise(mean = mean(num)) %>% 
  mutate(rest_ha_yr_obs = mean * 62943.95)

# join projected restoration rates from intermediate (PR) and ambitious (PR2) scenarios

sea.R <- rest.rates.s %>% 
  filter(scenario %in% c('PR', 'PR2')) %>% 
  group_by(SOVEREIGN1, ISO_SOV1, scenario) %>% 
  summarise(rest_ha_yr = sum(rest_ha_yr)) %>% 
  inner_join(seag.rest, by = 'ISO_SOV1')

mang.R <- mang.rest %>%
  rename(country = SOVEREIGN1) %>% 
  inner_join(filter(rest.rates.m, scenario %in% c('PR', 'PR2')), by = 'country')

# plot

s <- ggplot() +
  geom_jitter(data = filter(sea.R, scenario == 'PR2'), aes(x = rest_ha_yr_obs/100, y = rest_ha_yr/100), col = 'lightgoldenrod') +
  geom_jitter(data = filter(sea.R, scenario == 'PR'), aes(x = rest_ha_yr_obs/100, y = rest_ha_yr/100), alpha = 0.5) +
  ggtitle('B) Seagrass') +
  ylab('') +
  xlab('Observed restoration rate (km2/yr)') +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))
s

m <- ggplot() +
  geom_jitter(data = filter(mang.R, scenario == 'PR2'), aes(x = rest_ha_yr_obs/100, y = rest_ha_yr/100), col = 'lightgoldenrod') +
  geom_jitter(data = filter(mang.R, scenario == 'PR'), aes(x = rest_ha_yr_obs/100, y = rest_ha_yr/100), alpha = 0.5) +
  ggtitle('A) Mangrove') +
  ylab('Projected restoration rate (km2/yr)') +
  xlab('Observed restoration rate (km2/yr)') +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))
m

cc <- m + s
cc

# make inset barplots of restoration rates under most ambitious scenario

sea.R2 <- sea.R %>% filter(scenario == 'PR2') %>% 
  pivot_longer(cols = c(rest_ha_yr, rest_ha_yr_obs), 
               names_to = 'rate', values_to = 'val')

sea.R2$label <- recode(sea.R2$rate, rest_ha_yr = 'P', rest_ha_yr_obs = 'O')

q <- ggplot() +
  geom_boxplot(data = sea.R2, aes(x = label, y = val/100), outlier.shape = NA) +
  geom_jitter(data = sea.R2, aes(x = label, y = val/100), alpha = 0.5) +
  ylab('Rate (km2/yr)') +
  xlab('') +
  theme_classic() +
  theme(text = element_text(size = rel(3.3)))
q

mang.R2 <- mang.R %>% filter(scenario == 'PR2') %>% 
  pivot_longer(cols = c(rest_ha_yr, rest_ha_yr_obs), 
               names_to = 'rate', values_to = 'val')

mang.R2$label <- recode(mang.R2$rate, rest_ha_yr = 'P', rest_ha_yr_obs = 'O')

t <- ggplot() +
  geom_boxplot(data = mang.R2, aes(x = label, y = val/100), outlier.shape = NA) +
  geom_jitter(data = mang.R2, aes(x = label, y = val/100), alpha = 0.5) +
  ylab('Rate (km2/yr)') +
  xlab('') +
  theme_classic() +
  theme(text = element_text(size = rel(3.3)))
t

# plot and save 

tt <- m + inset_element(t, 0.45, 0.4, 1, 1)
tt2 <- s + inset_element(q, 0.45, 0.4, 1, 1)

tt+tt2

ggsave('outputs/restoration-feasibility.png', width = 7.7, height = 3.4)


