# map percentage required to be protected or restored in each country to reach ambitious conservation targets

library(tidyverse)
library(sf)
library(tmap)
library(patchwork)
library(pals)
library(classInt)
library(lattice)

# data

mang <- read.csv('outputs/mangrove-capped.csv') %>% arrange(Country)
seag <- read.csv('outputs/seagrass-capped.csv')
countries <- st_read('data/UIA_World_Countries_Boundaries/UIA_World_Countries_Boundaries.shp')
countries$Country <- recode(countries$Country, `Russian Federation` = 'Russia', Curacao = 'Curaco',
                           `Brunei Darussalam` = 'Brunei',`Timor-Leste` = 'East Timor',
                           `Côte d'Ivoire` = 'Ivory Coast',`Congo DRC` = 'Democratic Republic of the Congo',
                           Philippines = 'Phillippines',`Saint Martin` = 'Saint-Martin',
                           `Saint Vincent and the Grenadines` = 'Saint Vincent', `US Virgin Islands` = 'Virgin Islands')
data('World')
landEEZ <- st_read('data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')
seag.sf <- seag %>% st_as_sf(coords = c('lat', 'long'), crs = '+proj=moll') %>% 
  st_transform(crs = 4326) %>% st_join(st_make_valid(landEEZ))
seag.sf$SOVEREIGN1 <- recode(seag.sf$SOVEREIGN1, Philippines = 'Phillippines')

# projections 

prot.rates.s <- read.csv('outputs/seagrass-protection-rates.csv')
rest.rates.s <- read.csv('outputs/seagrass-restoration-rates.csv')
rest.rates.m <- read.csv('outputs/mangrove-restoration-rates.csv')
prot.rates.m <- read.csv('outputs/mangrove-protection-rates.csv')

# calculate the percentage restored or protected in a country as a percentage of the historical area in a country

mang.P <- prot.rates.m %>% 
  filter(scenario == 'PR2') %>% 
  rename(Country = country) %>% 
  left_join(mang, by = 'Country') %>% 
  mutate(area_historical = Forest_ha + Deforested_ha,
         prot.total_ha = prot_ha_yr*(2030-2023)) %>% 
  mutate(prot.prop = (prot.total_ha/area_historical)*100)

mang.R <- rest.rates.m %>% 
  filter(scenario == 'PR2') %>% 
  rename(Country = country) %>% 
  left_join(mang, by = 'Country') %>% 
  mutate(area_historical = Forest_ha + Deforested_ha,
         rest.total_ha = rest_ha_yr*(2050-2023)) %>% 
  mutate(rest.prop = (rest.total_ha/area_historical)*100)

sea.R <- rest.rates.s %>% 
  filter(scenario == 'PR2') %>% 
  rename(study_site = site) %>% 
  left_join(select(data.frame(seag.sf), study_site, SOVEREIGN1, area_start_X50., max_area)) %>% 
  mutate(max_area = ifelse(area_start_X50. > max_area, area_start_X50., max_area)) %>% 
  group_by(SOVEREIGN1) %>%
  summarise(rest_ha_yr = sum(rest_ha_yr), max_area = sum(max_area)) %>%
  mutate(rest.prop = (rest_ha_yr*(2050-2023)/max_area)*100)

sea.P <- prot.rates.s %>% 
  filter(scenario == 'PR2') %>% 
  rename(study_site = site) %>% 
  left_join(select(data.frame(seag.sf), study_site, SOVEREIGN1, area_start_X50., max_area)) %>% 
  mutate(max_area = ifelse(area_start_X50. > max_area, area_start_X50., max_area)) %>% 
  group_by(SOVEREIGN1) %>%
  summarise(prot_ha_yr = sum(prot_ha_yr), max_area = sum(max_area)) %>%
  mutate(prot.prop = (prot_ha_yr/max_area)*100)

# bivariate plot of protection and restoration percentages

# mangroves

mang2 <- select(mang.P, Country, prot.prop) %>% 
  left_join(select(mang.R, Country, rest.prop), by = 'Country')

class1 <- mang2 %>% 
  mutate(rowid = seq(1, nrow(.), 1)) %>% 
  arrange(prot.prop) %>% 
  mutate(cut = ntile(prot.prop, n = 3)) %>% 
  mutate(cut = ifelse(prot.prop == 0, 1, cut)) %>% 
  arrange(rowid)

class2 <- mang2 %>% 
  mutate(rowid = seq(1, nrow(.), 1)) %>% 
  arrange(rest.prop) %>% 
  mutate(cut = ntile(rest.prop, n = 3)) %>% 
  mutate(cut = ifelse(rest.prop == 0, 1, cut)) %>% 
  arrange(rowid)

mang2$cut <- factor(class1$cut + 3 * (class2$cut - 1))

sea2 <- sea.R %>% 
  full_join(select(sea.P, SOVEREIGN1, prot.prop), by = 'SOVEREIGN1') %>% 
  mutate(prot.prop = ifelse(is.na(prot.prop), 0, prot.prop)) %>% 
  rename(Country = SOVEREIGN1)

class1 <- sea2 %>% 
  mutate(rowid = seq(1, nrow(.), 1)) %>% 
  arrange(prot.prop) %>% 
  mutate(cut = ntile(prot.prop, n = 3)) %>% 
  mutate(cut = ifelse(prot.prop == 0, 1, cut)) %>% 
  arrange(rowid)

class2 <- sea2 %>% 
  mutate(rowid = seq(1, nrow(.), 1)) %>% 
  arrange(rest.prop) %>% 
  mutate(cut = ntile(rest.prop, n = 3)) %>% 
  mutate(cut = ifelse(rest.prop == 0, 1, cut)) %>% 
  arrange(rowid)

sea2$cut <- factor(c(class1$cut + 3 * (class2$cut - 1)))

# create a legend

png('outputs/bilegend.png')
levelplot(matrix(1:(3 * 3), nrow = 3),
    axes = FALSE, col.regions = stevens.greenblue(n=9),
    xlab = "Percent protect", ylab = "Percent restore",
    cuts = 8, colorkey = FALSE, scales = list(draw = 0))
dev.off()

# map countries and bivarate color scheme

# mangroves

world.agg <- World %>% 
  summarise() %>% 
  st_transform(crs = 4326) %>%
  st_make_valid() %>% 
  st_crop(st_bbox(c(xmin = -180, xmax = 180, ymax = 50, ymin = -55)))

world.dat <- countries %>% 
  inner_join(mang2, by = 'Country') %>% 
  st_transform(crs = 4326) %>%
  st_make_valid() %>% 
  st_crop(st_bbox(c(xmin = -180, xmax = 180, ymax = 50, ymin = -55)))

index <- as.numeric(sort(as.character(unique(world.dat$cut))))
pal <- stevens.greenblue(n=9)[index]

mm <- tm_shape(world.agg) +
  tm_fill(col = 'grey15') +
  tm_shape(world.dat) +
  tm_polygons('cut', palette = pal,legend.show = F) +
  tm_layout(panel.label.size = 0.7,
            panel.label.bg.color = 'white',
            legend.text.size = 0.5,
            legend.title.size = 0.7,
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.position = c(0.45, 0.5),
            legend.height = 0.1,
            outer.margins = c(0,0,0,0),
            inner.margins = c(0,0,0,0),
            frame = T)
mm

# seagrass

world.agg2 <- World %>% 
  summarise() %>% 
  st_transform(crs = 4326) %>%
  st_make_valid() %>% 
  st_crop(st_bbox(c(xmin = -180, xmax = 180, ymax = 71, ymin = -55)))

world.dat2s <- countries %>% 
  inner_join(sea2, by = 'Country') %>% 
  st_transform(crs = 4326) %>%
  st_make_valid() %>% 
  st_crop(st_bbox(c(xmin = -180, xmax = 180, ymax = 71, ymin = -55)))

index <- as.numeric(sort(as.character(unique(world.dat2s$cut))))
pal <- stevens.greenblue(n=9)[index]

m3 <- tm_shape(world.agg2) +
  tm_fill(col = 'grey15') +
  tm_shape(world.dat2s) +
  tm_polygons('cut', palette = stevens.greenblue(n = 9), legend.show = F) +
  tm_layout(panel.label.size = 0.7,
            panel.label.bg.color = 'white',
            legend.text.size = 0.5,
            legend.title.size = 0.7,
            legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.position = c(0.45, 0.5),
            outer.margins = c(0,0,0,0),
            inner.margins = c(0,0,0,0),
            frame = T)
m3

v <- tmap_arrange(mm, m3, ncol = 1, nrow = 2, outer.margins = 0)

tmap_save(v, 'outputs/map-quadrants.png', width = 5, height = 3)

