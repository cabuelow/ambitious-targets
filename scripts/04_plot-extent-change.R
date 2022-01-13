# extent change stats and bar plot of extent change under each scenario

library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(sf)
source('scripts/helper_functions/new_scale.R')

# data

mang <- read.csv('outputs/mangrove-capped.csv') %>% 
  arrange(Country)
seag <- read.csv('outputs/seagrass-capped.csv')
landEEZ <- st_read('data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')
seag.sf <- seag %>% st_as_sf(coords = c('lat', 'long'), crs = '+proj=moll') %>% 
  st_transform(crs = 4326) %>% st_join(st_make_valid(landEEZ))

# projections 

seag.proj <- read.csv('outputs/seagrass-projections.csv')
mang.proj <- read.csv('outputs/mangrove-projections.csv')

# calculate extent change summary stats under scenarios, since action was implemented in 2023

# mangrove

mang.proj2 <- mang.proj %>% 
  pivot_wider(id_cols = c(scenario, country, year), 
              names_from = 'state', values_from = 'area_ha') %>% 
  filter(year %in% c(2023, 2030, 2050, 2070)) %>% 
  mutate(area = PF+UF) %>% 
  pivot_wider(id_cols = c(scenario, country), names_from = 'year', values_from = 'area') %>% 
  rename(area_2023_ha = `2023`,area_2030_ha = `2030`, area_2050_ha = `2050`, area_2070_ha = `2070`) %>% 
  group_by(scenario) %>% 
  summarise_at(vars(area_2023_ha:area_2070_ha), sum) %>% 
  mutate(extent_change_ha_2023_2030 = area_2030_ha-area_2023_ha,
         extent_change_ha_2023_2050 = area_2050_ha-area_2023_ha,
         extent_change_ha_2023_2070 = area_2070_ha-area_2023_ha,
         area_historical_ha_1996 = sum(mang$Forest_ha) + sum(mang$Deforested_ha)) %>% 
  mutate(perc_change_2023_2030 = ((area_2030_ha-area_2023_ha)/area_2023_ha)*100,
         perc_change_2023_2050 = ((area_2050_ha-area_2023_ha)/area_2023_ha)*100,
         perc_historical_2030 = (extent_change_ha_2023_2030/area_historical_ha_1996)*100,
         perc_historical_2050 = (extent_change_ha_2023_2050/area_historical_ha_1996)*100,
         perc_historical_2070 = (extent_change_ha_2023_2070/area_historical_ha_1996)*100)

write.csv(mang.proj2, 'outputs/extent-change-stats_mangrove.csv', row.names = F)

# seagrass

seag.proj2 <- seag.proj %>% 
  pivot_wider(id_cols = c(scenario, site, year), 
              names_from = 'state', values_from = 'area_ha') %>% 
  filter(year %in% c(2023, 2030, 2050, 2070)) %>% 
  pivot_wider(id_cols = c(scenario, site), names_from = 'year', values_from = 'M') %>% 
  rename(area_2023_ha = `2023`, area_2030_ha = `2030`, area_2050_ha = `2050`, area_2070_ha = `2070`) %>% 
  group_by(scenario) %>% 
  summarise_at(vars(area_2023_ha:area_2070_ha), sum) %>% 
  mutate(extent_change_ha_2023_2030 = area_2030_ha-area_2023_ha,
         extent_change_ha_2023_2050 = area_2050_ha-area_2023_ha, 
         extent_change_ha_2023_2070 = area_2070_ha-area_2023_ha,
         area_historical_ha = sum(seag$max_area)) %>% 
  mutate(perc_change_2023_2030 = ((area_2030_ha-area_2023_ha)/area_2023_ha)*100,
         perc_change_2023_2050 = ((area_2050_ha-area_2023_ha)/area_2023_ha)*100,
         perc_historical_2030 = (extent_change_ha_2023_2030/area_historical_ha)*100,
         perc_historical_2050 = (extent_change_ha_2023_2050/area_historical_ha)*100,
         perc_historical_2070 = (extent_change_ha_2023_2070/area_historical_ha)*100)

write.csv(seag.proj2, 'outputs/extent-change-stats_seagrass.csv', row.names = F)

# barplots extent change (km2) by scenario with percent change in text on bar

# mangrove plot

# wrangle for plotting

extent.change.m <- mang.proj2 %>% 
  pivot_longer(cols = c("extent_change_ha_2023_2030", 
                        "extent_change_ha_2023_2050",
                        "extent_change_ha_2023_2070"), names_to = 'year', values_to = 'change')
percent.change.m <- mang.proj2 %>% 
  pivot_longer(cols = c("perc_historical_2030", 
                        "perc_historical_2050",
                        "perc_historical_2070"), names_to = 'year', values_to = 'perc.change')

extent.change.m <- cbind(extent.change.m, percent.change.m[,ncol(percent.change.m)])

extent.change.m$group <- recode(extent.change.m$scenario, 
                  BAU = 'Counterfactual', P = 'Protect (I)', P2 = 'Protect (A)',
                  R = 'Restore (I)', R2 = 'Restore (A)', PR = 'Protect & Restore (I)', PR2 = 'Protect & Restore (A)')
extent.change.m$group <- factor(extent.change.m$group, 
                                levels = c('Counterfactual', 'Protect (I)','Protect (A)', 'Restore (I)', 
                                           'Restore (A)', 'Protect & Restore (I)', 'Protect & Restore (A)'))
extent.change.m$pos <- as.integer(extent.change.m$group)
extent.change.m$year <- recode(extent.change.m$year,
                               extent_change_ha_2023_2030 = '2030', extent_change_ha_2023_2050 = '2050',
                               extent_change_ha_2023_2070 = '2070')
options(scipen=10000)

# plot

p <- ggplot(extent.change.m)+
  geom_rect(aes(xmin = pos - .5, xmax = pos + .5, ymin = -Inf, ymax = Inf, fill = extent.change.m$group), alpha = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = c('white', 'lightcyan2', 'lightcyan2','lightcyan4','lightcyan4', 'lightcyan3', 'lightcyan3')) +
  new_scale_fill() +
  geom_bar(aes(x = as.factor(pos), y = change/100, fill = year),position = 'dodge', stat = 'identity') +
  geom_text(aes(label = paste0(round(perc.change, digits = 1), '%'), y = change/100, x = pos, group = year),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 1.8, col = 'black') +
  ylab('Extent change (km2)') +
  ggtitle('A) Mangrove') +
  xlab('') +
  ylim(c(min(extent.change.m$change)/100, max(extent.change.m$change)/100)) + 
  scale_x_discrete(labels=c('1' = '0', '2' = "P(I)", '3' = 'P(A)', '4' = "R(I)",
                            '5' = "R(A)", '6' = 'PR(I)', '7' = 'PR(A)')) +
  scale_fill_manual(values = c( 'darkgrey', 'lightgoldenrod2', 'cyan4'), 
                    labels = c('2030', '2050', '2070')) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = 'none',
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.margin=margin(0, 0, 0, -10))

p

# seagrass plot 

extent.change.s <- seag.proj2 %>% 
  pivot_longer(cols = c("extent_change_ha_2023_2030", 
                        "extent_change_ha_2023_2050",
                        "extent_change_ha_2023_2070"), names_to = 'year', values_to = 'change')
percent.change.s <- seag.proj2 %>% 
  pivot_longer(cols = c("perc_historical_2030", 
                        "perc_historical_2050",
                        "perc_historical_2070"), names_to = 'year', values_to = 'perc.change')

extent.change.s <- cbind(extent.change.s, percent.change.s[,ncol(percent.change.m)])

extent.change.s$group <- recode(extent.change.s$scenario, 
                  BAU = 'Counterfactual', P = 'Protect (I)', P2 = 'Protect (A)',
                  R = 'Restore (I)', R2 = 'Restore (A)', PR = 'Protect & Restore (I)', PR2 = 'Protect & Restore (A)')
extent.change.s$group <- factor(extent.change.s$group, levels = c('Counterfactual', 'Protect (I)', 'Protect (A)',
                                                    'Restore (I)', 'Restore (A)', 'Protect & Restore (I)',
                                                    'Protect & Restore (A)'))
extent.change.s$year <- recode(extent.change.s$year,
                               extent_change_ha_2023_2030 = '2030',
                               extent_change_ha_2023_2050 = '2050',
                               extent_change_ha_2023_2100 = '2070')
extent.change.s$pos <- as.integer(extent.change.s$group)
options(scipen=10000)

p2 <- ggplot(extent.change.s)+
  geom_rect(aes(xmin = pos - .5, xmax = pos + .5, ymin = -Inf, ymax = Inf, fill = extent.change.s$group), alpha = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = c('white', 'lightcyan2', 'lightcyan2','lightcyan4','lightcyan4', 'lightcyan3', 'lightcyan3')) +
  new_scale_fill() +
  geom_bar(aes(x = as.factor(pos), y = change/100, fill = year), position = 'dodge', stat = 'identity') +
  geom_text(aes(label = paste0(round(perc.change, digits = 1), '%'), y = change/100, x = pos, group = year),
            position = position_dodge(width = 0.9), vjust = -0.4, size = 1.8, col = 'black') +
  ylab('') +
  ylab('Extent change (km2)') +
  ggtitle('B) Seagrass') +
  xlab('') +
  ylim(c(min(extent.change.s$change)/100, max(extent.change.s$change)/100)) + 
  scale_x_discrete(labels=c('1' = '0', '2' = "P(I)", '3' = 'P(A)', '4' = "R(I)",
                            '5' = "R(A)", '6' = 'PR(I)', '7' = 'PR(A)')) +
  scale_fill_manual(values = c('darkgrey', 'lightgoldenrod2', 'cyan4'), labels = c('2030', '2050', '2070')) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        legend.direction = "horizontal", legend.position = "bottom",
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.margin=margin(-20, 0, 0, 0))

p2

p3 <- p / p2
p3

ggsave('outputs/extent-change.png', width = 5.5, height = 5)

# for each country, does protect and restore always have greatest gains?

mang.proj2 <- mang.proj %>% 
  pivot_wider(id_cols = c(scenario, country, year), 
              names_from = 'state', values_from = 'area_ha') %>% 
  filter(year %in% c(2023, 2030, 2050, 2070)) %>% 
  mutate(area = PF+UF) %>% 
  pivot_wider(id_cols = c(scenario, country), names_from = 'year', values_from = 'area') %>% 
  rename(area_2023_ha = `2023`, area_2030_ha = `2030`, area_2050_ha = `2050`, area_2070_ha = `2070`)
  
name <- as.character(unique(mang.proj2$country))
df <- data.frame(site = NA, PRlessR = NA, PRlessP = NA)

for(i in 1:length(name)){
  
  nam <- name[i]
  d <- filter(mang.proj2, country == nam)
  
  df[i,1] <- nam
  df[i,2] <- filter(d, scenario == 'PR2')$area_2070_ha - filter(d, scenario == 'R2')$area_2070_ha
  df[i,3] <- filter(d, scenario == 'PR2')$area_2070_ha - filter(d, scenario == 'P2')$area_2070_ha
  
}

filter(df, PRlessR <0 | PRlessP <0)

# for seagrass is combined protect and restore always better than just protect or restore?

seag.proj2 <- seag.proj %>% 
  pivot_wider(id_cols = c(scenario, site, year), 
              names_from = 'state', values_from = 'area_ha') %>% 
  filter(year %in% c(2023, 2030, 2050, 2070)) %>% 
  pivot_wider(id_cols = c(scenario, site), names_from = 'year', values_from = 'M') %>% 
  rename(area_2023_ha = `2023`, area_2030_ha = `2030`, area_2050_ha = `2050`, area_2070_ha = `2070`)
  
name <- as.character(unique(seag.proj2$site))
df <- data.frame(site = NA, PRlessR = NA, PRlessP = NA)

for(i in 1:length(name)){
  
  nam <- name[i]
  d <- filter(seag.proj2, site == nam)
  
  df[i,1] <- nam
  df[i,2] <- filter(d, scenario == 'PR2')$area_2070_ha - filter(d, scenario == 'R2')$area_2070_ha
  df[i,3] <- filter(d, scenario == 'PR2')$area_2070_ha - filter(d, scenario == 'P2')$area_2070_ha
  
}

filter(df, PRlessR <0 | PRlessP <0)

# find country with highest projected decline in extent under BAU by 2070

mang.proj2 %>% 
  mutate(extent_change = area_2070_ha-area_2023_ha) %>% 
  filter(scenario == 'BAU' & extent_change < 0 & extent_change == min(extent_change))

mang.proj2 %>% 
  mutate(extent_change = area_2070_ha-area_2023_ha) %>% 
  filter(country == 'Indonesia')

seag.proj2 %>% 
  rename(study_site = site) %>% 
  left_join(select(data.frame(seag.sf), SOVEREIGN1, study_site)) %>% 
  mutate(extent_change = area_2070_ha-area_2023_ha) %>% 
  group_by(SOVEREIGN1, scenario) %>% 
  summarise(extent_change = sum(extent_change)) %>% 
  ungroup() %>% 
  filter(scenario == 'BAU' & extent_change < 0 & extent_change == min(extent_change))

seag.proj2 %>% 
  rename(study_site = site) %>% 
  left_join(select(data.frame(seag.sf), SOVEREIGN1, study_site)) %>% 
  mutate(extent_change = area_2070_ha-area_2023_ha) %>% 
  group_by(SOVEREIGN1, scenario) %>% 
  summarise(extent_change = sum(extent_change)) %>% 
  ungroup() %>% 
  filter(SOVEREIGN1 == 'United States')


