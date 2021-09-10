# plot habitat state trajectories through time

library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(sf)

# data

mang <- read.csv('outputs/mangrove-capped.csv') %>% 
  arrange(Country)
seag <- read.csv('outputs/seagrass-capped.csv')
landEEZ <- st_read('data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')
seag.sf <- seag %>% 
  st_as_sf(coords = c('lat', 'long'), crs = '+proj=moll') %>% 
  st_transform(crs = 4326) %>% 
  st_join(st_make_valid(landEEZ))

# projections 

seag.proj <- read.csv('outputs/seagrass-projections.csv')
mang.proj <- read.csv('outputs/mangrove-projections.csv')

# plot global habitat state trajectories

# wrangle for plotting

mang.global <- mang.proj %>% 
  group_by(scenario, year, state) %>% 
  summarise(area_ha = sum(area_ha, na.rm = T)) %>% 
  pivot_wider(names_from = state, values_from = area_ha) %>% 
  mutate(state_total = PD+PF+UD+UF+Unrest) %>% 
  pivot_longer(cols = PD:Unrest, names_to = 'state', values_to = 'area_ha') %>% 
  mutate(prop = area_ha/state_total) %>% 
  droplevels()

seag.global <- seag.proj %>% 
  group_by(scenario, year, state) %>% 
  summarise(area_ha = sum(area_ha, na.rm = T)) %>% 
  pivot_wider(names_from = state, values_from = area_ha) %>% 
  mutate(state_total = M + LM) %>% 
  pivot_longer(cols = M:LM, names_to = 'state', values_to = 'area_ha') %>% 
  mutate(prop = area_ha/state_total) %>% 
  droplevels()

labels <- c(BAU = 'Counterfactual', P = 'Protect (I)', P2 = 'Protect (A)', R = 'Restore (I)',
            R2 = 'Restore (A)', PR = 'Protect & Restore (I)', PR2 = 'Protect & Restore (A)')

# plot mangrove trajectories

a <-  ggplot() +
  geom_ribbon(data = filter(mang.global, scenario %in% c('BAU', 'PR2')) %>% 
                select(scenario, year, state, prop) %>% # prop Unprotected Forested
                pivot_wider(names_from = state, values_from = prop), 
              aes(x = as.integer(year), ymin=UD+PD+PF+UF, ymax=UF+PF+UD+PD+Unrest, fill= "brown3")) +
  geom_ribbon(data = filter(mang.global, scenario %in% c('BAU', 'PR2')) %>% 
                select(scenario, year, state, prop) %>% # prop Protected forest
                pivot_wider(names_from = state, values_from = prop), 
              aes(x = as.integer(year), ymin=PD+PF+UF, ymax=UD+PD+PF+UF, fill= "cadetblue2")) +
  geom_ribbon(data = filter(mang.global, scenario %in% c('BAU', 'PR2')) %>%
                select(scenario, year, state, prop) %>% # prop protected deforested
                pivot_wider(names_from = state, values_from = prop), 
              aes(x = as.integer(year), ymin=PF+UF,ymax=PD+PF+UF, fill="darkcyan")) +
  geom_ribbon(data = filter(mang.global, scenario %in% c('BAU', 'PR2')) %>%
                select(scenario, year, state, prop) %>% # prop unprodected deforested
                pivot_wider(names_from = state, values_from = prop), 
              aes(x = as.integer(year), ymin=UF,ymax=PF+UF, fill= "lightgoldenrod2")) +
  geom_ribbon(data = filter(mang.global, scenario %in% c('BAU', 'PR2')) %>%
                select(scenario, year, state, prop) %>% # prop unrestorable
                pivot_wider(names_from = state, values_from = prop), 
              aes(x = as.integer(year), ymin=0,ymax=UF, fill= "seagreen")) +
  scale_fill_identity(guide="legend", labels = c('Unrest', '(Ud) deforested', '(Pd) deforested',
                                                 '(Pf) forest','(Uf) forest'), name = '') +
  ylab('Proportion') +
  xlab('') +
  theme_classic() +
  ggtitle('A) Mangrove') +
  geom_vline(xintercept = 2050, linetype = 'longdash', alpha = 0.5) +
  geom_vline(xintercept = 2030, linetype = 'longdash', alpha = 0.5) +
  xlim(c(2023, 2070)) +
  facet_wrap(~scenario, scales = 'free', ncol = 2, labeller=labeller(scenario = labels)) +
  theme_classic() +
  theme(legend.title = element_blank(), 
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10),
        legend.direction = "horizontal", legend.position = "bottom",
        legend.text = element_text(size = 7.5),
        legend.margin=margin(-20, 0, 0, 0))

a

# plot seagrass trajectories

b <- ggplot() +
  geom_ribbon(data = seag.global %>%
                filter(scenario %in% c('BAU', 'PR2')) %>% 
                select(scenario, year, state, prop) %>% # prop unprodected deforested
                pivot_wider(names_from = state, values_from = prop), 
              aes(x = as.integer(year), ymin=M,ymax=LM+M, fill="cadetblue2")) +
  geom_ribbon(data = seag.global %>%
                filter(scenario %in% c('BAU', 'PR2')) %>% 
                select(scenario, year, state, prop) %>% # prop unrestorable
                pivot_wider(names_from = state, values_from = prop), 
              aes(x = as.integer(year), ymin=0,ymax=M, fill="seagreen")) +
  scale_fill_identity(guide = 'legend',labels = c('Lost Meadow', 'Meadow'), name = '') +
  ylab('Proportion') +
  xlab('') +
  xlim(c(2023,2070)) +
  theme_classic() +
  geom_vline(xintercept = 2050, linetype = 'longdash', alpha = 0.5) +
  geom_vline(xintercept = 2030, linetype = 'longdash', alpha = 0.5) +
  facet_wrap(~scenario, scales = 'free', ncol = 2, labeller=labeller(scenario = labels)) +   
  ggtitle('B) Seagrass') +
  theme_classic() +
  theme(legend.title = element_blank(), 
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 10),
        legend.direction = "horizontal", legend.position = "bottom",
        legend.text = element_text(size = 7.5),
        legend.margin=margin(-20, 0, 0, 0))
b

c <- a / b
c

ggsave('outputs/global-state-proportion-projections.png', width = 5, height = 5.7)

# plot country habitat state trajectories

# mangroves

prop.df <- mang.proj %>%
  pivot_wider(names_from = state, values_from = area_ha) %>% 
  mutate(state_total = PD+PF+UD+UF+Unrest) %>% 
  pivot_longer(cols = PF:Unrest, names_to = 'state', values_to = 'area_ha') %>% 
  mutate(prop = area_ha/state_total) %>% 
  droplevels()

names <- as.character(levels(prop.df$country))

prop.df$scenario <- factor(prop.df$scenario, levels = c('BAU', 'P', 'P2', 'R', 'R2', 'PR', 'PR2'))

for(i in 1:length(names)){
  
  name <- names[i]
  
  prop.df2 <- prop.df %>% 
    filter(country == name) %>%  # select a country
    filter(scenario %in% c('BAU', 'P2', 'R2', 'PR2'))
  
  a <-  ggplot() +
    geom_ribbon(data = prop.df2 %>% 
                  select(scenario, year, state, prop) %>% # prop Unprotected Forested
                  pivot_wider(names_from = state, values_from = prop),
                aes(x = as.integer(year), ymin=UD+PD+PF+UF, ymax=UF+PF+UD+PD+Unrest, fill= "brown3")) +
    geom_ribbon(data = prop.df2 %>% 
                  select(scenario, year, state, prop) %>% # prop Unprotected Forested
                  pivot_wider(names_from = state, values_from = prop),
                aes(x = as.integer(year), ymin=PD+PF+UF, ymax=UD+PD+PF+UF, fill= "cadetblue2")) +
    geom_ribbon(data = prop.df2 %>% 
                  select(scenario, year, state, prop) %>% # prop Unprotected Forested
                  pivot_wider(names_from = state, values_from = prop),
                aes(x = as.integer(year), ymin=PF+UF,ymax=PD+PF+UF, fill="darkcyan")) +
    geom_ribbon(data = prop.df2 %>% 
                  select(scenario, year, state, prop) %>% # prop Unprotected Forested
                  pivot_wider(names_from = state, values_from = prop),
                aes(x = as.integer(year), ymin=UF,ymax=PF+UF, fill= "lightgoldenrod2")) +
    geom_ribbon(data = prop.df2 %>% 
                  select(scenario, year, state, prop) %>% # prop Unprotected Forested
                  pivot_wider(names_from = state, values_from = prop),
                aes(x = as.integer(year), ymin=0,ymax=UF, fill= "seagreen")) +
    scale_fill_identity(
      guide="legend", 
      labels = c('Unrest', '(Ud) deforested', '(Pd) deforested','(Pf) forest','(Uf) forest'),
      name = '') +
    ylab('Proportion') +
    xlab('') +
    xlim(c(2016,2070)) +
    theme_classic() +
    geom_vline(xintercept = 2023, linetype = 'longdash', alpha = 0.5) +
    geom_vline(xintercept = 2030, linetype = 'longdash', alpha = 0.5) +
    geom_vline(xintercept = 2050, linetype = 'longdash', alpha = 0.5) +
    facet_wrap(~scenario, scales = 'free', ncol = 2, labeller=labeller(scenario = labels)) +
    ggtitle(paste0(name)) +
    theme_classic() +
    theme(legend.title = element_blank(), 
          plot.title = element_text(size = 10),
          axis.text.x = element_text(size = 5),
          legend.direction = "horizontal", legend.position = "bottom",
          legend.text = element_text(size = 7.5),
          legend.margin=margin(-20, 0, 0, 0))
  a
  ggsave(paste0('outputs/mang-country-trajectories/mangrove-state-prop_', name, '.png'), width = 5.5, height = 5.5)
}

# seagrass

seag.df <- data.frame(seag.sf) %>% 
  rename(site = study_site,
        country = SOVEREIGN1)

prop.df <- seag.proj %>% 
  left_join(select(seag.df, country, site), by = 'site') %>%  # select a country
  filter(scenario %in% c('BAU', 'P', 'R2', 'PR2')) %>% 
  droplevels() %>% 
  group_by(scenario, country, year, state) %>% 
  summarise(area_ha = sum(area_ha, na.rm = T)) %>%
  pivot_wider(names_from = state, values_from = area_ha) %>% 
  mutate(state_total = M + LM) %>% 
  pivot_longer(cols = M:LM, names_to = 'state', values_to = 'area_ha') %>% 
  mutate(prop = area_ha/state_total)

names <- as.character(levels(prop.df$country))

prop.df$scenario <- factor(prop.df$scenario, levels = c('BAU', 'P', 'P2', 'R', 'R2', 'PR', 'PR2'))

# plot state proportions

for(i in 1:length(names)){
  
  name <- names[i]
  
  prop.df2 <- prop.df %>%
    filter(country == name)
  
  d <- ggplot() +
    geom_ribbon(data = prop.df2 %>%
                  select(scenario, year, state, prop) %>% # prop unprodected deforested
                  pivot_wider(names_from = state, values_from = prop), 
                aes(x = as.integer(year), ymin=M,ymax=LM+M, fill="cadetblue2")) +
    geom_ribbon(data = prop.df2 %>%
                  select(scenario, year, state, prop) %>% # prop unrestorable
                  pivot_wider(names_from = state, values_from = prop), 
                aes(x = as.integer(year), ymin=0,ymax=M, fill="seagreen")) +
    scale_fill_identity(guide = 'legend',labels = c('Lost Meadow', 'Meadow'), name = '') +
    ylab('Proportion') +
    xlab('') +
    xlim(c(2010,2070)) +
    theme_classic() +
    geom_vline(xintercept = 2023, linetype = 'longdash', alpha = 0.5) +
    geom_vline(xintercept = 2030, linetype = 'longdash', alpha = 0.5) +
    geom_vline(xintercept = 2050, linetype = 'longdash', alpha = 0.5) +
    facet_wrap(~scenario, scales = 'free', ncol = 2, labeller=labeller(scenario = labels)) +   
    ggtitle(paste0(name)) +
    theme_classic() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 10),
          legend.direction = "horizontal", legend.position = "bottom",
          legend.text = element_text(size = 10),
          legend.margin=margin(-20, 0, 0, 0))
  d
  
  ggsave(paste0('outputs/seag-country-trajectories/seagrass-state-prop_', name, '.png'), width = 5.5, height = 5.5)
}  
