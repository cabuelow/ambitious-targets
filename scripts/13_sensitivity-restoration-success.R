# test the sensitivity of extent change to variable restoration success

library(tidyverse)
library(patchwork)
source('scripts/helper_functions/matrix-model-mangrove-sens4.R')
source('scripts/helper_functions/matrix-model-seagrass-sens3.R')

# data

mang <- read.csv('outputs/mangrove-capped.csv') %>% arrange(Country)
seag <- read.csv('outputs/seagrass-capped.csv')
seag.proj <- read.csv('outputs/seagrass-projections.csv')
mang.proj <- read.csv('outputs/mangrove-projections.csv')

# vary restoration success

# mangroves

rates <- c(0.1, 0.2, 0.3)
biglist <- list()

for(j in seq_along(rates)){
  
  r <- rates[j]
    
    ls <- list()
    ls2 <- list()
    ls3 <- list()
    
    for(i in 1:nrow(mang)){
      
      sub <- mang[i,] # select a country
      
      mangrove_project5(country = sub$Country, PF = sub$Protected_forest_ha, UF = sub$Unprotected_forest_ha, 
                        PD = sub$Protected_deforested_ha, UD = sub$Unprotected_deforested_ha,
                        ewe = sub$ewe_rate_10_16, erosion = sub$erosion_rate_10_16, commodities = sub$commodities_rate_10_16, 
                        settle = sub$settle_rate_10_16, npc = sub$npc_rate_10_16, growth = sub$Growth_rate_GMW, # loss and growth rates
                        initial.year = 2016, cons.start = 2023, target.year.protect = 2030, 
                        target.year.restore = 2050, project.year = 2050, rest.fail = r,  # conservation scenario start year and target year
                        PR.target = 0.3, RR.target = 0) # set protection and restoration target
      
      ls[[i]] <- df.full
      ls2[[i]] <- prot.rate
      ls3[[i]] <- rest.rate
    }
    
    P.m <- do.call(rbind, ls)
    P.m$scenario <- rep(paste0('P_', r), nrow(P.m))
    Pratep.m <- do.call(rbind, ls2)
    Pratep.m$scenario <- rep(paste0('P_', r), nrow(Pratep.m))
    
    # restoration only 
    
    # intermediate
    ls <- list()
    ls2 <- list()
    ls3 <- list()
    
    for(i in 1:nrow(mang)){
      
      sub <- mang[i,] # select a country
      
      mangrove_project5(country = sub$Country, PF = sub$Protected_forest_ha, UF = sub$Unprotected_forest_ha, 
                        PD = sub$Protected_deforested_ha, UD = sub$Unprotected_deforested_ha,
                        ewe = sub$ewe_rate_10_16, erosion = sub$erosion_rate_10_16, commodities = sub$commodities_rate_10_16, 
                        settle = sub$settle_rate_10_16, npc = sub$npc_rate_10_16, growth = sub$Growth_rate_GMW, # loss and growth rates
                        initial.year = 2016, cons.start = 2023, target.year.protect = 2030,  rest.fail = r,
                        target.year.restore = 2050, project.year = 2050,  # conservation scenario start year and target year
                        PR.target = 0, RR.target = 0.5) # set protection and restoration target
      
      ls[[i]] <- df.full
      ls2[[i]] <- prot.rate
      ls3[[i]] <- rest.rate
    }
    
    R.m <- do.call(rbind, ls)
    R.m$scenario <- rep(paste0('R_', r), nrow(R.m))
    Rrater.m <- do.call(rbind, ls3)
    Rrater.m$scenario <- rep(paste0('R_', r), nrow(Rrater.m))
    
    mang.sens1 <- rbind(P.m, R.m)

  biglist[[j]] <- mang.sens1
}

df <- do.call(rbind, biglist)

# seagrass, vary restoration success

rates <- c(0.1, 0.2, 0.3)
biglist <- list()

for(j in seq_along(rates)){
  
  r <- rates[j]
  
  ls <- list()
  
  seagrass_protect3(seag, PR.target = 0.3, target.year.protect = 2030, cons.start = 2023)
  
  for(i in 1:nrow(seag)){
    
    sub <- x[i,] # select a meadow
    
    seagrass_project4(site = sub$study_site, M = sub$area_start, LM = sub$restore_area, Unrest = 0, # initial states
                      loss = sub$loss, growth = sub$growth, # loss and growth rates
                      cons.start = 2023, initial.year = 2000, target.year.protect = 2030, 
                      target.year.restore = 2050, project.year = 2050, # conservation start year and target year
                      RR.target = 0, PR.target = 0.3, rest.fail = r, 
                      protect = sub$protect, protect.year = sub$protect.year) # restoration target 
    ls[[i]] <- df.full
  }
  
  
  P.s <- do.call(rbind, ls)
  P.s$scenario <- rep(paste0('P_', r), nrow(P.s))
  
  # restoration only 
  
  seagrass_protect3(seag, PR.target = 0.3, target.year.protect = 2030, cons.start = 2023)
  
  ls <- list()
  ls2 <- list()
  
  for(i in 1:nrow(seag)){
    
    sub <- x[i,] # select a meadow
    
    seagrass_project4(site = sub$study_site, M = sub$area_start, LM = sub$restore_area, Unrest = 0, # initial states
                      loss = sub$loss, growth = sub$growth, # loss and growth rates
                      cons.start = 2023, initial.year = 2000, target.year.protect = 2030, 
                      target.year.restore = 2050, project.year = 2050, rest.fail = r, # conservation start year and target year
                      RR.target = 0.5, PR.target = 0, protect.year = sub$protect.year) # restoration target 
    ls[[i]] <- df.full
    ls2[[i]] <- rest.rate
  }
  
  R.s <- do.call(rbind, ls)
  R.s$scenario <- rep(paste0('R_', r), nrow(R.s))
  
  seag.sens1 <- rbind(P.s, R.s)
  biglist[[j]] <- seag.sens1
}

df.s <- do.call(rbind, biglist)

# tragectories of global extent under different scenarios

# mangroves

mang.proj.P <- mang.proj %>% 
  filter(scenario == 'P' & year %in% c(2023:2050) & state %in% c('PF', 'UF')) %>% 
  group_by(year, scenario) %>% 
  summarise(area_ha = sum(area_ha)/100)

mang.proj.R <- mang.proj %>% 
  filter(scenario == 'R' & year %in% c(2023:2050) & state %in% c('PF', 'UF')) %>% 
  group_by(year, scenario) %>% 
  summarise(area_ha = sum(area_ha)/100)

prot <- str_subset(unique(df$scenario), 'P')
rest <- str_subset(unique(df$scenario), 'R')
df$action <- str_sub(df$scenario, 1, 1)
df$percent <- as.character(as.numeric(str_sub(df$scenario, 3, 5))*100)

dfsub.P <- df %>% 
  filter(scenario %in% prot, year %in% c(2023:2050) & state %in% c('PF', 'UF')) %>% 
  group_by(year, action, percent, scenario) %>% 
  summarise(area_ha = sum(area_ha)/100)

dfsub.R <- df %>% 
  filter(scenario %in% rest, year %in% c(2023:2050) & state %in% c('PF', 'UF')) %>% 
  group_by(year, action, percent, scenario) %>% 
  summarise(area_ha = sum(area_ha)/100)

r1 <- max(dfsub.P$area_ha) - min(dfsub.P$area_ha)
r2 <- max(dfsub.R$area_ha) - min(dfsub.R$area_ha)

if(r1 < r2){
  buffP <- (r2-r1)/2
  buffR <- 0
}else{
  buffR <- (r1-r2)/2
  buffP <- 0
}

a <- ggplot() +
  geom_line(data = dfsub.P, aes(x = year, y = area_ha, col = percent)) +
  geom_line(data = mang.proj.P, aes(x = year, y = area_ha)) +
  ggtitle('A) Mangrove protection') +
  ylab('Extent (km2)') +
  xlab('Year') +
  ylim(c(min(dfsub.P$area_ha)-buffP, max(dfsub.P$area_ha)+buffP)) +
  theme_classic() +
  theme(legend.position = 'none',
        plot.title = element_text(size = 10))
a

b <- ggplot() +
  geom_line(data = dfsub.R, aes(x = year, y = area_ha, col = percent)) +
  geom_line(data = mang.proj.R, aes(x = year, y = area_ha)) +
  ggtitle('B) Mangrove restoration') +
  ylab('Extent (km2)') +
  xlab('Year') +
  ylim(c(min(dfsub.R$area_ha)-buffR, max(dfsub.R$area_ha)+buffR)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10))
b

# seagrass

seag.proj.P <- seag.proj %>% 
  filter(scenario == 'P' & year %in% c(2023:2050) & state == 'M') %>% 
  group_by(year, scenario) %>% 
  summarise(area_ha = sum(area_ha)/100)

seag.proj.R <- seag.proj %>% 
  filter(scenario == 'R' & year %in% c(2023:2050) & state == 'M') %>% 
  group_by(year, scenario) %>% 
  summarise(area_ha = sum(area_ha)/100)

prot <- str_subset(unique(df.s$scenario), 'P')
rest <- str_subset(unique(df.s$scenario), 'R')
df.s$action <- str_sub(df.s$scenario, 1, 1)
df.s$percent <- as.character(as.numeric(str_sub(df.s$scenario, 3, 5))*100)

dfsub.P <- df.s %>% 
  filter(scenario %in% prot, year %in% c(2023:2050) & state == 'M') %>% 
  group_by(year, action, percent, scenario) %>% 
  summarise(area_ha = sum(area_ha)/100)

dfsub.R <- df.s %>% 
  filter(scenario %in% rest, year %in% c(2023:2050) & state == 'M') %>% 
  group_by(year, action, percent, scenario) %>% 
  summarise(area_ha = sum(area_ha)/100)

r1 <- max(dfsub.P$area_ha) - min(dfsub.P$area_ha)
r2 <- max(dfsub.R$area_ha) - min(dfsub.R$area_ha)

if(r1 < r2){
  buffP <- (r2-r1)/2
  buffR <- 0
}else{
  buffR <- (r1-r2)/2
  buffP <- 0
}

c <- ggplot() +
  geom_line(data = dfsub.P, aes(x = year, y = area_ha, col = percent)) +
  geom_line(data = seag.proj.P, aes(x = year, y = area_ha)) +
  ggtitle('C) Seagrass protection') +
  ylab('Extent (km2)') +
  xlab('Year') +
  ylim(c(min(dfsub.P$area_ha)-buffP, max(dfsub.P$area_ha)+buffP)) +
  theme_classic() +
  theme(legend.position = 'none',
        plot.title = element_text(size = 10))
c

d <- ggplot() +
  geom_line(data = dfsub.R, aes(x = year, y = area_ha, col = percent)) +
  geom_line(data = seag.proj.R, aes(x = year, y = area_ha)) +
  ggtitle('D) Seagrass restoration') +
  ylab('Extent (km2)') +
  xlab('Year') +
  ylim(c(min(dfsub.R$area_ha)-buffR, max(dfsub.R$area_ha)+buffR)) +
  theme_classic() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10))
d

f <- (a+b)/(c+d)
f

ggsave('outputs/sensitivity_restoration-success_global-trajectory.png', width = 7, height = 5)

