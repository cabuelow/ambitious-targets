# sensitivity of global extent change of mangroves and seagrass under the intermediate protect and restore scenario
# to percent increases or decreases (10, 20, or 30%) in the initial condition of restorable area

library(tidyverse)
library(patchwork)
source('scripts/helper_functions/matrix-model-mangrove.R')
source('scripts/helper_functions/matrix-model-seagrass.R')

# data

mang <- read.csv('outputs/mangrove-capped.csv') %>% arrange(Country)
seag <- read.csv('outputs/seagrass-capped.csv')
seag.proj <- read.csv('outputs/seagrass-projections.csv')
mang.proj <- read.csv('outputs/mangrove-projections.csv')

# test sensitivity to increase or decrease in rates of expansion by 10, 20, 30%

# mangroves - intermediate scenario

rates <- c(0.1, 0.2, 0.3)
biglist <- list()

for(j in seq_along(rates)){
  
  r <- rates[j]
  
  multm <- function(x)(x - x*r)
  multp <- function(x)(x + x*r)
  
  datm <- mang %>% 
    mutate_at(c('Unprotected_deforested_ha', 'Protected_deforested_ha'), multm)
  
  datp <- mang %>% 
    mutate_at(c('Unprotected_deforested_ha', 'Protected_deforested_ha'), multp)
  
  dall <- list(datm, datp)
  
  smalls <- list()
  
  for(k in seq_along(dall)){
    
    d <- dall[[k]]
    
    # increase 
    
    ls <- list()
    ls2 <- list()
    ls3 <- list()
    
    for(i in 1:nrow(d)){
      
      sub <- d[i,] # select a country
      
      mangrove_project(country = sub$Country, PF = sub$Protected_forest_ha, UF = sub$Unprotected_forest_ha, 
                       PD = sub$Protected_deforested_ha, UD = sub$Unprotected_deforested_ha,
                       ewe = sub$ewe_rate_10_16, erosion = sub$erosion_rate_10_16, commodities = sub$commodities_rate_10_16, 
                       settle = sub$settle_rate_10_16, npc = sub$npc_rate_10_16, growth = sub$Growth_rate_GMW, # loss and growth rates
                       initial.year = 2016, cons.start = 2023, target.year.protect = 2030, 
                       target.year.restore = 2050, project.year = 2050,  # conservation scenario start year and target year
                       PR.target = 0.3, RR.target = 0) # set protection and restoration target
      
      ls[[i]] <- df.full
      ls2[[i]] <- prot.rate
      ls3[[i]] <- rest.rate
    }
    
    if(k == 1){
      name <- 'dec'
    }else{name <- 'inc'}
    
    P.m <- do.call(rbind, ls)
    P.m$scenario <- rep(paste0('P_', r,'_',name), nrow(P.m))
    Pratep.m <- do.call(rbind, ls2)
    Pratep.m$scenario <- rep(paste0('P_', r,'_',name), nrow(Pratep.m))
    
    # restoration only 
    
    # intermediate
    ls <- list()
    ls2 <- list()
    ls3 <- list()
    
    for(i in 1:nrow(d)){
      
      sub <- d[i,] # select a country
      
      mangrove_project(country = sub$Country, PF = sub$Protected_forest_ha, UF = sub$Unprotected_forest_ha, 
                       PD = sub$Protected_deforested_ha, UD = sub$Unprotected_deforested_ha,
                       ewe = sub$ewe_rate_10_16, erosion = sub$erosion_rate_10_16, commodities = sub$commodities_rate_10_16, 
                       settle = sub$settle_rate_10_16, npc = sub$npc_rate_10_16, growth = sub$Growth_rate_GMW, # loss and growth rates
                       initial.year = 2016, cons.start = 2023, target.year.protect = 2030, 
                       target.year.restore = 2050, project.year = 2050,  # conservation scenario start year and target year
                       PR.target = 0, RR.target = 0.5) # set protection and restoration target
      
      ls[[i]] <- df.full
      ls2[[i]] <- prot.rate
      ls3[[i]] <- rest.rate
    }
    
    if(k == 1){
      name <- 'dec'
    }else{name <- 'inc'}
    
    R.m <- do.call(rbind, ls)
    R.m$scenario <- rep(paste0('R_', r,'_',name), nrow(R.m))
    Rrater.m <- do.call(rbind, ls3)
    Rrater.m$scenario <- rep(paste0('R_', r,'_',name), nrow(Rrater.m))
    
    mang.sens1 <- rbind(P.m, R.m)
    smalls[[k]] <- mang.sens1
  }
  sfull <- do.call(rbind, smalls)
  biglist[[j]] <- sfull
}

df <- do.call(rbind, biglist)

# seagrass

rates <- c(0.1, 0.2, 0.3)
biglist <- list()

for(j in seq_along(rates)){
  
  r <- rates[j]
  
  multm <- function(x)(x - x*r)
  multp <- function(x)(x + x*r)
  
  datm <- seag %>% 
    mutate(restore_area = multm(restore_area))
  
  datp <- seag %>% 
    mutate(restore_area = multp(restore_area))
  
  dall <- list(datm, datp)
  
  smalls <- list()
  
  for(k in seq_along(dall)){
    
    d <- dall[[k]]
    
    ls <- list()
    
    seagrass_protect(d, PR.target = 0.3, target.year.protect = 2030, cons.start = 2023)
    
    for(i in 1:nrow(seag)){
      
      sub <- x[i,] # select a meadow
      
      seagrass_project(site = sub$study_site, M = sub$area_start, LM = sub$restore_area, # initial states
                       loss = sub$loss, growth = sub$growth, # loss and growth rates
                       cons.start = 2023, initial.year = 2000, target.year.protect = 2030, 
                       target.year.restore = 2050, project.year = 2050, # conservation start year and target year
                       RR.target = 0, PR.target = 0.3,
                       protect = sub$protect, protect.year = sub$protect.year) # restoration target 
      ls[[i]] <- df.full
    }
    
    
    if(k == 1){
      name <- 'dec'
    }else{name <- 'inc'}
    
    P.s <- do.call(rbind, ls)
    P.s$scenario <- rep(paste0('P_', r,'_',name), nrow(P.s))
    
    # restoration only 
    
    seagrass_protect(d, PR.target = 0.3, target.year.protect = 2030, cons.start = 2023)
    
    ls <- list()
    ls2 <- list()
    
    for(i in 1:nrow(seag)){
      
      sub <- x[i,] # select a meadow
      
      seagrass_project(site = sub$study_site, M = sub$area_start, LM = sub$restore_area, # initial states
                       loss = sub$loss, growth = sub$growth, # loss and growth rates
                       cons.start = 2023, initial.year = 2000, target.year.protect = 2030, 
                       target.year.restore = 2050, project.year = 2050, # conservation start year and target year
                       RR.target = 0.5, PR.target = 0, protect.year = sub$protect.year) # restoration target 
      ls[[i]] <- df.full
      ls2[[i]] <- rest.rate
    }
    
    if(k == 1){
      name <- 'dec'
    }else{name <- 'inc'}
    
    R.s <- do.call(rbind, ls)
    R.s$scenario <- rep(paste0('R_', r,'_',name), nrow(R.s))
    Rrater.s <- do.call(rbind, ls2)
    Rrater.s$scenario <- rep(paste0('R_', r,'_',name), nrow(Rrater.s))
    
    seag.sens1 <- rbind(P.s, R.s)
    smalls[[k]] <- seag.sens1
  }
  sfull <- do.call(rbind, smalls)
  biglist[[j]] <- sfull
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
df$direction <- str_sub(df$scenario, 7, 9)

dfsub.P <- df %>% 
  filter(scenario %in% prot, year %in% c(2023:2050) & state %in% c('PF', 'UF')) %>% 
  group_by(year, action, percent, direction, scenario) %>% 
  summarise(area_ha = sum(area_ha)/100)

dfsub.R <- df %>% 
  filter(scenario %in% rest, year %in% c(2023:2050) & state %in% c('PF', 'UF')) %>% 
  group_by(year, action, percent, direction, scenario) %>% 
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
  geom_line(data = filter(dfsub.P, direction == 'inc'), aes(x = year, y = area_ha, col = percent)) +
  geom_line(data = filter(dfsub.P, direction == 'dec'), aes(x = year, y = area_ha, col = percent)) +
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
  geom_line(data = filter(dfsub.R, direction == 'inc'), aes(x = year, y = area_ha, col = percent)) +
  geom_line(data = filter(dfsub.R, direction == 'dec'), aes(x = year, y = area_ha, col = percent)) +
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
df.s$direction <- str_sub(df.s$scenario, 7, 9)

dfsub.P <- df.s %>% 
  filter(scenario %in% prot, year %in% c(2023:2050) & state == 'M') %>% 
  group_by(year, action, percent, direction, scenario) %>% 
  summarise(area_ha = sum(area_ha)/100)

dfsub.R <- df.s %>% 
  filter(scenario %in% rest, year %in% c(2023:2050) & state == 'M') %>% 
  group_by(year, action, percent, direction, scenario) %>% 
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
  geom_line(data = seag.proj.P, aes(x = year, y = area_ha)) +
  geom_line(data = filter(dfsub.P, direction == 'inc'), aes(x = year, y = area_ha, col = percent)) +
  geom_line(data = filter(dfsub.P, direction == 'dec'), aes(x = year, y = area_ha, col = percent)) +
  ggtitle('C) Seagrass protection') +
  ylab('Extent (km2)') +
  xlab('Year') +
  ylim(c(min(dfsub.P$area_ha)-buffP, max(dfsub.P$area_ha)+buffP)) +
  theme_classic() +
  theme(legend.position = 'none',
        plot.title = element_text(size = 10))
c

d <- ggplot() +
  geom_line(data = filter(dfsub.R, direction == 'inc'), aes(x = year, y = area_ha, col = percent)) +
  geom_line(data = filter(dfsub.R, direction == 'dec'), aes(x = year, y = area_ha, col = percent)) +
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

ggsave('outputs/sensitivity_restorable-area_global-trajectory.png', width = 7, height = 5)


