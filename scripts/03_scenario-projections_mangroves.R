# project mangrove extent to 2070 under baseline (BAU), protection and restoration scenarios 

library(tidyverse)
library(sf)
source('scripts/helper_functions/matrix-model-mangrove.R')

# data

dat <- read.csv('outputs/mangrove-capped.csv')

# scenarios

pr <- c(0, 0.3, 0.5) # protection targets
rr <- c(0, 0.5, 0.9) # restoration targets

scenarios <- expand.grid(PR = pr, RR = rr)[-c(6,8),]
scenarios$scenario <- c('BAU', 'P', 'P2', 'R', 'PR', 'R2', 'PR2')

# loop through each scenario, run projections, and save results

projections <- list() # list for storing projections
prot.rates <- list() # list for storing protection rates
rest.rates <- list() # list for storing restoratoin rates

for(j in 1:nrow(scenarios)){
  
# loop through each country and run projections 

ls <- list() # list for storing results for each country
ls2 <- list() # list for storing protection rate
ls3 <- list() # list for storing restoration rate

for(i in 1:nrow(dat)){
  
  sub <- dat[i,] # select a country
  
  
  mangrove_project(country = sub$Country, PF = sub$Protected_forest_ha, UF = sub$Unprotected_forest_ha, 
                   PD = sub$Protected_deforested_ha, UD = sub$Unprotected_deforested_ha,
                   ewe = sub$ewe_rate_10_16, erosion = sub$erosion_rate_10_16, commodities = sub$commodities_rate_10_16, 
                   settle = sub$settle_rate_10_16, npc = sub$npc_rate_10_16, growth = sub$Growth_rate_GMW,
                   initial.year = 2016, cons.start = 2023, target.year.protect = 2030, 
                   target.year.restore = 2050, project.year = 2070,
                   PR.target = scenarios[j,1], RR.target = scenarios[j,2])
  
  ls[[i]] <- df.full
  ls2[[i]] <- prot.rate
  ls3[[i]] <- rest.rate
}

PR.m <- do.call(rbind, ls)
PR.m$scenario <- rep(scenarios[j,3], nrow(PR.m))
projections[[j]] <- PR.m
Pratepr.m <- do.call(rbind, ls2)
Pratepr.m$scenario <- rep(scenarios[j,3], nrow(Pratepr.m))
prot.rates[[j]] <- Pratepr.m
Rratepr.m <- do.call(rbind, ls3)
Rratepr.m$scenario <- rep(scenarios[j,3], nrow(Rratepr.m))
rest.rates[[j]] <- Rratepr.m

}

# bind all projections and save for plotting

mang.proj <- do.call(rbind, projections)
mang.proj$scenario <- factor(mang.proj$scenario, levels = c('BAU', 'P', 'P2', 'R', 'R2', 'PR', 'PR2'))
mang.proj <- arrange(mang.proj, scenario)
prot.rates.m <- do.call(rbind, prot.rates)
prot.rates.m <- distinct(prot.rates.m)
rest.rates.m <- do.call(rbind, rest.rates)
rest.rates.m <- distinct(rest.rates.m)

write.csv(mang.proj, 'outputs/mangrove-projections.csv', row.names = F)
write.csv(rest.rates.m, 'outputs/mangrove-restoration-rates.csv', row.names = F)
write.csv(prot.rates.m, 'outputs/mangrove-protection-rates.csv', row.names = F)


