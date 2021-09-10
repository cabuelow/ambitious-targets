# project seagrass extent to 2070 under baseline (BAU), protection and restoration scenarios 

library(tidyverse)
library(sf)
source('scripts/helper_functions/matrix-model-seagrass.R')

# data

seag <- read.csv('outputs/seagrass-capped.csv')

# scenarios

pr <- seq(from = 0, to = 0.9, by = 0.1)
rr <- seq(from = 0, to = 0.9, by = 0.1)

scenarios <- expand.grid(PR = pr, RR = rr)

# loop through each scenario, run projections, and save results

projections <- list() # list for storing projections
prot.rates <- list() # list for storing protection rates
rest.rates <- list() # list for storing restoratoin rates

for(j in 1:nrow(scenarios)){
  
# identify sites that are priorities for protection

seagrass_protect(seag, PR.target = scenarios[j,1], target.year.protect = 2030, cons.start = 2023)

# loop through each site and project

ls <- list()
ls2 <- list()
ls3 <- list()

for(i in 1:nrow(seag)){
  
  sub <- x[i,] # select a site
  
  seagrass_project(site = sub$study_site, M = sub$area_start, LM = sub$restore_area,
                   loss = sub$loss, growth = sub$growth,
                   cons.start = 2023, initial.year = 2000, target.year.protect = 2030, 
                   target.year.restore = 2050, project.year = 2070,
                   RR.target = scenarios[j,2], PR.target = scenarios[j,1], protect = sub$protect, protect.year = sub$protect.year)
  ls[[i]] <- df.full
  ls2[[i]] <- rest.rate
  ls3[[i]] <- prot.rate
}

PR.s2 <- do.call(rbind, ls)
PR.s2$PR <- rep(scenarios[j,1], nrow(PR.s2))
PR.s2$RR <- rep(scenarios[j,2], nrow(PR.s2))
projections[[j]] <- PR.s2
Rratepr.s2 <- do.call(rbind, ls2)
Rratepr.s2$PR <- rep(scenarios[j,1], nrow(Rratepr.s2))
Rratepr.s2$RR <- rep(scenarios[j,2], nrow(Rratepr.s2))
rest.rates[[j]] <- Rratepr.s2
Pratepr.s2 <- do.call(rbind, ls3)
Pratepr.s2$PR <- rep(scenarios[j,1], nrow(Pratepr.s2))
Pratepr.s2$RR <- rep(scenarios[j,2], nrow(Pratepr.s2))
prot.rates[[j]] <- Pratepr.s2

}

# bind all projections and save for plotting

seag.proj <- do.call(rbind, projections)
rest.rates.s <- do.call(rbind, rest.rates)
rest.rates.s <- distinct(rest.rates.s)
prot.rates.s <- do.call(rbind, prot.rates)
prot.rates.s <- distinct(prot.rates.s)
prot.rates.s <- filter(prot.rates.s, !is.na(prot_ha_yr))

saveRDS(seag.proj, 'outputs/seagrass-projections-all.rds')
saveRDS(rest.rates.s, 'outputs/seagrass-restoration-rates-all.rds')
saveRDS(prot.rates.s, 'outputs/seagrass-protection-rates-all.rds')
 
