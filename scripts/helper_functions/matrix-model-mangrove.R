# matrix multiplication to project mangrove extent through time
# under baseline (BAU) and conservation scenarios
# assumes can restore in protected areas

mangrove_project <- function(country, PF, UF, PD, UD, # initial states
                             ewe, erosion, commodities, settle, npc, growth, # loss and growth rates
                             cons.start, initial.year, # conservation start year, initial year of projections, before conservation starts
                             target.year.protect, target.year.restore, project.year, # target year for protection, target year for restoration, final year of projections
                             PR.target, RR.target){ # protection target (proportion), restoration target (proportion)
  
  # set transition probability matrix (i.e probabilities of transitions from one habitat state to another)
  
  trans <- matrix(ncol = 5, nrow = 5)
  
  trans[2,1] <- 0
  trans[3,1] <- 1 - exp(-abs(ewe))
  trans[4,1] <- 0
  trans[5,1] <- 1 - exp(-abs(erosion))
  trans[1,2] <- 0 
  trans[3,2] <- 0
  trans[4,2] <- 1 - exp((-abs(ewe)) + (-abs(commodities)) + (-abs(npc)))
  trans[5,2] <- 1 - exp((-abs(erosion)) + (-abs(settle)))
  trans[1,3] <- 1 - exp(-abs(growth))
  trans[2,3] <- 0
  trans[4,3] <- 0
  trans[5,3] <- 1 - exp(-abs(erosion))
  trans[1,4] <- 0
  trans[2,4] <- 1 - exp(-abs(growth))
  trans[3,4] <- 0
  trans[5,4] <- 1 - exp((-abs(erosion)) + (-abs(settle)))
  trans[1,5] <- 0
  trans[2,5] <- 0
  trans[3,5] <- 0
  trans[4,5] <- 0
  
  trans[1,1] <- 1 - (sum(trans[1:5,1], na.rm = T))
  trans[2,2] <- 1 - (sum(trans[1:5,2], na.rm = T))
  trans[3,3] <- 1 - (sum(trans[1:5,3], na.rm = T))
  trans[4,4] <- 1 - (sum(trans[1:5,4], na.rm = T))
  trans[5,5] <- 1 - (sum(trans[1:5,5], na.rm = T))
  
  # project mangrove forest in each state from initial year to conservation start year
  
  states <- as.matrix(c(PF, UF, PD, UD, 0))
  
  years <- cons.start-initial.year
  
  ls <- list(states)
  
  for(i in 1:years){
    states2 <-  trans %*% ls[[i]]
    ls[[i+1]] <- states2
  }
  
  st <- do.call(c,ls)
  
  # store results as dataframe in list
  
  stat.names <- rep(c('PF', 'UF', 'PD', 'UD', 'Unrest'), years+1)
  
  int <- list(rep(initial.year, length(states))) # initialise for first year
  for(k in 1:years){
    sf <- rep(initial.year+k, length(states))
    int[[i+k]] <- sf
  }
  year <- do.call(c, int)
  
  name <- rep(country, length(year))
  
  df <- data.frame(country = name, year = year, state = stat.names, area_ha = st)
  
  # take states from last year and run projections with different restoration/protection rates
  
  df.last <- df %>% 
    filter(year == cons.start) %>% 
    pivot_wider(id_cols = country, names_from = state, values_from = area_ha)
  
  states <- t(as.matrix(df.last[,-1]))
  
  years <- project.year-cons.start
  
  # set protection target
  
  forest <- states[1] + states[2]
  prop.protected <- states[1]/forest # proportion of forest protected by cons start year
  
  if(is.nan(prop.protected)){ # if proportion protected is not a number, set it to zero
    prop.protected <- 0
  }

  if(PR.target == 0){ # if the protection target is 0, make it the protection target
    PR.target <- PR.target
  }
  else if(prop.protected >= PR.target){ # otherwise if propotion protected is greater than or equal to target, set to zero
    PR.target <- 0
  }else if(prop.protected < PR.target){ # otherwise if propotion protected is less than target target, set the target as the difference 
    PR.target <- PR.target - prop.protected
  }

  # calculate number of ha to be protected in each year to reach protected target by target year
  
  target.protected <- forest * PR.target # ha to be protected by target year
  
  if(target.protected > states[2]){ # if the target to be protected is greater than what is unprotected, make it equal to what is unprotected
    target.protected <- states[2]
  }
  
  PR <- target.protected/(target.year.protect-cons.start) # number of ha to be protected each year

  # calculate the number of ha to be restored in each year to reach the restoration target
    
    target.restored <- (states[3] + states[4]) * RR.target # ha to be restored by target year, a percentatage of the total deforested area
    RR <- target.restored/(target.year.restore-cons.start) # hectares to be restored in each year
    
  # project extent to final year
    
    ls <- list(states) 
    
    for(i in 1:years){
      
      # matrix mult for projections
      
      if(cons.start+i > target.year.protect & cons.start+i <= target.year.restore & sum(ls[[i]][3,], ls[[i]][4,]) > RR){# if past target year for protection, don't protect
          states2 <-  trans %*% ls[[i]]
          states2[1,1] <- states2[1,1]
          states2[2,1] <- states2[2,1] + RR
          states2[3,1] <- states2[3,1] - (RR*(states2[3,1]/(states2[3,1]+states2[4,1])))
          states2[4,1] <- states2[4,1] - (RR*(states2[4,1]/(states2[3,1]+states2[4,1])))
          ls[[i+1]] <- states2
        }
        else if(cons.start+i > target.year.protect & cons.start+i <= target.year.restore & sum(ls[[i]][3,], ls[[i]][4,]) <= RR){# if area of UD is <= 0, make restoration what is available
          states2 <-  trans %*% ls[[i]]
          states2[1,1] <- states2[1,1]
          states2[2,1] <- states2[2,1] + states2[4,1] + states2[3,1]
          states2[3,1] <- states2[3,1] - states2[3,1]
          states2[4,1] <- states2[4,1] - states2[4,1]
          ls[[i+1]] <- states2
        }
      else if(cons.start+i <= target.year.protect & ls[[i]][2,] > PR & sum(ls[[i]][3,], ls[[i]][4,]) > RR){# if UF and UD are greater than amount needed for protection and restoration, can protect and restore
        states2 <-  trans %*% ls[[i]]
        states2[1,1] <- states2[1,1] + PR
        states2[2,1] <- states2[2,1] - PR + RR
        states2[3,1] <- states2[3,1] - (RR*(states2[3,1]/(states2[3,1]+states2[4,1])))
        states2[4,1] <- states2[4,1] - (RR*(states2[4,1]/(states2[3,1]+states2[4,1])))
        ls[[i+1]] <- states2
      }
      else if(cons.start+i <= target.year.protect & ls[[i]][2,] <= PR & sum(ls[[i]][3,], ls[[i]][4,]) > RR){# if area of UF is <= PR, make the protection what is available
        states2 <-  trans %*% ls[[i]]
        states2[1,1] <- states2[1,1] + states2[2,1]
        states2[2,1] <- states2[2,1] - states2[2,1] + RR
        states2[3,1] <- states2[3,1] - (RR*(states2[3,1]/(states2[3,1]+states2[4,1])))
        states2[4,1] <- states2[4,1] - (RR*(states2[4,1]/(states2[3,1]+states2[4,1])))
        ls[[i+1]] <- states2
      }
      else if(cons.start+i <= target.year.protect & sum(ls[[i]][3,], ls[[i]][4,]) <= RR & ls[[i]][2,] > PR){# if area of UD is <= 0, make restoration what is available
        states2 <-  trans %*% ls[[i]]
        states2[1,1] <- states2[1,1] + PR
        states2[2,1] <- states2[2,1] - PR + states2[4,1] + states2[3,1]
        states2[3,1] <- states2[3,1] - states2[3,1]
        states2[4,1] <- states2[4,1] - states2[4,1]
        ls[[i+1]] <- states2
      }
      else if(cons.start+i <= target.year.protect & sum(ls[[i]][3,], ls[[i]][4,]) <= RR & ls[[i]][2,] <= PR){# if area of UF and UD is <= 0, cannot restore or protect
        states2 <-  trans %*% ls[[i]]
        states2[1,1] <- states2[1,1] + states2[2,1]
        states2[2,1] <- states2[2,1] - states2[2,1] + states2[4,1] + states2[3,1]
        states2[3,1] <- states2[3,1] - states2[3,1]
        states2[4,1] <- states2[4,1] - states2[4,1]
        ls[[i+1]] <- states2
      }
      else if(cons.start+i > target.year.restore){
        states2 <-  trans %*% ls[[i]]
        ls[[i+1]] <- states2
      }
    }
    
    st <- do.call(c,ls)
    
  # store results as dataframe and save output
  
  stat.names <- rep(c('PF', 'UF', 'PD', 'UD', 'Unrest'), years+1)
  
  int <- list(rep(cons.start, length(states))) # initialise for first year
  for(k in 1:years){
    sf <- rep(cons.start+k, length(states))
    int[[i+k]] <- sf
  }
  year <- do.call(c, int)
  
  name <- rep(country, length(year))
  
  df2 <- data.frame(country = name, year = year, state = stat.names, area_ha = st)
  
  df.full <<- rbind(df, filter(df2, year!=cons.start)) 
  rest.rate <<- data.frame(country = name, years = years, rest_ha_yr = RR)
  prot.rate <<- data.frame(country = name, years = years, prot_ha_yr = PR)
  
}