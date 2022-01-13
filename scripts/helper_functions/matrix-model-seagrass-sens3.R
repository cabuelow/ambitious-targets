# matrix multiplication to project seagrass extent
# under counterfactual and conservation scenario to 2030 and 2050

# first read in all data and identify sites with highest rates of loss and 
# threshold according to protection target

seagrass_protect3 <- function(x, PR.target, cons.start, target.year.protect, target.year, unprotect){
  years <- target.year.protect - cons.start
  x.sort <- x %>% arrange(IRtrend_X50.)
  row.select <- round(nrow(x.sort) * PR.target)
  x.sort$protect <- c(rep('Y', row.select), rep('N', nrow(x.sort)-row.select))
  protect.year <- round(row.select/years)
  x.sort$protect.year <- c(rep(1:years, each = protect.year), rep(0, nrow(x.sort)-years*protect.year))
  x <<- x.sort
}
  
seagrass_project4 <- function(site, M, LM, Unrest, # initial states
                             loss, growth, # loss and growth rates
                             cons.start, initial.year, target.year.protect, 
                             target.year.restore, project.year, # conservation start year and target year
                             RR.target, PR.target, protect,
                             protect.year, rest.fail){ # restoration target 
  
  # set transition probability matrix
  
  trans <- matrix(ncol = 3, nrow = 3)
  
  trans[1,1] <- 0 # meadow to meadow
  trans[2,1] <- 1 - exp(-abs(loss)) # meadow to lost meadow (loss)
  trans[3,1] <- 0
  trans[1,2] <-  1 - exp(-abs(growth)) # lost meadow to meadow (growth)
  trans[2,2] <- 0
  trans[3,2] <- 0
  trans[1,3] <- 0
  trans[2,3] <- 0
  trans[3,3] <- 0
  
  trans[1,1] <- 1 - (sum(trans[1:3,1], na.rm = T))
  trans[2,2] <- 1 - (sum(trans[1:3,2], na.rm = T))
  trans[3,3] <- 1 - (sum(trans[1:3,3], na.rm = T))
  
  # project seagrass in each state from initial year of time series to year conservation start year
  
  states <- as.matrix(c(M, LM, Unrest))
  
  years <- cons.start-initial.year
  
  ls <- list(states)
  
  for(i in 1:years){
    states2 <-  trans %*% ls[[i]]
    ls[[i+1]] <- states2
  }
  
  st <- do.call(c,ls)
  
  # store results as dataframe in list
  
  stat.names <- rep(c('M', 'LM', 'Unrest'), years+1)
  
  int <- list(rep(initial.year, 3)) # initialise for first year
  for(k in 1:years){
    sf <- rep(initial.year+k, 3)
    int[[i+k]] <- sf
  }
  year <- do.call(c, int)
  
  name <- rep(site, length(year))
  
  df <- data.frame(site = name, year = year, state = stat.names, area_ha = st)
  
  # take states from last year and run projections with different restoration/protection rates
  
  df.last <- df %>% 
    filter(year == cons.start) %>% 
    pivot_wider(id_cols = site, names_from = state, values_from = area_ha)
  
  ## project under scenario to target year
  
  states <- t(as.matrix(df.last[,-1]))
  
  # project seagrass in each state to projection year
  
  years <- project.year-cons.start
  
  # if protect is Y, 
  # set new transition probability matrix with rate of loss set to 0
  
  if(PR.target == 0){
    trans2 <- matrix(ncol = 3, nrow = 3)
    
    trans2[1,1] <- 0 # meadow to meadow
    trans2[2,1] <- 1 - exp(-abs(loss)) # meadow to lost meadow (loss)
    trans2[3,1] <- 0
    trans2[1,2] <-  1 - exp(-abs(growth)) # lost meadow to meadow (growth)
    trans2[2,2] <- 0
    trans2[3,2] <- 0
    trans2[1,3] <- 0
    trans2[2,3] <- 0
    trans2[3,3] <- 0
    
    trans2[1,1] <- 1 - (sum(trans2[1:3,1], na.rm = T))
    trans2[2,2] <- 1 - (sum(trans2[1:3,2], na.rm = T))
    trans2[3,3] <- 1 - (sum(trans2[1:3,3], na.rm = T))
  }
  else if(PR.target > 0 & protect == 'Y'){
    trans2 <- matrix(ncol = 3, nrow = 3)
    
    trans2[1,1] <- 0 # meadow to meadow
    trans2[2,1] <- 0
    trans2[3,1] <- 0
    trans2[1,2] <-  1 - exp(-abs(growth)) # lost meadow to meadow (growth)
    trans2[2,2] <- 0
    trans2[3,2] <- 0
    trans2[1,3] <- 0
    trans2[2,3] <- 0
    trans2[3,3] <- 0
    
    trans2[1,1] <- 1 - (sum(trans2[1:3,1], na.rm = T))
    trans2[2,2] <- 1 - (sum(trans2[1:3,2], na.rm = T))
    trans2[3,3] <- 1 - (sum(trans2[1:3,3], na.rm = T))
  }
  else{
    trans2 <- matrix(ncol = 3, nrow = 3)
    
    trans2[1,1] <- 0 # meadow to meadow
    trans2[2,1] <- 1 - exp(-abs(loss)) # meadow to lost meadow (loss)
    trans2[3,1] <- 0
    trans2[1,2] <-  1 - exp(-abs(growth)) # lost meadow to meadow (growth)
    trans2[2,2] <- 0
    trans2[3,2] <- 0
    trans2[1,3] <- 0
    trans2[2,3] <- 0
    trans2[3,3] <- 0
    
    trans2[1,1] <- 1 - (sum(trans2[1:3,1], na.rm = T))
    trans2[2,2] <- 1 - (sum(trans2[1:3,2], na.rm = T))
    trans2[3,3] <- 1 - (sum(trans2[1:3,3], na.rm = T))
  }
  
  # set restoration constant - ha in each year, proportion of available seagrass area available in conservation start year
    
    target.restored <- states[2] * RR.target # ha to be restored by target year
    RR <- target.restored/(target.year.restore-cons.start) # hectares to be restored in each year
    
    # calculate number of ha restored that fail in each year, if they fail they become unrestorable
    
    RR.fail <- RR*rest.fail
    
    # project 
    
    ls <- list(states) 
    
    for(i in 1:years){
      
      # matrix mult for projections
      
      if(cons.start+i > target.year.restore & i == protect.year){# if past target year, don't restore
        states2 <-  trans2 %*% ls[[i]]
        ls[[i+1]] <- states2}
      else if(ls[[i]][2,] > RR & i == protect.year){ # if amount of meadow lost is greater than RR target, we can restore
        states2 <-  trans2 %*% ls[[i]]
        states2[1,1] <- states2[1,1] + RR - RR.fail
        states2[2,1] <- states2[2,1] - RR
        states2[3,1] <- states2[3,1] + RR.fail
        ls[[i+1]] <- states2
      } else if(ls[[i]][2,] <= RR & i == protect.year){ # if amount of meadow lost is less than or equal to RR target, then make what is available
        states2 <-  trans2 %*% ls[[i]]
        states2[1,1] <- states2[1,1] + states2[2,1] - (states2[2,1]*RR.fail)
        states2[2,1] <- states2[2,1] - states2[2,1]
        states2[3,1] <- states2[3,1] + (states2[2,1]*RR.fail)
        ls[[i+1]] <- states2
      } else if(cons.start+i > target.year.restore & i < protect.year){# if past target year, don't restore
        states2 <-  trans %*% ls[[i]]
        ls[[i+1]] <- states2}
      else if(ls[[i]][2,] > RR & i < protect.year){ # if amount of meadow lost is greater than RR target, we can restore
        states2 <-  trans %*% ls[[i]]
        states2[1,1] <- states2[1,1] + RR - RR.fail
        states2[2,1] <- states2[2,1] - RR
        states2[3,1] <- states2[3,1] + RR.fail
        ls[[i+1]] <- states2
      } else if(ls[[i]][2,] <= RR & i < protect.year){ # if amount of meadow lost is less than or equal to RR target, then make what is available
        states2 <-  trans %*% ls[[i]]
        states2[1,1] <- states2[1,1] + states2[2,1] - (states2[2,1]*RR.fail)
        states2[2,1] <- states2[2,1] - states2[2,1]
        states2[3,1] <- states2[3,1] + (states2[2,1]*RR.fail)
        ls[[i+1]] <- states2
      } else if(cons.start+i > target.year.restore & i > protect.year){# if past target year, don't restore
        states2 <-  trans2 %*% ls[[i]]
        ls[[i+1]] <- states2}
      else if(ls[[i]][2,] > RR & i > protect.year){ # if amount of meadow lost is greater than RR target, we can restore
        states2 <-  trans2 %*% ls[[i]]
        states2[1,1] <- states2[1,1] + RR - RR.fail
        states2[2,1] <- states2[2,1] - RR
        states2[3,1] <- states2[3,1] + RR.fail
        ls[[i+1]] <- states2
      } else if(ls[[i]][2,] <= RR & i > protect.year){ # if amount of meadow lost is less than or equal to RR target, then make what is available
        states2 <-  trans2 %*% ls[[i]]
        states2[1,1] <- states2[1,1] + states2[2,1] - (states2[2,1]*RR.fail)
        states2[2,1] <- states2[2,1] - states2[2,1]
        states2[3,1] <- states2[3,1] + (states2[2,1]*RR.fail)
        ls[[i+1]] <- states2
      } 
      }

    st <- do.call(c,ls)
  
  # store results as dataframe in list
  
  stat.names <- rep(c('M', 'LM', 'Unrest'), years+1)
  
  int <- list(rep(cons.start, 3)) # initialise for first year
  for(k in 1:years){
    sf <- rep(cons.start+k, 3)
    int[[i+k]] <- sf
  }
  year <- do.call(c, int)
  
  name <- rep(site, length(year))
  
  df2 <- data.frame(site = name, year = year, state = stat.names, area_ha = st)
  
  df.full <<- rbind(df, filter(df2, year!=cons.start)) 
  
  rest.rate <<- data.frame(country = site, years = years, rest_ha_yr = RR)
}