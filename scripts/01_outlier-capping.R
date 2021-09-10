# wrangle seagrass and mangrove data, calculate trend z-scores, and cap those with z-score + or -3

library(tidyverse)

# data

mang <- read.csv('outputs/mangrove-states-rates.csv') %>% 
  arrange(Country)
seag <- read.csv('data/seagrass-trends.csv')

# mangroves

# first find outliers and save for reporting

mang.out <- mang %>% 
  mutate(zscore_growth = (Growth_rate_GMW - mean(Growth_rate_GMW))/sd(Growth_rate_GMW),
         zscore_loss = (total_rate_10_16 - mean(total_rate_10_16))/sd(total_rate_10_16)) %>% 
  filter(zscore_growth > 3 | zscore_growth < -3 | zscore_loss > 3 | zscore_loss < -3)

write.csv(mang.out, 'outputs/mang-outliers.csv', row.names = F)

# find maximum and minimum rates of loss and growth for countries that are not outliers

mang.nout <- mang %>% 
  mutate(zscore_growth = (Growth_rate_GMW - mean(Growth_rate_GMW))/sd(Growth_rate_GMW),
         zscore_loss = (total_rate_10_16 - mean(total_rate_10_16))/sd(total_rate_10_16)) %>% 
  filter(zscore_growth < 3 & zscore_growth > -3) %>% 
  filter(zscore_loss < 3 & zscore_loss > -3)

# cap outliers with min or max rates of change

mang.cap <- mang %>% 
  mutate(zscore_growth = (Growth_rate_GMW - mean(Growth_rate_GMW))/sd(Growth_rate_GMW),
         zscore_loss = (total_rate_10_16 - mean(total_rate_10_16))/sd(total_rate_10_16)) %>% 
  mutate(Growth_rate_GMW = ifelse(zscore_growth > 3, max(mang.nout$Growth_rate_GMW), Growth_rate_GMW),
         ewe_rate_10_16 = ifelse(zscore_loss < -3 & ewe_rate_10_16 < min(mang.nout$ewe_rate_10_16), min(mang.nout$ewe_rate_10_16), ewe_rate_10_16),
         erosion_rate_10_16 = ifelse(zscore_loss < -3 & erosion_rate_10_16 < min(mang.nout$erosion_rate_10_16), min(mang.nout$erosion_rate_10_16), erosion_rate_10_16),
         commodities_rate_10_16 = ifelse(zscore_loss < -3 & commodities_rate_10_16 < min(mang.nout$commodities_rate_10_16), min(mang.nout$commodities_rate_10_16), commodities_rate_10_16),
         npc_rate_10_16 = ifelse(zscore_loss < -3 & npc_rate_10_16 < min(mang.nout$npc_rate_10_16), min(mang.nout$npc_rate_10_16), npc_rate_10_16),
         settle_rate_10_16 = ifelse(zscore_loss < -3 & settle_rate_10_16 < min(mang.nout$settle_rate_10_16), min(mang.nout$settle_rate_10_16), settle_rate_10_16))

write.csv(mang.cap, 'outputs/mangrove-capped.csv', row.names = F)

# seagrass

# find outliers and save for reporting

seag.out <- seag %>% 
  filter(!is.na(area_start_X50.)) %>% # GAMs did not estimate area at these sites
  filter(!is.na(IRtrend_X50.)) %>% 
  mutate(growth = ifelse(IRtrend_X50. > 0, IRtrend_X50., 0),# set positive trends as growth rate, negative trends as loss rate
         loss = ifelse(IRtrend_X50. < 0, IRtrend_X50., 0),
         restore_area = max_area - area_start_X50.,
         zscore = (IRtrend_X50. - mean(IRtrend_X50.))/sd(IRtrend_X50.)) %>% 
  mutate(restore_area = ifelse(restore_area < 0, 0, restore_area)) %>% 
  filter(zscore > 3 | zscore < -3)

write.csv(seag.out, 'outputs/seag-outliers.csv', row.names = F)

# find maximum and minimum rates of loss and growth for countries that are not outliers

seag1 <- seag %>% 
  filter(!is.na(area_start_X50.)) %>% # GAMs did not estimate area at these sites
  filter(!is.na(IRtrend_X50.)) %>% 
  mutate(growth = ifelse(IRtrend_X50. > 0, IRtrend_X50., 0),# set positive trends as growth rate, negative trends as loss rate
         loss = ifelse(IRtrend_X50. < 0, IRtrend_X50., 0),
         restore_area = max_area - area_start_X50.,
         zscore = (IRtrend_X50. - mean(IRtrend_X50.))/sd(IRtrend_X50.)) %>% 
  mutate(restore_area = ifelse(restore_area < 0, 0, restore_area)) %>% 
  filter(zscore < 3 & zscore > -3)

# cap outliers with min or max rates of change

max.growth <- max(seag1$growth)
min.loss <- min(seag1$loss)

seag2 <- seag %>% 
  filter(!is.na(area_start_X50.)) %>%
  filter(!is.na(IRtrend_X50.)) %>% 
  mutate(growth = ifelse(IRtrend_X50. > 0, IRtrend_X50., 0),
         loss = ifelse(IRtrend_X50. < 0, IRtrend_X50., 0),
         restore_area = max_area - area_start_X50.,
         zscore = (IRtrend_X50. - mean(IRtrend_X50.))/sd(IRtrend_X50.)) %>% 
  mutate(restore_area = ifelse(restore_area < 0, 0, restore_area)) %>% 
  mutate(growth = ifelse(zscore > 3 & growth !=0, max.growth, growth),
         loss = ifelse(zscore < -3 & loss != 0, min.loss, loss))

write.csv(seag2, 'outputs/seagrass-capped.csv', row.names = F)




