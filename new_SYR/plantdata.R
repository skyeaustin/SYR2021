#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(rcompanion)
library(nlme) #for mixed linear and generalized linear models
library(lme4) #for mixed linear and generalized linear models
library(devtools)
library(lmerTest)

#set wd#
setwd("~/Documents/r_stuff/SYR2021-1/new_SYR") #skye's mac
setwd("~/SYR/SYR_rcode/SYR2021/new_SYR") #skye's pc

###Plant Data###
#csvs
p1 <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\plantdata\\r1_leaves.csv")
  #get rid of treatment column
  p1.1 <- p1 %>% 
    
p2 <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\plantdata\\r2_leaves.csv")
p3 <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\plantdata\\r3_leaves.csv")
  #fix PIN cols
  pinfix <- p3 %>% 
    subset(p3$spp_id == "Potentilla indica")
  pinfix1 <- pinfix %>% 
    mutate(co2=substring(pot_id, 12, 14))
  pinfix2 <- pinfix1 %>% 
    mutate(nutrient=substring(pot_id, 16))

#make r3 w/o p.indica
r3_nopin <- r3bm %>% 
  subset(r3bm$spp_id != "Potentilla indica")

#make CO2 column
r1bm_co2 <- r1bm %>% 
  mutate(co2=substring(pot_id, 11, 13))
r2bm_co2 <- r2bm %>% 
  mutate(co2=substring(pot_id, 11, 13))
r3bm_co2 <- r3_nopin %>% 
  mutate(co2=substring(pot_id, 11, 13))

#nutrient column 
r1bm_n <- r1bm_co2 %>% 
  mutate(nutrient=substring(pot_id, 15))
r2bm_n <- r2bm_co2 %>% 
  mutate(nutrient=substring(pot_id, 15))
r3bm_n <- r3bm_co2 %>% 
  mutate(nutrient=substring(pot_id, 15))

#add p.indica back in to r3
r3 <- bind_rows(r3bm_n, pinfix2)

#n/nn column
r1bm_native <- r1bm_n %>% 
  mutate(n_nn=if_else(r1bm_n$spp_id %in% c("Chenopodium album"), "non-native", "native" ))
r2bm_native <- r2bm_n %>% 
  mutate(n_nn=if_else(r2bm_n$spp_id %in% c("Plantago rugelii"), "native", "non-native" ))
r3bm_native <- r3 %>% 
  mutate(n_nn="non-native")


###Leaf Area/SLA###