#load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(rcompanion)
library(nlme)
library(lme4)
library(devtools)
library(lmerTest)
library(vegan)

#set wd#
setwd("~/Documents/r_stuff/SYR2021-1/new_SYR") #skye's mac
setwd("~/SYR/SYR_rcode/SYR2021/new_SYR") #skye's pc

###Plant Data###
#skye's mac#
p1 <- read.csv("/Users/saus/Documents/SYR/datadata/plantdata/r1_leaves.csv")
#skye's PC#
p1 <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\plantdata\\r1_leaves.csv")
#make columns numeric
p1$height_cm <- as.numeric(as.character(p1$height_cm))
p1$leaves_emerged <- as.numeric(as.character(p1$leaves_emerged))
p1$leaves_emerging <- as.numeric(as.character(p1$leaves_emerging))
p1$leaves_dead <- as.numeric(as.character(p1$leaves_dead))
#get rid of treatment column
p1.1 = subset(p1, select = -eval(parse(text=treatment)))

#mac
p2 <- read.csv("/Users/saus/Documents/SYR/datadata/plantdata/r2_leaves.csv")
#pc
p2 <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\plantdata\\r2_leaves.csv")
#make columns numeric
p2$height_cm <- as.numeric(as.character(p2$height_cm))
p2$leaves_emerged <- as.numeric(as.character(p2$leaves_emerged))
p2$leaves_emerging <- as.numeric(as.character(p2$leaves_emerging))
p2$leaves_dead <- as.numeric(as.character(p2$leaves_dead))

#mac
p3 <- read.csv("/Users/saus/Documents/SYR/datadata/plantdata/r3_leaves.csv")
#pc
p3 <- "C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\plantdata\\r3_leaves.csv"
#make columns numeric
p3$height_cm <- as.numeric(as.character(p3$height_cm))
p3$leaves_emerged <- as.numeric(as.character(p3$leaves_emerged))
p3$leaves_emerging <- as.numeric(as.character(p3$leaves_emerging))
p3$leaves_dead <- as.numeric(as.character(p3$leaves_dead))
#fix PIN cols
pinfix <- p3 %>% 
  subset(p3$spp_id == "Potentilla indica")
pinfix1 <- pinfix %>% 
  mutate(co2=substring(pot_id, 12, 14))
pinfix2 <- pinfix1 %>% 
  mutate(nutrient=substring(pot_id, 16))


#make columns numeric
p1$height_cm <- as.numeric(as.character(p1$height_cm))
p1$leaves_emerged <- as.numeric(as.character(p1$leaves_emerged))
p1$leaves_emerging <- as.numeric(as.character(p1$leaves_emerging))
p1$leaves_dead <- as.numeric(as.character(p1$leaves_dead))
#get rid of treatment column
p1.1 = subset(p1, select = -eval(parse(text=treatment)))
p2 <- read.csv("/Users/saus/Documents/SYR/datadata/plantdata/r2_leaves.csv")
#make columns numeric
p2$height_cm <- as.numeric(as.character(p2$height_cm))
p2$leaves_emerged <- as.numeric(as.character(p2$leaves_emerged))
p2$leaves_emerging <- as.numeric(as.character(p2$leaves_emerging))
p2$leaves_dead <- as.numeric(as.character(p2$leaves_dead))
p3 <- read.csv("/Users/saus/Documents/SYR/datadata/plantdata/r3_leaves.csv")
#make columns numeric
p3$height_cm <- as.numeric(as.character(p3$height_cm))
p3$leaves_emerged <- as.numeric(as.character(p3$leaves_emerged))
p3$leaves_emerging <- as.numeric(as.character(p3$leaves_emerging))
p3$leaves_dead <- as.numeric(as.character(p3$leaves_dead))
#fix PIN cols
pinfix <- p3 %>% 
  subset(p3$spp_id == "Potentilla indica")
pinfix1 <- pinfix %>% 
  mutate(co2=substring(pot_id, 12, 14))
pinfix2 <- pinfix1 %>% 
  mutate(nutrient=substring(pot_id, 16))

#add cols and combine#

#take out pin from 3
p3_nopin <- p3 %>% 
  subset(p3$spp_id != "Potentilla indica")

#add co2 and nutrient to 1, 2, and 3 no pin
p1_co2 <- p1.1 %>% 
  mutate(co2=substring(pot_id, 11, 13))
p2_co2 <- p2 %>% 
  mutate(co2=substring(pot_id, 11, 13))
p3nopin_co2 <- p3_nopin %>% 
  mutate(co2=substring(pot_id, 11, 13))

#nutrient column to 1, 2, and 3 no pin
p1_n <- p1_co2 %>% 
  mutate(nutrient=substring(pot_id, 15))
p2_n <- p2_co2 %>% 
  mutate(nutrient=substring(pot_id, 15))
p3nopin_n <- p3nopin_co2 %>% 
  mutate(nutrient=substring(pot_id, 15))

#add pin back to 3
p3_n <- bind_rows(pinfix2, p3nopin_n)

#remove empty rows
p3_n <- na.omit(p3_n)

#combine all 3 plant data sets
plantdat1 <- bind_rows(p3_n, p2_n, p1_n)

#add n/nn col         ##NOT DONE##
r1bm_native <- r1bm_n %>% 
  mutate(n_nn=if_else(r1bm_n$spp_id %in% c("Chenopodium album"), "non-native", "native" ))
r2bm_native <- r2bm_n %>% 
  mutate(n_nn=if_else(r2bm_n$spp_id %in% c("Plantago rugelii"), "native", "non-native" ))
r3bm_native <- r3 %>% 
  mutate(n_nn="non-native")

#remove dead plants


##add in leaf area stuff##
#skye's pc
la2 <- "C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\leafarea\\total_leafarea_r2.csv"
la3 <- "C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\leafarea\\total_leafarea_r3.csv"
















