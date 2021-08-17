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

#set wd#
setwd("~/Documents/r_stuff/SYR_RCode")

#read csv
r1bm <- read.csv("/Users/saus/Documents/SYR/data/r1_biomass.csv")
r2bm <- read.csv("/Users/saus/Documents/SYR/data/r2_biomass.csv")
r3bm <- read.csv("/Users/saus/Documents/SYR/data/r3_biomass.csv")

#make CO2 column
r1bm1 <- r1bm %>% 
  mutate(co2=substring(pot_id, 11, 13))
r2bm1 <- r2bm %>% 
  mutate(co2=substring(pot_id, 11, 13))
r3bm1 <- r3bm %>% 
  mutate(co2=substring(pot_id, 11, 13))
#nitrogen column 
r1bm2 <- r1bm1 %>% 
  mutate(nitrogen=substring(pot_id, 15))
r2bm2 <- r2bm1 %>% 
  mutate(nitrogen=substring(pot_id, 15))
r3bm2 <- r3bm1 %>% 
  mutate(nitrogen=substring(pot_id, 15))
#n/nn column
r1bm3 <- r1bm2 %>% 
  mutate(n_nn=if_else(r1bm2$spp_id %in% c("Chenopodium album"), "non-native", "native" ))
r2bm3 <- r2bm2 %>% 
  mutate(n_nn=if_else(r2bm2$spp_id %in% c("Plantago rugelii"), "native", "non-native" ))
r3bm3 <- r3bm2 %>% 
  mutate(n_nn=if_else(r3bm2$spp_id %in% c("Digitaria ischaemum", "Plantago major", "Potentilla indica"), "non-native", "native" ))
#combine all 3 data frames#
#fix columns
r1bm3.1 <- r1bm3 %>% 
  mutate(leaf_f_mg = "NA")
r1bm3.2 <- r1bm3.1 %>% 
  rename(leaf_d_mg = leafmass_d_mg)
#combined
bm1 <- rbind(r1bm3.2, r2bm3) #add three when you have more data

###fix p. indica co2 code and treatment code 
#make columns numeric
bm1$leaf_d_mg <- as.numeric(as.character(bm1$leaf_d_mg))
bm1$leaf_f_mg <- as.numeric(as.character(bm1$leaf_f_mg))
#remove dead plants
bm2 <- bm1 %>% 
  subset(bm1$survival == "y")
#root:shoot fresh column
bm3 <- bm2 %>% 
  mutate(root_shoot_f = blw_f_g/abv_f_g)
#root:shoot dry
bm4 <- bm3 %>% 
  mutate(root_shoot_d = blw_d_g/abv_d_g)
#RMF column
bm5 <- bm4 %>% 
  mutate(rmf = blw_d_g/total_d_g)
#LMF column
bm5.1 <- bm5 %>% 
  mutate(leaf_d_g = leaf_d_mg/1000)
bm6 <- bm5.1 %>% 
  mutate(lmf = leaf_d_g/total_d_g)
#ldmc
bm7 <- bm6 %>% 
  mutate(ldmc = total_d_g/fresh_wt_g_2)
tapply(bm7$ldmc, bm7$spp_id, mean)
ldmc_aa <- 0.1894757
ldmc_ca <- 0.2432723  
ldmc_os <- 0.2580468 
lmdc_ds <- 0.1312032 
lmdc_pr <- 0.2035800
lmdc_pi <- 0.3405158

##calulating RGR##
#bring in extras sheets
r1e <- read.csv("/Users/saus/Documents/SYR/data/rnd1_fwdw_extras.csv")
r2e <- read.csv("/Users/saus/Documents/SYR/data/r2_extras.csv")
#edit sheet
r2e1 <- select(r2e, -c("abv_f_g", "blw_f_g", "abv_d_g", "blw_d_g"))
#combine sheets
e1 <- rbind(r1e, r2e1)
#get the average of each data point#
e2 <- e1 %>% 
  mutate(avg_g = dry_wt_g/fresh_wt_g)
#seperate into each species
e_os <- e2 %>% 
  subset(e2$spp_id == "O. stricta")
e_ca <- e2 %>% 
  subset(e2$spp_id == "C. album")
e_aa <- e2 %>% 
  subset(e2$spp_id == "A. artemisiifolia")
e_pi <- e2 %>% 
  subset(e2$spp_id == "Potentilla intermedia")
e_pr <- e2 %>% 
  subset(e2$spp_id == "Plantago rugelii")
e_ds <- e2 %>% 
  subset(e2$spp_id == "Digitaria sanguinalis")
#take the mean of the averages
summary(e_os$avg_g) #do for the rest of them
moa_os <- 0.1594
moa_ca <- 0.13281
moa_aa <- 0.1987
moa_pi <- 0.3692
moa_pr <- 0.2232
moa_ds <- 0.2491
#seperate big df into species
bm_os <- bm7 %>% 
  subset(bm7$spp_id == "Oxalis stricta")
bm_ca <- bm7 %>% 
  subset(bm7$spp_id == "Chenopodium album")
bm_aa <- bm7 %>% 
  subset(bm7$spp_id == "Ambrosia artemisiifolia")
bm_pi <- bm7 %>% 
  subset(bm7$spp_id == "Potentilla intermedia")
bm_pr <- bm7 %>% 
  subset(bm7$spp_id == "Plantago rugelii")
bm_ds <- bm7 %>% 
  subset(bm7$spp_id == "Digitaria sanguinalis")
#get initial dry weight (m1)#
bmaa1 <- bm_aa %>% 
  mutate(dry_wt_1 = fresh_wt_g_1*moa_aa)
bmca1 <- bm_ca %>% 
  mutate(dry_wt_1 = fresh_wt_g_1*moa_ca)
bmos1 <- bm_os %>% 
  mutate(dry_wt_1 = fresh_wt_g_1*moa_os)
bmpi1 <- bm_pi %>% 
  mutate(dry_wt_1 = fresh_wt_g_1*moa_pi)
bmpr1 <- bm_pr %>% 
  mutate(dry_wt_1 = fresh_wt_g_1*moa_pr)
bmds1 <- bm_ds %>% 
  mutate(dry_wt_1 = fresh_wt_g_1*moa_ds)
#rgr
aa_rgr <- bmaa1 %>% 
  mutate(rgr = (log(total_d_g)-log(dry_wt_1))/15)
ca_rgr <- bmca1 %>% 
  mutate(rgr = (log(total_d_g)-log(dry_wt_1))/15)
os_rgr <- bmos1 %>% 
  mutate(rgr = (log(total_d_g)-log(dry_wt_1))/15)
pi_rgr <- bmpi1 %>% 
  mutate(rgr = (log(total_d_g)-log(dry_wt_1))/15)
pr_rgr <- bmpr1 %>% 
  mutate(rgr = (log(total_d_g)-log(dry_wt_1))/15)
ds_rgr <- bmds1 %>% 
  mutate(rgr = (log(total_d_g)-log(dry_wt_1))/15)
#combine back together
bm8 <- rbind(aa_rgr, ca_rgr, os_rgr, pi_rgr, pr_rgr, ds_rgr)

##test for normality
ggqqplot(bm8$root_shoot_f) #not normal
bm8.1 <- bm8 %>% 
  mutate(log_rsf = log(x = bm8$root_shoot_f))
ggqqplot(bm8$root_shoot_d) #not normal
bm8.2 <- bm8.1 %>% 
  mutate(log_rsd = log(x = bm8$root_shoot_d))
ggqqplot(bm8.2$rmf) #ok
ggqqplot(bm8.2$ldmc) #ok, but has weird outlier
ggqqplot(bm8.2$lmf) #tail's kinda funny
bm8.3 <- bm8.2 %>% 
  mutate(tukey_lmf = transformTukey(bm8.2$lmf)) #still kinda funky
ggqqplot(bm8$rgr) #not normal
bm8.4 <- bm8.3 %>% 
  mutate(log_rgr = log(x = bm8.3$rgr))
bm8.5 <- bm8.3 %>% 
  mutate(tukey_rgr = transformTukey(bm8.3$rgr))#still not fixed
#use 8.3 for now, change later
#plot(x = , y = ) #experiment

#rgr by species, fill nitrogen
pdf("RGR_by_spp1", width = 10, height = 10)
bm8.3 %>%
  ggplot(aes(x=spp_id, y=rgr, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Relative Growth Rate by Species") +
  xlab("Species")+
  ylab("RGR")
dev.off()
#rgr by species, fill co2
bm8.3 %>%
  ggplot(aes(x=spp_id, y=rgr, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Relative Growth Rate by Species") +
  xlab("Species")+
  ylab("RGR")

#rmf, fill nitrogen
pdf("RMF_by_spp2", width = 10, height = 10)
bm8.3 %>%
  ggplot(aes(x=spp_id, y=rmf, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Root Mass Fraction by Species") +
  xlab("Species")+
  ylab("RMF")
dev.off()
#rmf, fill co2
bm8.3 %>%
  ggplot(aes(x=spp_id, y=rmf, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Root Mass Fraction by Species") +
  xlab("Species")+
  ylab("RMF")


#ldmc, fill nitrogen
bm8.3 %>%
  ggplot(aes(x=spp_id, y=ldmc, fill=nitrogen)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Dry Matter Content by Species") +
  xlab("Species")+
  ylab("LDMC")

#ldmc, fill co2
bm8.3 %>%
  ggplot(aes(x=spp_id, y=ldmc, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Dry Matter Content by Species") +
  xlab("Species")+
  ylab("LDMC")

#prelim, look at rgr and rmf
rgraov <- aov(data = bm8.3, rgr~co2*nitrogen*n_nn*spp_id)
rmfaov <- aov(data = bm8.3, rmf~co2*nitrogen*n_nn*spp_id)
summary.aov(rgraov)
#n*spp, spp, n/nn, and n are sig
summary.aov(rmfaov)
#co2*n, spp, n/nn are sig, n and co2*spp are trending


