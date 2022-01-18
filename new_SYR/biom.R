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
library(vegan)

#set wd#
setwd("~/Documents/r_stuff/SYR2021-1/new_SYR") #skye's mac
setwd("~/SYR/SYR_rcode/SYR2021/new_SYR") #skye's pc

#read csv#
#mac
r1bm <- read.csv("/Users/saus/Documents/SYR/datadata/biomass/r1_biomass.csv")
r2bm <- read.csv("/Users/saus/Documents/SYR/datadata/biomass/r2_biomass.csv")
r3bm <- read.csv("/Users/saus/Documents/SYR/datadata/biomass/r3_biomass.csv")

#PC
r1bm <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\biomass\\r1_biomass.csv")
r2bm <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\biomass\\r2_biomass.csv")
r3bm <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\biomass\\r3_biomass.csv")

#fix p. indica columns
pinfix <- r3bm %>% 
  subset(r3bm$spp_id == "Potentilla indica")
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

#combine all 3 data frames#
bm1 <- bind_rows(r1bm_native, r2bm_native, r3bm_native)

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

##calculating RGR##
#bring in extras sheets
r1e <- read.csv("/Users/saus/Documents/SYR/datadata/biomass/r1_extras.csv")
r2e <- read.csv("/Users/saus/Documents/SYR/datadata/biomass/r2_extras.csv")
r3e <- read.csv("/Users/saus/Documents/SYR/datadata/biomass/r3_extras.csv")

#PC
r1e <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\biomass\\r1_extras.csv")
r2e <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\biomass\\r2_extras.csv")
r3e <- read.csv("C:\\Users\\Airsi\\OneDrive\\Documents\\SYR\\datadata\\biomass\\r3_extras.csv")

#edit sheet
r2e1 <- select(r2e, -c("abv_f_g", "blw_f_g", "abv_d_g", "blw_d_g"))
r3e1 <- select(r3e, -c("abv_f_g", "blw_f_g", "abv_d_g", "blw_d_g"))

#combine sheets
e1 <- rbind(r1e, r2e1, r3e1)

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
e_di <- e2 %>% 
  subset(e2$spp_id == "Digitaria ischaemum")
e_pm <- e2 %>% 
  subset(e2$spp_id == "Plantago major")
e_pin <- e2 %>% 
  subset(e2$spp_id == "Potentilla indica")


#take the mean of the averages
summary(e_pin$avg_g) #do for the rest of them
moa_os <- 0.1594
moa_ca <- 0.13281
moa_aa <- 0.1987
moa_pi <- 0.3692
moa_pr <- 0.2232
moa_ds <- 0.2491
moa_di <- 0.08759
moa_pm <- 0.1843
moa_pin <- 0.3360
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
bm_di <- bm7 %>% 
  subset(bm7$spp_id == "Digitaria ischaemum")
bm_pm <- bm7 %>% 
  subset(bm7$spp_id == "Plantago major")
bm_pin <- bm7 %>% 
  subset(bm7$spp_id == "Potentilla indica")

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
bmdi1 <- bm_di %>% 
  mutate(dry_wt_1 = fresh_wt_g_1*moa_di)
bmpm1 <- bm_pm %>% 
  mutate(dry_wt_1 = fresh_wt_g_1*moa_pm)
bmpin1 <- bm_pin %>% 
  mutate(dry_wt_1 = fresh_wt_g_1*moa_pin)

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
di_rgr <- bmdi1 %>% 
  mutate(rgr = (log(total_d_g)-log(dry_wt_1))/15)
pm_rgr <- bmpm1 %>% 
  mutate(rgr = (log(total_d_g)-log(dry_wt_1))/15)
pin_rgr <- bmpin1 %>% 
  mutate(rgr = (log(total_d_g)-log(dry_wt_1))/15)

#combine back together
bm8 <- rbind(aa_rgr, ca_rgr, os_rgr, pi_rgr, pr_rgr, ds_rgr, di_rgr, pm_rgr, pin_rgr)

##test for normality
bm8.1 <- bm8 %>% 
  mutate(log_rgr = log(x=bm8$rgr))
ggqqplot(bm8.1$log_rgr)#best one


ggqqplot(bm8.1$rmf) #ok

ggqqplot(bm8.1$ldmc) #ok, but has weird outlier

ggqqplot(bm8.1$lmf) #tail's kinda funny

##do i need these??##
ggqqplot(bm8.1$root_shoot_f) #not normal
ggqqplot(bm8.1$root_shoot_d) #not normal
####

#add fw2/fw1 column
bm9 <- bm8.1 %>% 
  mutate(fw2fw1 = bm8.1$fresh_wt_g_2/bm8.1$fresh_wt_g_1)

###QUESTIONS/HYPOTHESES###
##plant response to nutrient/co2##
rgraov1 <- aov(data = bm9, log_rgr~co2*nutrient) #nutrient**
rgraov2 <- aov(data = bm9, log_rgr~co2*nutrient*spp_id) #co2*, nutrient***, spp_id***, nutrient:spp_id***
#
ldmcaov1 <- aov(data = bm9, ldmc~co2*nutrient)
ldmcaov2 <- aov(data = bm9, ldmc~co2*nutrient*spp_id) #spp_id***, nutrient:spp_id*
#
lmfaov1 <- aov(data = bm9, lmf~co2*nutrient)#nutrient., co2:nutrient.
lmfaov2 <- aov(data = bm9, lmf~co2*nutrient*spp_id)#nutrient***, spp_id***, co2:nutrient*, co2:nutrient:spp_id***
#
rmfaov1 <- aov(data = bm9, rmf~co2*nutrient)#nutrient., co2:nutrient*
rmfaov2 <- aov(data = bm9, rmf~co2*nutrient*spp_id)#nutrient*, spp_id***, co2:nutrient*

####

rgr_lmer <- lmer(data = bm9, rgr~co2*nutrient-1 + (1|spp_id), subset = bm9$rgr>0) #can i log transform this? or do i have to take out the subset?
#
ldmc_lmer <- lmer(data = bm9, ldmc~co2*nutrient-1 + (1|spp_id))
#
lmf_lmer <- lmer(data = bm9, lmf~co2*nutrient-1 + (1|spp_id))
#
rmf_lmer <- lmer(data = bm9, rmf~co2*nutrient-1 + (1|spp_id))

###

boxplot(data = bm9, log_rgr~co2)
boxplot(data = bm9, log_rgr~nutrient)
boxplot(data = bm9, log_rgr~nutrient*co2)
#
boxplot(data = bm9, ldmc~co2)
boxplot(data = bm9, ldmc~nutrient)
boxplot(data = bm9, ldmc~nutrient*co2)
#
boxplot(data = bm9, lmf~co2)
boxplot(data = bm9, lmf~nutrient)
boxplot(data = bm9, lmf~nutrient*co2)
#
boxplot(data = bm9, rmf~co2)
boxplot(data = bm9, rmf~nutrient)
boxplot(data = bm9, rmf~nutrient*co2)


#RGR co2, fill nutrient
bm9 %>%
  ggplot(aes(x=co2, y=rgr, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Relative Growth Rate (RGR) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("RGR (g/g-1 d-1)")

#RGR by species, fill nutrient
bm9 %>%
  ggplot(aes(x=spp_id, y=rgr, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Relative Growth Rate (RGR) by Species") +
  xlab("Species")+
  ylab("RGR (g/g-1 d-1)")

#RGR by species, fill CO2
bm9 %>%
  ggplot(aes(x=spp_id, y=rgr, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Relative Growth Rate by Species") +
  xlab("Species")+
  ylab("RGR (g/g-1 d-1)")

#ldmc co2, fill nutrient
bm9 %>%
  ggplot(aes(x=co2, y=ldmc, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Dry Matter Content (LDMC) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("LDMC (units)")

#ldmc by species, fill nutrient
bm9 %>%
  ggplot(aes(x=spp_id, y=ldmc, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Dry Matter Content (LDMC) by Species") +
  xlab("Species")+
  ylab("LDMC (units)")

#ldmc by species, fill CO2
bm9 %>%
  ggplot(aes(x=spp_id, y=ldmc, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Dry Matter Content (LDMC) by Species") +
  xlab("Species")+
  ylab("LDMC (units)")

#lmf by co2, fill nutrient
bm9 %>%
  ggplot(aes(x=co2, y=lmf, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Mass Fraction (LMF) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("LMF (units)")

#lmf by species, fill co2
bm9 %>%
  ggplot(aes(x=spp_id, y=lmf, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Mass Fraction (LMF) by Species") +
  xlab("Species")+
  ylab("LMF (units)")

#lmf by species fill nutrient
bm9 %>%
  ggplot(aes(x=spp_id, y=lmf, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Leaf Mass Fraction (LMF) by Species") +
  xlab("Species")+
  ylab("LMF (units)")

#rmf by co2, fill nutrient
bm9 %>%
  ggplot(aes(x=co2, y=rmf, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Root Mass Fraction (RMF) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("RMF (units)")

#lmf by species, fill co2
bm9 %>%
  ggplot(aes(x=spp_id, y=rmf, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Root Mass Fraction (RMF) by Species") +
  xlab("Species")+
  ylab("RMF (units)")

#lmf by species fill nutrient
bm9 %>%
  ggplot(aes(x=spp_id, y=rmf, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Root Mass Fraction (RMF) by Species") +
  xlab("Species")+
  ylab("RMF (units)")
##############################################################################
##response by nativity##
rgraov3 <- aov(data = bm9, log_rgr~co2*nutrient*n_nn) #nutrient**
rgraov4 <- aov(data = bm9, log_rgr~co2*nutrient*n_nn*spp_id) #nutrient***, spp_id***, nutrient:n_nn., nutrient:spp_id***
#
ldmcaov3 <- aov(data = bm9, ldmc~co2*nutrient*n_nn) #nothing
ldmcaov4 <- aov(data = bm9, ldmc~co2*nutrient*n_nn*spp_id) #n_nn., spp_id***, nutrient:n_nn., nutrient:spp_id.
#
rmfaov3 <- aov(data = bm9, rmf~co2*nutrient*n_nn) #nutrient., n_nn**, co2:nutrient.
rmfaov4 <- aov(data = bm9, rmf~co2*nutrient*n_nn*spp_id) #nutrient*, n_nn***, spp_id***, co2:nutrient*
#
lmfaov3 <- aov(data = bm9, lmf~co2*nutrient*n_nn) #nutrient., co2:nutrient.
lmfaov4 <- aov(data = bm9, lmf~co2*nutrient*n_nn*spp_id) #nutrient***, spp_id***, co2:nutrient*, co2:nutrient:spp_id***


rgrlmer1 <- lmer(data = bm9, rgr~co2*n_nn-1 + (1|spp_id), subset = bm9$rgr>0) #log trans?
rgrlmer2 <- lmer(data = bm9, rgr~nutrient*n_nn-1 + (1|spp_id), subset = bm9$rgr>0) #log trans?
rgrlmer3 <- lmer(data = bm9, rgr~co2*nutrient*n_nn-1 + (1|spp_id), subset = bm9$rgr>0) #log trans?
#
ldmclmer1 <- lmer(data = bm9, ldmc~co2*n_nn-1 + (1|spp_id))
ldmclmer2 <- lmer(data = bm9, ldmc~nutrient*n_nn-1 + (1|spp_id))
ldmclmer3 <- lmer(data = bm9, ldmc~co2*nutrient*n_nn-1 + (1|spp_id))
#
rmflmer1 <- lmer(data = bm9, rmf~co2*n_nn-1 + (1|spp_id))
rmflmer2 <- lmer(data = bm9, rmf~nutrient*n_nn-1 + (1|spp_id))
rmflmer3 <- lmer(data = bm9, rmf~co2*nutrient*n_nn-1 + (1|spp_id))
#
lmflmer1 <- lmer(data = bm9, lmf~co2*n_nn-1 + (1|spp_id))
lmflmer2 <- lmer(data = bm9, lmf~nutrient*n_nn-1 + (1|spp_id))
lmflmer3 <- lmer(data = bm9, lmf~co2*nutrient*n_nn-1 + (1|spp_id))

##not sure if these boxplots are helpful##
boxplot(data = bm9, log_rgr~nutrient*co2*n_nn)
boxplot(data = bm9, log_rgr~nutrient*co2*spp_id)
boxplot(data = bm9, log_rgr~nutrient*co2*spp_id*n_nn)
#
boxplot(data = bm9, ldmc~nutrient*co2*n_nn)
boxplot(data = bm9, ldmc~nutrient*co2*spp_id)
boxplot(data = bm9, ldmc~nutrient*co2*spp_id*n_nn)
#
boxplot(data = bm9, lmf~nutrient*co2*n_nn)
boxplot(data = bm9, lmf~nutrient*co2*spp_id)
boxplot(data = bm9, lmf~nutrient*co2*spp_id*n_nn)
#
boxplot(data = bm9, rmf~nutrient*co2*n_nn)
boxplot(data = bm9, rmf~nutrient*co2*spp_id)
boxplot(data = bm9, rmf~nutrient*co2*spp_id*n_nn)

#RGR nativity, fill co2
bm9 %>%
  ggplot(aes(x=n_nn, y=rgr, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("CO2 Response by Nativity") +
  xlab("Native/Non-Native")+
  ylab("RGR (g/g-1 d-1)")

#RGR nativity, fill nutrient
bm9 %>%
  ggplot(aes(x=n_nn, y=rgr, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Nutrient Response by Nativity") +
  xlab("Native/Non-Native")+
  ylab("RGR (g/g-1 d-1)")

#ldmc nativity, fill co2
bm9 %>%
  ggplot(aes(x=n_nn, y=ldmc, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("CO2 Response by Nativity") +
  xlab("Native/Non-Native")+
  ylab("LDMC (units)")

#ldmc nativity, fill nutrient
bm9 %>%
  ggplot(aes(x=n_nn, y=ldmc, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Nutrient Response by Nativity") +
  xlab("Native/Non-Native")+
  ylab("LDMC (units)")

#lmf nativity, fill co2
bm9 %>%
  ggplot(aes(x=n_nn, y=lmf, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("CO2 Response by Nativity") +
  xlab("Native/Non-Native")+
  ylab("LMF (units)")

#lmf nativity, fill nutrient
bm9 %>%
  ggplot(aes(x=n_nn, y=lmf, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Nutrient Response by Nativity") +
  xlab("Native/Non-Native")+
  ylab("LMF (units)")

#rmf nativity, fill nutrient
bm9 %>%
  ggplot(aes(x=n_nn, y=rmf, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("CO2 Response by Nativity") +
  xlab("Native/Non-Native")+
  ylab("RMF (units)")

#rmf nativity, fill co2
bm9 %>%
  ggplot(aes(x=n_nn, y=rmf, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Nutrient Response by Nativity") +
  xlab("Native/Non-Native")+
  ylab("RMF (units)")






#permanova tests?
#set seed for reproducability
rgr_permanova <- adonis(data = bm9, rgr~co2 + nutrient, method = "bray")
                        #+ (1|spp_id), subset = bm9$rgr>0) #can i log transform this? or do i have to take out the subset?
#
ldmc_permanova <- adonis(data = bm9, ldmc~co2*nutrient-1 + (1|spp_id))
#
lmf_permanova <- adonis(data = bm9, lmf~co2*nutrient-1 + (1|spp_id))
#
rmf_permanova <- adonis(data = bm9, rmf~co2*nutrient-1 + (1|spp_id))
