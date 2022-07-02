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

#read csv
licor1 <- read.csv("/Users/saus/Documents/SYR/datadata/licor/licor_final.csv", check.names = F)

licor2 <- licor1 %>% 
  mutate(spp_id=substring(pot_id, 8, 9))
licor3 <- licor2 %>% 
  mutate(co2=substring(pot_id, 11, 13))
licor4 <- licor3 %>% 
  mutate(nutrient=substring(pot_id, 15))
#take out low values
licor5 <- licor4 %>% 
  filter(licor4$A >= 3)

licor6 <- licor5 %>% 
  mutate(n_nn = if_else(licor5$spp_id %in% c("AA", "PR"), "native", "non-native"))


A_lmer <- lmer(data = licor5, A~co2*nutrient-1 + (1|spp_id))
gsw_lmer <- lmer(data = licor5, gsw~co2*nutrient-1 + (1|spp_id))
rgraov1 <- aov(data = bm9, log_rgr~co2*nutrient)

test <- lmer(data = licor_nnn, gsw~co2*nutrient*nativity-1 + (1|spp_id))
test2 <- lmer(data = licor_nnn, A~co2*nutrient*nativity-1 + (1|spp_id))


#run nativity
rgr_lmer <- lmer(data = bm9, rgr~co2*nutrient*n_nn-1 + (1|spp_id), subset = bm9$rgr>0)


#A by co2, fill nutrient
licor5 %>%
  ggplot(aes(x=co2, y=A, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Photosynthetic Rate (A) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("A (units)")

#gsw by co2, fill nutrient
licor5 %>%
  ggplot(aes(x=co2, y=gsw, fill=nutrient)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Stomatal Conductance (gsw) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("gsw (units)")

#add nativity column
licor_nnn <- licor5 %>% 
  mutate(nativity = if_else(licor5$spp_id %in% c("AA", "PR"), "native", "non-native"))


#A by co2, fill nativity
licor_nnn %>%
  ggplot(aes(x=co2, y=A, fill=nativity)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Photosynthetic Rate (A) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("A (units)")

#A by nutrient, fill nativity
licor_nnn %>%
  ggplot(aes(x=nutrient, y=A, fill=nativity)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Photosynthetic Rate (A) by Nutrient Level") +
  xlab("Nutrient Level")+
  ylab("A (units)")

#gsw by co2, fill nativity
licor_nnn %>%
  ggplot(aes(x=co2, y=gsw, fill=nativity)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Stomatal Conductance (gsw) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("gsw (units)")


#gsw by nutrient, fill nativity
licor_nnn %>%
  ggplot(aes(x=nutrient, y=gsw, fill=nativity)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Stomatal Conductance (gsw) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("gsw (units)")

gsw_lmer2 <- lmer(data = licor_nnn, gsw~co2*nutrient*nativity-1 + (1|spp_id))
A_lmer <- lmer(data = licor_nnn, A~co2*nutrient*nativity-1 + (1|spp_id))


licor_nnn %>%
  ggplot(aes(x=spp_id, y=gsw, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Stomatal Conductance (gsw) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("gsw (units)")


licor_nnn %>%
  ggplot(aes(x=spp_id, y=A, fill=co2)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#653371", "#76C19A"))+  
  geom_jitter(color="black", size=0.4, alpha=0.9, position = position_jitter(seed = 1)) +
  theme(
    plot.title = element_text(size=11)
  ) +
  ggtitle("Photosynthetic Rate (A) by CO2 Level") +
  xlab("CO2 (ppm)")+
  ylab("A (units)")

#lmer(data = licor_nnn, gsw~co2*nutrient*nativity-1 + (1|spp_id))
#Note you had the (built-in) regression tables as output for these, 
#but a better option is the anova table from the package 'lmerTest', 
#using its 'anova' function (see p. 5 here). 
