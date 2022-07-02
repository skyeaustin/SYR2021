
###Linear Mixed-Effects Models ---------------------------
##analysed with a Type III Anova (anova()function)##
rgr_lmer <- lmer(data = bm9, rgr~co2*nutrient*n_nn-1 + (1|spp_id), subset = bm9$rgr>0)
VarCorr(rgr_lmer)
     ##co2 *, nutrient ***

ldmc_lmer <- lmer(data = bm9, ldmc~co2*nutrient*n_nn-1 + (1|spp_id))
VarCorr(ldmc_lmer)
     ##co2 *

lmf_lmer <- lmer(data = bm9, lmf~co2*nutrient*n_nn-1 + (1|spp_id))
VarCorr(lmf_lmer)
     ##nothing is significant

rmf_lmer <- lmer(data = bm9, rmf~co2*nutrient*n_nn-1 + (1|spp_id))
VarCorr(rmf_lmer)
     ##co2 ***, nutrient *, co2:nutrient *

A_lmer <- lmer(data = licor6, A~co2*nutrient*n_nn-1 + (1|spp_id))
VarCorr(A_lmer) #this is weird, ask jason

gsw_lmer <- lmer(data = licor6, gsw~co2*nutrient*n_nn-1 + (1|spp_id))
VarCorr(gsw_lmer) #check notes for what this means?
     ##co2 and co2:nutrient trending with significance indicator (.)


###Plots created using ggplot### ------------------------------

#RGR by nativity, fill co2
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


#RGR by nativity, fill nutrient
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
