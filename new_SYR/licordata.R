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
library(readxl)

#set wd#
setwd("~/Documents/r_stuff/SYR2021-1/new_SYR") #skye's mac

r1_200 <- read_excel("/Users/saus/Documents/SYR/datadata/licor/licor_dat_fixed/r1_200.xlsx")
r1_400 <- read_excel("/Users/saus/Documents/SYR/datadata/licor/licor_dat_fixed/r1_400.xlsx")
r2_200 <- read_excel("/Users/saus/Documents/SYR/datadata/licor/licor_dat_fixed/r2_200.xlsx")
r2_400 <- read_excel("/Users/saus/Documents/SYR/datadata/licor/licor_dat_fixed/r2_400.xlsx")
r3_200 <- read_excel("/Users/saus/Documents/SYR/datadata/licor/licor_dat_fixed/r3_200.xlsx")
r3_400 <- read_excel("/Users/saus/Documents/SYR/datadata/licor/licor_dat_fixed/r3_400.xlsx")









