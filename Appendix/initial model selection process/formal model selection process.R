#formal model selection process
library(terra)
library(lubridate)
library(tidyverse)
library(quantreg)
library(ggeffects)
library(viridis)
library(mgcv)
library(fitdistrplus)

setwd('C:/Users/zmajor/OneDrive - University of Tasmania/zac')
df_af <- read.csv('afdr_dataframe2.csv')
df_dryeuc <- subset(df_af, df_af$VEG_GROUP=="Dry eucalypt forest and woodland")

sum(is.na(df_dryeuc$fbi))
sum(is.na(df_dryeuc$sfl))
sum(is.nan(df_dryeuc$fbi))
sum(is.nan(df_dryeuc$sfl))
sum(is.infinite(df_dryeuc$fbi))
sum(is.infinite(df_dryeuc$sfl))

df_dryeuc <- df_dryeuc[complete.cases(df_dryeuc$fbi),]
###################################################
#create basic linear model (GLM)

#summary (AIC scores)

#run diagnostic plots

#run descdist to find suggested family



#create linear model with suggested family

#summary (AIC scores)

#run diagnostic plots to find suggested transform



#create transformed model

#summary (AIC scores)

#run diagnostic plots to assess fit



#visually inspect model against scatter

###########################################
#create basic linear model (GLM)

#summary (AIC scores)

#run diagnostic plots

#run descdist to find suggested family

#density plot of response variable

#transform response variable to be normally distributed if it can

#run descdist to find suggested family



#create linear model with suggested family

#summary (AIC scores)

#run diagnostic plots to find suggested transform



#create transformed model

#summary (AIC scores)

#run diagnostic plots to assess fit



#visually inspect model against scatter

###########################################
#create basic linear model (GAM)

#summary (GCV scores)

#run diagnostic plots

#run descdist to find suggested family

#density plot of response variable

#transform response variable to be normally distributed

#run descdist to find suggested family



#create linear model with suggested family

#summary (GCV scores)

#run diagnostic plots to find suggested transform



#create transformed model

#summary (GCV scores)

#run diagnostic plots to assess fit



#visually inspect model against scatter 