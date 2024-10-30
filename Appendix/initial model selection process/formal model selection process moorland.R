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
df_moor <- subset(df_af, df_af$VEG_GROUP=="Moorland, sedgeland and rushland")

sum(is.na(df_moor$fbi))
sum(is.na(df_moor$sfl))
sum(is.nan(df_moor$fbi))
sum(is.nan(df_moor$sfl))
sum(is.infinite(df_moor$fbi))
sum(is.infinite(df_moor$sfl))

df_moor <- df_moor[complete.cases(df_moor$fbi),]
###################################################
#create basic linear model (GLM)
mod_moor <- glm(FRP ~ fbi*sfl, data = df_moor)
ef_moor <- ggpredict(mod_moor, terms=c("fbi","sfl"))
plot(ef_moor, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_moor) #AIC = 13719, intercept and fbi:sfl signif

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_moor)
#resid vs fitted is good
#Q-Q has large upward curve
#scale-location has clear positive slope
#no influential points

#run descdist to find suggested family
descdist(df_moor$FRP)
#beta/gamma

####### check distribution'
ggplot(df_moor, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#large right skew => log transform
####### check distribution'
ggplot(df_moor, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#nice apporximately normal distribution

descdist(log1p(df_moor$FRP))
#normal


#create linear model with suggested family
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
mod_moor <- glm(log1p(FRP) ~ fbi*sfl , family = gaussian(link = "log"), data = df_moor, start = start_values)
ef_moor <- ggpredict(mod_moor, terms=c("fbi","sfl"))
plot(ef_moor, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_moor) # AIC = 3092, intercept and fbi:sfl signif

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
plot(mod_moor)
#resid vs fitted is good, 
#Q-Q is good
#scale-location is ok but with slight kink up
#no influential points


#create transformed model
start_values <- c(Intercept = 1, fbi = 0 )
mod_moor <- glm(log1p(FRP) ~ fbi, family = gaussian(link = "log"), data = df_moor, start = start_values)
ef_moor <- ggpredict(mod_moor, terms=c("fbi"))
plot(ef_moor, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#summary (AIC scores) 
summary(mod_moor) # AIC = 3094, all terms signif

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_moor)
#they are all good

#another transfromed model
start_values <- c(Intercept = 1, sfl = 0 )
mod_moor <- glm(log1p(FRP) ~ sfl, family = gaussian(link = "log"), data = df_moor, start = start_values)
ef_moor <- ggpredict(mod_moor, terms=c("sfl"))
plot(ef_moor, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_moor, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_moor, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_moor, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Mod", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


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
gam_moor <- gam(FRP ~ fbi*sfl, data = df_moor)
ef_gam_moor <- ggpredict(gam_moor, terms = c("fbi","sfl"))
plot(ef_gam_moor, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (GCV scores)
summary(gam_moor) # R-sq.(adj) =  0.0241   Deviance explained = 2.73%  GCV = 2.4789e+05

#run diagnostic plots
par(mfrow=c(2,2))
gam.check(gam_moor)
#1 has clear curve
#2 is good
#3 is right skewed
#4 is good

#run descdist to find suggested family
#know from before that it suggests normal with log trans

#density plot of response variable

#transform response variable to be normally distributed

#run descdist to find suggested family



#create linear model with suggested family
gam_moor <- gam(log1p(FRP) ~ fbi*sfl, family = gaussian(link = "log"), data = df_moor)
ef_gam_moor <- ggpredict(gam_moor, terms = c("fbi","sfl"))
plot(ef_gam_moor, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))


#summary (GCV scores)
summary(gam_moor) # R-sq.(adj) =  0.0207   Deviance explained =  2.4%  GCV = 1.8225

#run diagnostic plots to find suggested transform
gam.check(gam_moor)
#they look good

#create transformed model
gam_moor <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3) + ti(fbi,sfl,k=3), family = gaussian(link = "log"), data = df_moor)
ef_gam_moor <- ggpredict(gam_moor, terms = c("fbi","sfl"))
#R-sq.(adj) =  0.0261   Deviance explained = 3.06%  GCV = 1.814
#fbi and fbi:sfl signif 
#weird mixing in visual

gam_moor <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3), family = gaussian(link = "log"), data = df_moor)
ef_gam_moor <- ggpredict(gam_moor, terms = c("fbi","sfl"))
#R-sq.(adj) =  0.0188   Deviance explained = 2.17%  GCV = 1.8253
#fuel loads predict almost the same and plateau for FBI > 25

gam_moor <- gam(log1p(FRP) ~ s(fbi,k=3), family = gaussian(link = "log"), data = df_moor)
ef_gam_moor <- ggpredict(gam_moor, terms = c("fbi"))
#R-sq.(adj) =  0.0193   Deviance explained = 2.11%  GCV = 1.8224
#same plateau as above

gam_moor <- gam(log1p(FRP) ~ s(sfl,k=3), family = gaussian(link = "log"), data = df_moor)
ef_gam_moor <- ggpredict(gam_moor, terms = c("sfl"))
#R-sq.(adj) =  -3.54e-06   Deviance explained = 0.111%  GCV = 1.8569
#clear bow but linear relationship

gam_moor <- gam(log1p(FRP) ~ ti(fbi,sfl,k=3), family = gaussian(link = "log"), data = df_moor)
ef_gam_moor <- ggpredict(gam_moor, terms = c("fbi","sfl"))


#summary (GCV scores)
plot(ef_gam_moor, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_moor) 


#run diagnostic plots to assess fit
par(mfrow=c(2,2))
gam.check(gam_moor)



#visually inspect model against scatter 
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_gam_moor, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_moor, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_moor, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "moorland", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 35), breaks = seq(0, 35, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 50)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


#select GLM fbi*sfl because evem though not all terms are signif, it has best AIC, makes the most physical sense, and most other models are terrible
#GAM fbi + sfl with no int is also candidate but plateau is strange. Are moorland fires really smoky because of fire activity when there is still lots of moisture?

