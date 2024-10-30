#formal model selection process (DRYEUC)
library(terra)
library(lubridate)
library(tidyverse)
library(quantreg)
library(ggeffects)
library(viridis)
library(mgcv)
library(fitdistrplus)
library("AICcmodavg")

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
mod_dryeuc <- glm(FRP ~ fbi*sfl, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_dryeuc) #60952

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_dryeuc)
#residuals vs fitted looks good
#Q-Q shows clear upward curve, bad
#scale-location has slight positive slope but visually spread is ok
#no infleuntial points

#run descdist to find suggested family
descdist(df_dryeuc$FRP)
#suggests gamma

#create linear model with suggested family
df_dryeuc <- df_dryeuc[df_dryeuc$FRP > 0, ]
mod_dryeuc <- glm(FRP ~ fbi*sfl, family = Gamma(link="log"), data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_dryeuc) #48601

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
plot(mod_dryeuc)
#residuals vs fitted looks good
#Q-Q still has upward curve
#scale-location looks good
#no influential points

#create transformed model
mod_dryeuc <- glm(log1p(FRP) ~ fbi*sfl, family = Gamma(link="log"), data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_dryeuc) #13715

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_dryeuc)
#all good except
#Q-Q has slight downward curve

#create transformed model
mod_dryeuc <- glm(sqrt(FRP) ~ fbi*sfl, family = Gamma(link="log"), data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_dryeuc) #25179

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_dryeuc)
#all good except
#Q-Q has slight upward curve

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_dryeuc, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


###########################################
#create basic linear model (GLM)
mod_dryeuc <- glm(FRP ~ fbi*sfl, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_dryeuc) #60891

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_dryeuc)
#residuals vs fitted is good
#Q-Q has significant upward curve
#scale-location has clear positive slope
#no influential points

#run descdist to find suggested family
descdist(df_dryeuc$FRP)
#suggests gamma

#density distribution of response variable
ggplot(df_dryeuc, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#its right skewed 

#transform response variable to be normally distributed if it can
ggplot(df_dryeuc, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#looks normal now

#run descdist to find suggested family
descdist(log1p(df_dryeuc$FRP))
#gaussian family suggested

#create linear model with suggested family
mod_dryeuc <- glm(log1p(FRP) ~ fbi*sfl, family = gaussian(link = "log"), data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_dryeuc) #14110

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
plot(mod_dryeuc)
#residuals look good

#create transformed model
mod_dryeuc <- glm(sqrt(FRP) ~ fbi*sfl, family = gaussian(link = "log"), data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#summary (AIC scores)
summary(mod_dryeuc) #28668

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_dryeuc)
#scale-location and Q-Q looks bad

#visually inspect model against scatter
mod_dryeuc <- glm(log1p(FRP) ~ fbi*sfl, family = gaussian(link = "log"), data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

mod_dryeuc <- glm(sqrt(FRP) ~ fbi*sfl, family = gaussian(link = "log"), data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

#visually inspect less terms
mod_dryeuc <- glm(log1p(FRP) ~ fbi+sfl, family = gaussian(link = "log"), data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

mod_dryeuc <- glm(sqrt(FRP) ~ fbi+sfl, family = gaussian(link = "log"), data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_dryeuc, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

###########################################
#create basic linear model (GAM)
gam_dryeuc <- gam(FRP ~ fbi*sfl, data = df_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (GCV scores)
summary(gam_dryeuc) #R-sq.(adj) =  0.0431   Deviance explained = 4.38%   GCV =  60255 

#run diagnostic plots
gam.check(gam_dryeuc)

#run descdist to find suggested family
descdist(df_dryeuc$FRP)

#density plot of response variable
ggplot(df_dryeuc, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#transform response variable to be normally distributed
gam_dryeuc <- gam(log1p(FRP) ~ fbi*sfl, data = df_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#run descdist to find suggested family
descdist(log1p(df_dryeuc$FRP))
#normal, gaussian

#create linear model with suggested family
gam_dryeuc <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3) + ti(fbi,sfl,k=3), family = gaussian(link = "log"), data = df_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
crop <- subset(ef_gam_dryeuc, ef_gam_dryeuc$x <50)
plot(crop, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))


#summary (GCV scores)
summary(gam_dryeuc) #R-sq.(adj) =  0.0549   Deviance explained = 5.57%    GCV = 1.4491

#run diagnostic plots to find suggested transform
gam.check(gam_dryeuc)
#diagnostics look good

#create transformed model
gam_dryeuc <- gam(sqrt(FRP) ~ s(fbi,k=3) + s(sfl,k=3) + ti(fbi,sfl,k=3), family = gaussian(link = "log"), data = df_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
crop <- subset(ef_gam_dryeuc, ef_gam_dryeuc$x <50)
plot(crop, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (GCV scores)
summary(gam_dryeuc) # R-sq.(adj) =  0.0626   Deviance explained = 6.33%   GCV = 39.769

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
gam.check(gam_dryeuc)
#bit more curve in residual plot, otherwise good

#visually inspect model against scatter 
gam_dryeuc <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3) + ti(fbi,sfl,k=3), family = gaussian(link = "log"), data = df_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
plot(crop, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc) #R-sq.(adj) =  0.0549   Deviance explained = 5.57%  GCV = 1.4491

gam_dryeuc <- gam(sqrt(FRP) ~ s(fbi,k=3) + s(sfl,k=3) + ti(fbi,sfl,k=3), family = gaussian(link = "log"), data = df_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
plot(crop, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc) # R-sq.(adj) =  0.0626   Deviance explained = 6.33%   GCV = 39.769

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_gam_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_dryeuc, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#candidates are
#GAM, log(FRP)~fbi sfl, gauss(log) #R-sq.(adj) =  0.0549   Deviance explained = 5.57%  GCV = 1.4491
#GLM, log(FRP)~fbi sfl, gauss(log) AIC: 14110
#GAM, sqrt(FRP)~fbi sfl, gauss(log) #R-sq.(adj) =  0.0626   Deviance explained = 6.33%   GCV = 39.769
#GLM, sqrt(FRP)~fbi sfl, gauss(log) AIC: 28668

#for log models there is little difference in predicted FRP between 0 and 25 FBI. This does not make much physical sense
#GLM log model has best residuals
#GAM log model also has best residuals
#sqrt has change in predicted FRP between 0 and 25, GLM and GAM almost the same
#GAM sqrt has curve in residuals (not good)
#GLM sqrt has curve in Q-Q residuals (not good)

#they look the same, selection is GLM sqrt for simplicity

