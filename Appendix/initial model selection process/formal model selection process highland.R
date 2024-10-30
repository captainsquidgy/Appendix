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
df_high <- subset(df_af, df_af$VEG_GROUP=="Highland and treeless vegetation")

sum(is.na(df_high$fbi))
sum(is.na(df_high$sfl))
sum(is.na(df_high$FRP_2))
sum(is.nan(df_high$fbi))
sum(is.nan(df_high$sfl))
sum(is.nan(df_high$FRP_2))
sum(is.infinite(df_sum$fbi))
sum(is.infinite(df_high$sfl))
sum(is.infinite(df_high$FRP_2))


df_high <- df_high[complete.cases(df_high$fbi),]
###################################################
#create basic linear model (GLM)
mod_high <- glm(FRP ~ fbi*sfl, data = df_high)
ef_high <- ggpredict(mod_high, terms=c("fbi","sfl"))
plot(ef_high, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_high) #AIC = 2253, no signif terms

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_high)
#resid vs fitted
#Q-Q has large upward curve
#scale-location has slight positive slope but is ok
#one influential point, 14429

#run descdist to find suggested family
descdist(df_high$FRP)
#beta/gamma

#create linear model with suggested family
mod_high <- glm(FRP ~ fbi*sfl, family = Gamma(link="log"), data = df_high)
ef_high <- ggpredict(mod_high, terms=c("fbi","sfl"))
plot(ef_high, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_high) #AIC=1993, only intercept signif

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
plot(mod_high)
#diagnostics look better
#Q-Q has slight downward curve, no influential points

#create transformed model
df_high$FRP_2 <- df_high$FRP^2
df_high <- df_high[df_high$FRP > 0,]

mod_high <- glm(FRP ~ fbi+sfl, family = Gamma(link="log"), data = df_high)
ef_high <- ggpredict(mod_high, terms=c("fbi","sfl"))
plot(ef_high, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#standardise predictors because large values could be causing numerical instability
df_high$fbi_std <- scale (df_high$fbi)
df_high$sfl_std <- scale (df_high$sfl)
#still getting error, gamma could be poor approx

#summary (AIC scores)

#run diagnostic plots to assess fit



#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_high, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_high, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_high, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")
###########################################
#density plot of response
ggplot(df_high, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#right skew, log trans

ggplot(df_high, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#normal

#create basic linear model (GLM)
mod_high <- glm(log1p(FRP) ~ fbi*sfl, family = gaussian(link="log"), data = df_high)
ef_high <- ggpredict(mod_high, terms=c("fbi","sfl"))
plot(ef_high, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_high) # AIC = 524, only intercept signif

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_high)
#they are great

#run descdist to find suggested family
par(mfrow=c(1,1))
descdist(log1p(df_high$FRP))
#suggests uniform first and normal/gauss second

#density plot of response variable
#we know that its normal/uniform big blob

#transform response variable to be normally distributed if it can



#run descdist to find suggested family


#create linear model with suggested family
mod_high <- glm(log1p(FRP) ~ fbi*sfl, family = gaussian(link="identity"), data = df_high)
ef_high <- ggpredict(mod_high, terms=c("fbi","sfl"))
plot(ef_high, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_high) # AIC = 524, only intercept signif


#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
plot(mod_high)
#still good, lets take some terms out

#create transformed model
mod_high <- glm(log1p(FRP) ~ fbi+sfl, family = gaussian(link="identity"), data = df_high)
ef_high <- ggpredict(mod_high, terms=c("fbi","sfl"))
plot(ef_high, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_high) # AIC = 525, fbi signif now

#create transformed model
mod_high <- glm(log1p(FRP) ~ fbi, family = gaussian(link="identity"), data = df_high)
ef_high <- ggpredict(mod_high, terms=c("fbi"))
plot(ef_high, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_high) #AIC = 523, all terms signif

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_high)
#nice


#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_high, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_high, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_high, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#could probably subset for below 30

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