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
df_scrub <- subset(df_af, df_af$VEG_GROUP=="Scrub, heathland and coastal complexes")

sum(is.na(df_scrub$fbi))
sum(is.na(df_scrub$sfl))
sum(is.nan(df_scrub$fbi))
sum(is.nan(df_scrub$sfl))
sum(is.infinite(df_scrub$fbi))
sum(is.infinite(df_scrub$sfl))

df_scrub <- df_scrub[complete.cases(df_scrub$fbi),]
###################################################
#create basic linear model (GLM)
mod_scrub <- glm(FRP ~ fbi*sfl, data = df_scrub)
ef_scrub <- ggpredict(mod_scrub, terms=c("fbi","sfl"))
plot(ef_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_scrub) #AIC = 12011, only intercept signif

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_scrub)
#resid vs fitted looks good
#Q-Q has large defined curve and then linear positive slope
#scale location is good
#no influential points


#run descdist to find suggested family
descdist(df_scrub$FRP)
#beta/gamma

#check density distribution
ggplot(df_scrub, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#large right skew => log transform
ggplot(df_scrub, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#almost normal

descdist(log1p(df_scrub$FRP))
#closer to logistic regression, 2nd is gaussian
#binomial doesnt make sense, but we will look at the data

#create linear model with suggested family
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
mod_scrub <- glm(log1p(FRP) ~ fbi*sfl, family = gaussian(link = "log"), data = df_scrub, start = start_values)
ef_scrub <- ggpredict(mod_scrub, terms=c("fbi","sfl"))
plot(ef_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_scrub) # AIC = 3009.5, only intercept significant

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
plot(mod_scrub)
#1,3,4 good
#Q-Q has big kink before coming back to line
#should probably look at data 
ggplot() +
  geom_jitter(data = df_scrub, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Scrub", x = "FBI", y = "FRP") +
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 250)) + # Customize y-axis
  theme_minimal() +
  theme(legend.position = "none")
#there is not much pattern
ggplot(df_scrub, aes(x=fbi)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FBI", x = "FBI", y = "Density")

#create transformed model
df_scrub0 <- df_scrub[df_scrub$FRP > 0, ]
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
mod_scrub <- glm(log1p(FRP) ~ fbi*sfl, family = Gamma(link = "log"), data = df_scrub0, start = start_values)
ef_scrub <- ggpredict(mod_scrub, terms=c("fbi","sfl"))
plot(ef_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#summary (AIC scores)
summary(mod_scrub) # AIC = 2624, only intercept signif

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_scrub)
#all good except Q-Q, downward curve but not huge, use sqrt transform instead

#create transformed model
df_scrub0 <- df_scrub[df_scrub$FRP > 0, ]
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
mod_scrub <- glm(sqrt(FRP) ~ fbi*sfl, family = Gamma(link = "log"), data = df_scrub0, start = start_values)
ef_scrub <- ggpredict(mod_scrub, terms=c("fbi","sfl"))
plot(ef_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_scrub) # AIC = 5167, only intercept signif

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_scrub)
#all good 
#lets take a few terms out, sfl is good candidate
start_values <- c(Intercept = 1, fbi = 0)
mod_scrub <- glm(sqrt(FRP) ~ fbi, family = Gamma(link = "log"), data = df_scrub0, start = start_values)
ef_scrub <- ggpredict(mod_scrub, terms=c("fbi"))
plot(ef_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_scrub) 
#sfl not signif, remove it
#all terms signif now

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_scrub, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_scrub, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_scrub, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Scrub", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#out of the GLM's select sqrt transform gamma with only fbi term
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
gam_scrub <- gam(FRP ~ fbi*sfl, data = df_scrub)
ef_gam_scrub <- ggpredict(gam_scrub, terms = c("fbi","sfl"))
plot(ef_gam_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#summary (GCV scores)
summary(gam_scrub) #R-sq.(adj) =  0.000674   Deviance explained = 0.422%  GCV =  8418
#only intercept is significant

#run diagnostic plots
gam.check(gam_scrub)
#1 has curve
#2 is fine
#3 has right skew
#4 is fine


#run descdist to find suggested family
descdist(df_scrub$FRP)
#same as before it needs transform

#density plot of response variable
ggplot(df_scrub, aes(x=sqrt(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#still has heavy right tail skew

#transform response variable to be normally distributed

#run descdist to find suggested family
descdist(sqrt(df_scrub$FRP))
#gamma/exponential


#create linear model with suggested family
gam_scrub <- gam(FRP ~ fbi*sfl, family = Gamma("log"), data = df_scrub0)
ef_gam_scrub <- ggpredict(gam_scrub, terms = c("fbi","sfl"))
plot(ef_gam_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (GCV scores)
summary(gam_scrub) # R-sq.(adj) =  0.00139   Deviance explained = 0.946%  GCV = 1.6359
#only intercept is signif

#run diagnostic plots to find suggested transform
gam.check(gam_scrub)
#1 has clear curve
#2 is good
#3 is slightly right skewed
#4 is good

#create transformed model
gam_scrub <- gam(sqrt(FRP) ~ fbi*sfl, family = Gamma("log"), data = df_scrub0)
ef_gam_scrub <- ggpredict(gam_scrub, terms = c("fbi","sfl"))
plot(ef_gam_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#no interaction, FBi is now signif. R-sq.(adj) =  0.00757   Deviance explained = 1.26%  GCV = 0.40545
gam_scrub <- gam(sqrt(FRP) ~ fbi+sfl, family = Gamma("log"), data = df_scrub0)
ef_gam_scrub <- ggpredict(gam_scrub, terms = c("fbi","sfl"))
plot(ef_gam_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#only fbi, all terms signif. R-sq.(adj) =  0.00826   Deviance explained = 1.17%  GCV = 0.40482  
gam_scrub <- gam(sqrt(FRP) ~ fbi, family = Gamma("log"), data = df_scrub0)
ef_gam_scrub <- ggpredict(gam_scrub, terms = c("fbi"))
plot(ef_gam_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

gam_scrub <- gam(sqrt(FRP) ~ s(fbi,k=3) + s(sfl,k=3), family = Gamma("log"), data = df_scrub0)
ef_gam_scrub <- ggpredict(gam_scrub, terms = c("fbi","sfl"))
plot(ef_gam_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#take ti out
#mixing looks shit, change it back

#summary (GCV scores)
summary(gam_scrub) # R-sq.(adj) =  0.00647   Deviance explained = 1.27%  GCV = 0.40643
#only intercept is signif

#run diagnostic plots to assess fit
gam.check(gam_scrub)
#still slight curve in 1 but apart from thta all good
#take some terms out


#visually inspect model against scatter 
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_gam_scrub, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_scrub, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_scrub0, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "scrub", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 35), breaks = seq(0, 35, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#out of GAM select the sqrt fbi gamma as other models are overfitted and no significant effect from sfl
#actually the smooth fit with no interaction is better, try with no smooth sfl and sfl interaction