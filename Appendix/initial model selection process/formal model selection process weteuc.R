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
df_weteuc <- subset(df_af, df_af$VEG_GROUP=="Wet eucalypt forest and woodland")

sum(is.na(df_weteuc$fbi))
sum(is.na(df_weteuc$sfl))
sum(is.nan(df_weteuc$fbi))
sum(is.nan(df_weteuc$sfl))
sum(is.infinite(df_weteuc$fbi))
sum(is.infinite(df_weteuc$sfl))

df_weteuc <- df_weteuc[complete.cases(df_weteuc$fbi),]
###################################################
#create basic linear model (GLM)
mod_weteuc <- glm(FRP ~ fbi*sfl, data = df_weteuc)
ef_weteuc <- ggpredict(mod_weteuc, terms=c("fbi","sfl"))
plot(ef_weteuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_weteuc) #int signif, no else AIC = 40756

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_weteuc)
#resid vs fit is good
#Q-Q has upward curve
#scale-location is good
#no influential points but 5137 is close

#run descdist to find suggested family
descdist(df_weteuc$FRP)
#experience says we should check density to not waste time

#create linear model with suggested family

#summary (AIC scores)

#run diagnostic plots to find suggested transform



#create transformed model

#summary (AIC scores)

#run diagnostic plots to assess fit



#visually inspect model against scatter

###########################################
#create basic linear model (GLM)
mod_weteuc <- glm(FRP ~ fbi*sfl, data = df_weteuc)
ef_weteuc <- ggpredict(mod_weteuc, terms=c("fbi","sfl"))
plot(ef_weteuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_weteuc) #int signif, no else AIC = 40756

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_weteuc)
#resid vs fit is good
#Q-Q has upward curve
#scale-location is good
#no influential points but 5137 is close

#run descdist to find suggested family
descdist(df_weteuc$FRP)
#experience says we should check density to not waste time

#density plot of response variable
ggplot(df_weteuc, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#right skewed again

#transform response variable to be normally distributed if it can
ggplot(df_weteuc, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#looks normal

#run descdist to find suggested family
descdist(log1p(df_weteuc$FRP))
#gaussian family, no question

#create linear model with suggested family
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
mod_weteuc <- glm(log1p(FRP) ~ fbi*sfl, family = gaussian(link = "log"), data = df_weteuc, start =  start_values)
ef_weteuc <- ggpredict(mod_weteuc, terms=c("fbi","sfl"))
plot(ef_weteuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_weteuc) #AIC = 9453, intercept is signif, nothing else

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
plot(mod_weteuc)
#perfect

#create transformed model
start_values <- c(Intercept = 1, fbi = 0, sfl = 0)
mod_weteuc <- glm(log1p(FRP) ~ fbi+sfl, family = gaussian(link = "log"), data = df_weteuc, start =  start_values)
ef_weteuc <- ggpredict(mod_weteuc, terms=c("fbi","sfl"))
plot(ef_weteuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_weteuc) #AIC = 9451, intercept and fbi is signif

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_weteuc)
#great

#create transformed model
start_values <- c(Intercept = 1, fbi = 0)
mod_weteuc <- glm(log1p(FRP) ~ fbi, family = gaussian(link = "log"), data = df_weteuc, start =  start_values)
ef_weteuc <- ggpredict(mod_weteuc, terms=c("fbi"))
plot(ef_weteuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_weteuc) #AIC = 9450, intercept and fbi is signif, no insignif terms

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_weteuc)
#great again

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_weteuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_weteuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_weteuc, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#on first look there is only a small variance in fuel load, still a noticable amount. sfl is accounted for in fbi but not entirely, no significant terms though
#try with no interaction term, doesnt make sense that sfl is additive though
#try with only fbi as predictor, implies that sfl is included in calculation of fbi

#select log1p(FRP) ~ fbi because sfl is accounted for in fbi and should not be additive, AIC also better slightly
#GAM and GLM are effectively the same, choose simpler model => GLM


###########################################
#create basic linear model (GAM)
gam_weteuc <- gam(FRP ~ fbi*sfl, data = df_weteuc)
ef_gam_weteuc <- ggpredict(gam_weteuc, terms = c("fbi","sfl"))
plot(ef_gam_weteuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (GCV scores)
summary(gam_weteuc) #R-sq.(adj) =  0.000585   Deviance explained = 0.164%  GCV =  97437

#run diagnostic plots
gam.check(gam_weteuc)

#run descdist to find suggested family
descdist(df_weteuc$FRP)
#we already know what this looks like

#density plot of response variable

#transform response variable to be normally distributed

#run descdist to find suggested family



#create linear model with suggested family
gam_weteuc <- gam(log1p(FRP) ~ fbi*sfl, gaussian(link = "log"), data = df_weteuc)
ef_gam_weteuc <- ggpredict(gam_weteuc, terms = c("fbi","sfl"))
plot(ef_gam_weteuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#summary (GCV scores)
summary(gam_weteuc) #R-sq.(adj) =  0.00582   Deviance explained = 0.686%  GCV =  1.623

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
gam.check(gam_weteuc)
#residuals look good


#create transformed model
gam_weteuc <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3) + ti(fbi,sfl,k=3), gaussian(link = "log"), data = df_weteuc)
#R-sq.(adj) =  0.00872   Deviance explained = 1.03%  GCV = 1.6192
#fbi and interaction signif
#mixing sfl, needs less splines
#wont let me do less

gam_weteuc <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3), gaussian(link = "log"), data = df_weteuc)
#R-sq.(adj) =  0.00663   Deviance explained = 0.753%  GCV = 1.6215
#fbi signif

gam_weteuc <- gam(log1p(FRP) ~ s(fbi,k=3), gaussian(link = "log"), data = df_weteuc)
#R-sq.(adj) =  0.00634   Deviance explained = 0.669%  GCV =  1.621 
#fbi signif

gam_weteuc <- gam(log1p(FRP) ~ ti(fbi,sfl,k=3), gaussian(link = "log"), data = df_weteuc)
#R-sq.(adj) =  0.00208   Deviance explained = 0.275%  GCV = 1.6285
#interaction is signif

#summary (GCV scores)
ef_gam_weteuc <- ggpredict(gam_weteuc, terms = c("fbi","sfl"))
plot(ef_gam_weteuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_weteuc)

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
gam.check(gam_weteuc)
#they all seem good


#visually inspect model against scatter 
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_gam_weteuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_weteuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_weteuc, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Grass", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 50)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")
