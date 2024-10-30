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
df_mod <- subset(df_af, df_af$VEG_GROUP=="Modified land")

sum(is.na(df_mod$fbi))
sum(is.na(df_mod$sfl))
sum(is.nan(df_mod$fbi))
sum(is.nan(df_mod$sfl))
sum(is.infinite(df_mod$fbi))
sum(is.infinite(df_mod$sfl))

df_mod <- df_mod[complete.cases(df_mod$fbi),]
###################################################
#create basic linear model (GLM)
mod_mod <- glm(FRP ~ fbi*sfl, data = df_mod)
ef_mod <- ggpredict(mod_mod, terms=c("fbi","sfl"))
plot(ef_mod, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_mod) #AIC = 58500, intercept, fbi, sfl signif, interaction not

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_mod)
#residuals vs fitted is good
#Q-Q has large upward curve
#scale-location has slight positive slope
#no influential points

#run descdist to find suggested family
descdist(df_mod$FRP)
#suggests beta and gamma

#create linear model with suggested family
#check for density distribution

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
ggplot(df_mod, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#transform response variable to be normally distributed if it can
ggplot(df_mod, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#almost normal, still a touch right skewed

#run descdist to find suggested family
descdist(log1p(df_mod$FRP))
#pretty close to gaussian


#create linear model with suggested family
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
mod_mod <- glm(log1p(FRP) ~ fbi*sfl, family = gaussian(link = "log"), data = df_mod, start = start_values)
ef_mod <- ggpredict(mod_mod, terms=c("fbi","sfl"))
plot(ef_mod, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_mod) #AIC = 13754, intercept, fbi, and sfl signif 

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
plot(mod_mod)
#pretty good, Q-Q has slight upward curve

#create transformed model
start_values <- c(Intercept = 1, fbi = 0, sfl = 0)
mod_mod <- glm(log1p(FRP) ~ fbi+sfl, family = gaussian(link = "log"), data = df_mod, start = start_values)
ef_mod <- ggpredict(mod_mod, terms=c("fbi","sfl"))
plot(ef_mod, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_mod) #AIC = 13752, all terms signif

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_mod)
#pretty good still, Q-Q still has slight upward curve

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_mod, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_mod, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_mod, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Mod", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#first model is borderline uniform, sfl has more effect than fbi. Modified land is probably quite variable. 
#second model is the same
#trying linear again, has more delineation of behaviour. 
#because "modified land could mean many things the model might be meaningless


###########################################
#create basic linear model (GAM)
gam_mod <- gam(FRP ~ fbi*sfl, data = df_mod)
ef_gam_mod <- ggpredict(gam_mod, terms = c("fbi","sfl"))
plot(ef_gam_mod, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (GCV scores)
summary(gam_mod)

#run diagnostic plots
par(mfrow=c(2,2))
gam.check(gam_mod)
#plot 1 has big curve
#2 is mostly good
#3 is right skewed
#4 is good

#run descdist to find suggested family
descdist(df_mod$FRP)
#beta/gamma

#density plot of response variable
ggplot(df_mod, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#transform response variable to be normally distributed
ggplot(df_mod, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#run descdist to find suggested family
descdist(log1p(df_mod$FRP))
#normal/uniform

#create linear model with suggested family
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
gam_mod <- gam(log1p(FRP) ~ fbi*sfl, gaussian(link = "log"), data = df_mod)
ef_gam_mod <- ggpredict(gam_mod, terms = c("fbi","sfl"))
plot(ef_gam_mod, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (GCV scores)
summary(gam_mod) #R-sq.(adj) =  0.0556   Deviance explained = 5.62%  GCV = 1.3819

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
gam.check(gam_mod)
#they look pretty good

#create transformed model
gam_mod <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3) + ti(fbi,sfl,k=3), gaussian(link = "log"), data = df_mod)
ef_gam_mod <- ggpredict(gam_mod, terms = c("fbi","sfl")) 
#R-sq.(adj) =  0.0612   Deviance explained = 6.22%  GCV = 1.3742
#all terms signif except t int fbi:sfl

gam_mod <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3), gaussian(link = "log"), data = df_mod)
ef_gam_mod <- ggpredict(gam_mod, terms = c("fbi","sfl"))
#R-sq.(adj) =  0.0614   Deviance explained = 6.22%  GCV = 1.3736 
#all terms signif

gam_mod <- gam(log1p(FRP) ~ s(fbi,k=3), gaussian(link = "log"), data = df_mod)
ef_gam_mod <- ggpredict(gam_mod, terms = c("fbi"))
#R-sq.(adj) =  0.00247   Deviance explained = 0.291%  GCV = 1.4592
#fbi is signif

gam_mod <- gam(log1p(FRP) ~ s(sfl,k=3), gaussian(link = "log"), data = df_mod)
ef_gam_mod <- ggpredict(gam_mod, terms = c("sfl"))
#R-sq.(adj) =  0.0574   Deviance explained = 5.79%  GCV = 1.3789
#sfl is signif

#summary (GCV scores)
plot(ef_gam_mod, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_mod)

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
gam.check(gam_mod)


#visually inspect model against scatter 
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_gam_mod, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_mod, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_mod, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Modified Land", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 50)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#choose GAM with fbi + sfl and no interaction because simplest model with all signif terms and same** AIC
#GLM with no interaction also good condidate for same reason
#GAM just captures higher FRP behaviour at FBI > 40 which is to be expected
