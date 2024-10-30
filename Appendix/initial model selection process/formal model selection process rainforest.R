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
df_rain <- subset(df_af, df_af$VEG_GROUP=="Rainforest and related scrub")

sum(is.na(df_rain$fbi))
sum(is.na(df_rain$sfl))
sum(is.nan(df_rain$fbi))
sum(is.nan(df_rain$sfl))
sum(is.infinite(df_rain$fbi))
sum(is.infinite(df_rain$sfl))

df_rain <- df_rain[complete.cases(df_rain$fbi),]
###################################################
#create basic linear model (GLM)
mod_rain <- glm(FRP ~ fbi*sfl, data = df_rain)
ef_rain <- ggpredict(mod_rain, terms=c("fbi","sfl"))
plot(ef_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_rain) #AIC: 7845.3, intercept signif, no other terms signif

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_rain)
#resid vs fitted
#Q-Q has large upward curve
#scale-location is good
#no influential points

#run descdist to find suggested family
par(mfrow=c(1,1))
descdist(df_rain$FRP)
#gamma/beta

#create linear model with suggested family
df_rain0 <- df_rain[df_rain$FRP > 0, ]
mod_rain <- glm(FRP ~ fbi*sfl, family = Gamma(link = "log"), data = df_rain0)
ef_rain <- ggpredict(mod_rain, terms=c("fbi","sfl"))
plot(ef_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_rain) #AIC = 6647.6, intercept signif, no other terms signif

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
plot(mod_rain)
#resid vs fitted is good
#Q-Q is good
#scale-location is good
#no influential points

#check density distribution
ggplot(df_rain, aes(x=FRP)) + 
geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#right skew, sqrt or log transform

ggplot(df_rain, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#looks normal
ggplot(df_rain, aes(x=sqrt(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#still right skewed a little
par(mfrow=c(1,1))
descdist(log1p(df_rain$FRP))
#closest to normal, uniform also an option

#create transformed model
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
mod_rain <- glm(log1p(FRP) ~ fbi*sfl, family = gaussian(link = "log"), data = df_rain, start = start_values)
ef_rain <- ggpredict(mod_rain, terms=c("fbi","sfl"))
plot(ef_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_rain) #AIC = 1854.3, intercept signif, no other terms signif

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_rain)
#diagnostics look good
#lets take some terms out, sfl int
start_values <- c(Intercept = 1, fbi = 0)
mod_rain <- glm(log1p(FRP) ~ fbi, family = gaussian(link = "log"), data = df_rain, start = start_values)
ef_rain <- ggpredict(mod_rain, terms=c("fbi"))
plot(ef_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_rain) 
#fbi and sfl not signif
#take sfl out
#fbi still not signif but very close

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_rain, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_rain, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_rain, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "rain", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

################
#subset for less than 17.5
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
mod_rain <- glm(log1p(FRP) ~ fbi*sfl, family = gaussian(link = "log"), data = df_rain_ss, start = start_values)
ef_rain <- ggpredict(mod_rain, terms=c("fbi","sfl"))
plot(ef_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_rain)

start_values <- c(Intercept = 1, fbi = 0, sfl = 0)
mod_rain <- glm(log1p(FRP) ~ fbi+sfl, family = gaussian(link = "log"), data = df_rain_ss, start = start_values)
ef_rain <- ggpredict(mod_rain, terms=c("fbi","sfl"))
plot(ef_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_rain)
#fbi signif

start_values <- c(Intercept = 1, fbi = 0)
mod_rain <- glm(log1p(FRP) ~ fbi, family = gaussian(link = "log"), data = df_rain_ss, start = start_values)
ef_rain <- ggpredict(mod_rain, terms=c("fbi"))
plot(ef_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_rain)
#fbi signif again, AIC has improved from no subset to 1815

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
gam_rain <- gam(FRP ~ fbi*sfl, data = df_rain)
ef_gam_rain <- ggpredict(gam_rain, terms = c("fbi","sfl"))
plot(ef_gam_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#summary (GCV scores)
summary(gam_rain) #R-sq.(adj) =  -0.000543   Deviance explained = 0.487%  GCV =  78343
#only intercept signif

#run diagnostic plots
par(mfrow=c(2,2))
gam.check(gam_rain)
#1 has curve
#2 is good
#3 has right skew
#4 is good

#run descdist to find suggested family
#beta/gamma as before

#density plot of response variable
#right skew as before

#transform response variable to be normally distributed
#wants log transform

#run descdist to find suggested family
#wants a gaussian family


#create linear model with suggested family
gam_rain <- gam(log1p(FRP) ~ fbi*sfl, family = gaussian (link = "log"), data = df_rain)
ef_gam_rain <- ggpredict(gam_rain, terms = c("fbi","sfl"))
plot(ef_gam_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (GCV scores)
summary(gam_rain) #R-sq.(adj) =  -0.00063   Deviance explained = 0.478%  GCV = 1.6382


#run diagnostic plots to find suggested transform
gam.check(gam_rain)
#they look good

#create transformed model, with smooth terms and ti
gam_rain <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3) + ti(fbi,sfl,k=3), family = gaussian (link = "log"), data = df_rain)
ef_gam_rain <- ggpredict(gam_rain, terms = c("fbi","sfl"))
plot(ef_gam_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#fbi signif, other terms not signif

gam_rain <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3), family = gaussian (link = "log"), data = df_rain)
ef_gam_rain <- ggpredict(gam_rain, terms = c("fbi","sfl"))
plot(ef_gam_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#fbi signif, other terms not signif

gam_rain <- gam(log1p(FRP) ~ s(fbi,k=3), family = gaussian (link = "log"), data = df_rain)
ef_gam_rain <- ggpredict(gam_rain, terms = c("fbi"))
plot(ef_gam_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#all terms signif

#summary (GCV scores)
summary(gam_rain)

#run diagnostic plots to assess fit
gam.check(gam_rain)



#visually inspect model against scatter 
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_gam_rain, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_rain, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_rain, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "rain", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 35), breaks = seq(0, 35, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 50)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#data is too sparse for FBI > 17.5 so maybe subset and fit smooth again
#see if we can stop mixing by lowering fbi range again
df_rain_ss <- df_rain[df_rain$fbi < 17.5,]

#create transformed model, with smooth terms and ti
gam_rain <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3) + ti(fbi,sfl,k=3), family = gaussian (link = "log"), data = df_rain_ss)
ef_gam_rain <- ggpredict(gam_rain, terms = c("fbi","sfl"))
plot(ef_gam_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#fbi signif, other terms not signif
#mixing is shit

gam_rain <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3), family = gaussian (link = "log"), data = df_rain_ss)
ef_gam_rain <- ggpredict(gam_rain, terms = c("fbi","sfl"))
plot(ef_gam_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#fbi signif, other terms not signif
#looks smooth

gam_rain <- gam(log1p(FRP) ~ s(fbi,k=3), family = gaussian (link = "log"), data = df_rain_ss)
ef_gam_rain <- ggpredict(gam_rain, terms = c("fbi"))
plot(ef_gam_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#all terms signif
#looks smooth

#summary (GCV scores)
summary(gam_rain)

#run diagnostic plots to assess fit
gam.check(gam_rain)

#visual inspection
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_gam_rain, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_rain, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_rain, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "rain", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 35), breaks = seq(0, 35, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 50)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


#have arrived at the same model for GAM and GLM, choose GLM for simplicity
#sfl does not have significant effect in this case
#log(FRP) ~ fbi, gauss with FBI < 17.5