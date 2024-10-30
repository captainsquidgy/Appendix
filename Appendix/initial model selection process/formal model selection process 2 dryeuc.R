#refining the model selection process to make it repeatable easily 
library(terra)
library(lubridate)
library(tidyverse)
library(quantreg)
library(ggeffects)
library(viridis)
library(fitdistrplus)

setwd('C:/Users/zmajor/OneDrive - University of Tasmania/zac')
df <- read.table("afdr_dataframe4.csv", header = TRUE, sep = ",", row.names =  NULL)

#then constrain it for summer (november to march)

df_af_nd <- df[month(df$date) > 10,]
df_af_jfm <- df[month(df$date) < 4, ]
df_af_sum <- rbind(df_af_nd, df_af_jfm)

#subset for vegetation type
df <- df_af_sum[df_af_sum$VEG_GROUP == "Dry eucalypt forest and woodland",]

df <- df[is.finite(df$FRP), ]

###################################################
#create basic linear model (GLM)
#model for the upper quantile (80th)
mod_q <- rq(FRP ~ FFDI*SFL, tau = 0.8, data = df)
ef_q <- ggpredict(mod_q, terms=c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#sfl has visual effect

#summary (AIC scores)
summary(mod_q)
#ffdi:sfl not signif

#density plot of response variable
ggplot(df, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#right skew

#transform response variable to be normally distributed if it can
ggplot(df, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#normal

#create linear model with suggested transform
mod_q <- rq(log1p(FRP) ~ FFDI*SFL, tau = 0.8, data = df)
ef_q <- ggpredict(mod_q, terms=c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#sfl has visual effect at upper ffdi

#summary (AIC scores)
summary(mod_q)
#ffdi:sfl and sfl not signif

#create linear model with suggested transform
mod_q <- rq(log1p(FRP) ~ FFDI+SFL, tau = 0.8, data = df)
ef_q <- ggpredict(mod_q, terms=c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#

#summary (AIC scores)
summary(mod_q)
#

#fit model with suggested terms
mod_q <- rq(log1p(FRP) ~ FFDI, tau = 0.8, data = df)
ef_q <- ggpredict(mod_q, terms=c("FFDI"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#sfl has little visual effect
#looks good

#summary (AIC scores)
summary(mod_q)
#sfl not signi
#all terms signif

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = df, aes(x = FFDI, y = FRP, color = as.factor(SFL)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Data", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1450), breaks = seq(0, 1450, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


###########################################
#create basic linear model (GAM)
#model for the upper quantile (80th)
gam_q <- qgam(FRP ~ FFDI*SFL, qu = 0.8, data = df)
ef_q <- ggpredict(gam_q, terms=c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#sfl has visual effect

#summary (AIC scores)
summary(gam_q)
#only ffdi signif

#density plot of response variable
ggplot(df, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#right skew

#transform response variable to be normally distributed if it can
ggplot(df, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#normal

#create linear model with suggested transform
gam_q <- qgam(log1p(FRP) ~ FFDI*SFL, qu = 0.8, data = df)
ef_q <- ggpredict(gam_q, terms=c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#sfl has visual effect at upper ffdi

#summary (AIC scores)
summary(gam_q)
#only ffdi signif

#fit model with suggested terms
gam_q <- qgam(log1p(FRP) ~ FFDI, qu = 0.8, data = df)
ef_q <- ggpredict(gam_q, terms=c("FFDI"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#sfl has almost no visual effect
#looks celan now

#summary (AIC scores)
summary(gam_q)
#sfl still not signif, 
#all terms signif now, R-sq.(adj) =  0.0434   Deviance explained = 27.6%

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = df, aes(x = FFDI, y = FRP, color = as.factor(SFL)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Data", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1450), breaks = seq(0, 1450, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

###########################################
#log(FRP) ~ FFDI is clearly the best model