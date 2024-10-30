#refining the model selection process to make it repeatable easily 
library(terra)
library(lubridate)
library(tidyverse)
library(quantreg)
library(ggeffects)
library(viridis)
library(fitdistrplus)
library(qgam)

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
mod_q <- rq(FRP ~ FBI*SFL, tau = 0.8, data = df)
ef_q <- ggpredict(mod_q, terms=c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#good shape, sfl looks to have little effect

#summary (AIC scores)
summary(mod_q)
#fbi is signif, sfl and fbi:sfl is not signif

#density plot of response variable
ggplot(df, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#right skewed

#transform response variable to be normally distributed if it can
ggplot(df, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#normal dist now

#create linear model with suggested transform
mod_q <- rq(log1p(FRP) ~ FBI*SFL, tau = 0.8, data = df)
ef_q <- ggpredict(mod_q, terms=c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#sfl visually looks to have little effect till upper fuel loads

#summary (AIC scores)
summary(mod_q)
#slightly better, sfl and fbi:sfl still not signif

#fit model with suggested terms
mod_q <- rq(log1p(FRP) ~ FBI+SFL, tau = 0.8, data = df)
ef_q <- ggpredict(mod_q, terms=c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#sfl visuall still not useful

#summary (AIC scores)
summary(mod_q)
#sfl not significant

#fit model with suggested terms
mod_q <- rq(log1p(FRP) ~ FBI, tau = 0.8, data = df)
ef_q <- ggpredict(mod_q, terms=c("FBI"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#looks good

#summary (AIC scores)
summary(mod_q)
#fbi significant

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = df, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Data", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1450), breaks = seq(0, 1450, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


###########################################
#create basic linear model (GAM)
#model for the upper quantile (80th)
gam_q <- qgam(FRP ~ FBI*SFL, qu = 0.8, data = df)
ef_q <- ggpredict(gam_q, terms=c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#sfl looks useless

#summary (AIC scores)
summary(gam_q)
#fbi only signif term, R-sq.(adj) =  0.031   Deviance explained =  2.2%

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
gam_q <- qgam(log1p(FRP) ~ FBI+SFL, qu = 0.8, data = df)
ef_q <- ggpredict(gam_q, terms=c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#sfl still looks useless

#summary (AIC scores)
summary(gam_q)
#sfl not signif

#fit model with suggested terms
gam_q <- qgam(log1p(FRP) ~ FBI, qu = 0.8, data = df)
ef_q <- ggpredict(gam_q, terms=c("FBI"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_q)
#all terms signif

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = df, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Data", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1450), breaks = seq(0, 1450, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

###########################################
#log1p(FRP) ~ FBI is clearly the best model

