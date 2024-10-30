#model selection but trying to get a gaussian family (ie. trying to get normal distribution in response variable)
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

#basic linear glm
mod_dryeuc <- glm(FRP ~ fbi*sfl, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_dryeuc) #60952

#check distribution of response variable
ggplot(df_dryeuc, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#its right skewed

#diagnostic plots
par(mfrow=c(2,2))
plot(mod_dryeuc)
#residual vs fitted looks good
#Q-Q shows large upward curve
#scale-location show moderate positive slope
#no influential points

#log transform
df_dryeuc$log_FRP <- log1p(df_dryeuc$FRP)
ggplot(df_dryeuc, aes(x=log_FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#looks normal now

mod_dryeuc <- glm(log_FRP ~ fbi*sfl, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

par(mfrow=c(2,2))
plot(mod_dryeuc)

#residuals all look good
descdist(df_dryeuc$log_FRP)

#pretty close to normal dist so gaussian family is ok
mod_dryeuc <- glm(log1p(FRP) ~ fbi*sfl, family = gaussian(link="identity"), data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

par(mfrow=c(2,2))
plot(mod_dryeuc)


ggplot() +
  # Plot the predicted values
  geom_line(data = ef_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_dryeuc, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#crop the visual range
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_dryeuc, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")
