#model selection process
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
#suggests a gamma model

#create linear model with suggested family
df_dryeuc <- df_dryeuc[df_dryeuc$FRP > 0, ]
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
mod_dryeuc <- glm(FRP ~ fbi*sfl, family = Gamma(link="identity"), data = df_dryeuc, start=start_values)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_dryeuc) #48666

#run diagnostic plots to find suggested transform
par(mfrow=c(2,2))
plot(mod_dryeuc)
#residual vs fitted looks good
#Q-Q plot has upward curve but much less
#scale-location horizontal, good
#no influential points

#create transformed model
#diagnostic plots suggest sqrt(response) or log 1p
#sqrt
mod_dryeuc <- glm(sqrt(FRP) ~ fbi*sfl, family = Gamma(link="log"), data = df_dryeuc, start=start_values)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_dryeuc) #25179

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_dryeuc)
#res vs fit is good
#Q-Q has improved, still slight upward curve
#scale-location is horizontal, good
#no influential points

#transform again
mod_dryeuc <- glm(sqrt(FRP) ~ I(fbi^2)*sfl, family = Gamma(link="log"), data = df_dryeuc, start=start_values)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 60)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#goes a bit high  at the upper FBI range

#summary
summary(mod_dryeuc) #25132

#diagnostics
par(mfrow=c(2,2))
plot(mod_dryeuc)
#Q-Q slight improve
#all others slight degrade

#visually inspect model against scatter 

#log1p
mod_dryeuc <- glm(log1p(FRP) ~ fbi*sfl, family = Gamma(link="log"), data = df_dryeuc, start=start_values)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_dryeuc) #13715

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_dryeuc)
#all good except Q-Q shows slight downward curve

#transform
mod_dryeuc <- glm(I((log1p(FRP))^2) ~ fbi*sfl, family = Gamma(link="log"), data = df_dryeuc, start=start_values)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 30)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#way overpredicts

#summary (AIC scores)
summary(mod_dryeuc) #30985

#run diagnostic plots to assess fit
par(mfrow=c(2,2))
plot(mod_dryeuc)
#these look good, Q-Q slight improvement

#visually inspect model against scatter 

#select model
#lets take best models and compare them to scatter 
mod_dryeuc <- glm(sqrt(FRP) ~ fbi*sfl, family = Gamma(link="log"), data = df_dryeuc, start=start_values)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

mod_dryeuc <- glm(log1p(FRP) ~ fbi*sfl, family = Gamma(link="log"), data = df_dryeuc, start=start_values)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

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

#we can see that there is not much data for FBI > 40 and FRP > 500 so we crop to that
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_dryeuc, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=0.8, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#select sqrt(FRP), gamma(log) model because better delineation of behaviour across whole FBI range
mod_dryeuc <- glm(sqrt(FRP) ~ fbi*sfl, family = Gamma(link="log"), data = df_dryeuc, start=start_values)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
