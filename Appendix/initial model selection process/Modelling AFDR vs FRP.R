#packages 
library(terra)
library(lubridate)
library(tidyverse)
library(quantreg)
library(ggeffects)
library(viridis)
library(mgcv)
library(fitdistrplus)

#read in csv
setwd('C:/Users/zmajor/OneDrive - University of Tasmania/zac')
df_af <- read.csv('afdr_dataframe2.csv')


#we now have dataframe with afdrs data, so lets plot a few things
ggplot(df_af, aes(x=fbi, y=FRP)) + 
  geom_jitter(alpha=0.5, size=0.1)    

#check date alignment
df_2013 <- subset(df_af, year(df_af$date) == 2013)

#this gives us a scaled ffdi to compare (comparison only approx)
scale_fbi_to_frp <- function(ffdi) {
  ffdi * (500 / 25)
}

df_2013$scaled_fbi <- scale_fbi_to_frp(df_2013$fbi)


#plots FRP and FFDI against date for comparison, includes all locations
ggplot(df_2013, aes(x = date)) +
  geom_point(aes(y = FRP, color = "FRP"), size=1, position=position_nudge(x=0.15)) +
  geom_point(aes(y = scaled_fbi, color = "FBI"), size=1, position=position_nudge(x=-0.15)) +
  scale_y_continuous(
    name = "FRP",
    sec.axis = sec_axis(~ .*(25/500), name = "FBI", breaks = seq(0, 100, by = 10))
  ) +
  labs(title = "FRP and FBI over Time",
       x = "Date") +
  scale_color_manual(name = "Legend", values = c("FRP" = "blue", "FBI" = "red")) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red")
  )

#date alignment looks good

#lets get a colour gradient for fuel load
ggplot() +
  geom_jitter(data = df_af, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 500)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#nice now lets subset for veg type
table(df_af$VEG_GROUP)

df_dryeuc <- subset(df_af, df_af$VEG_GROUP=="Dry eucalypt forest and woodland")
df_weteuc <- subset(df_af, df_af$VEG_GROUP=="Wet eucalypt forest and woodland")
df_grass <- subset(df_af, df_af$VEG_GROUP=="Native grassland")
df_rain <- subset(df_af, df_af$VEG_GROUP=="Rainforest and related scrub")
df_scrub <- subset(df_af, df_af$VEG_GROUP=="Scrub, heathland and coastal complexes")
df_mod <- subset(df_af, df_af$VEG_GROUP=="Modified land")
df_moor <- subset(df_af, df_af$VEG_GROUP=="Moorland, sedgeland and rushland")
df_salt <- subset(df_af, df_af$VEG_GROUP=="Saltmarsh and wetland")
df_high <- subset(df_af, df_af$VEG_GROUP=="Highland and treeless vegetation")
df_other <- subset(df_af, df_af$VEG_GROUP=="Other natural environments")
df_non <- subset(df_af, df_af$VEG_GROUP=="Non eucalypt forest and woodland")


#dry euc
ggplot() +
  geom_jitter(data = df_dryeuc, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 250)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#wet euc
ggplot() +
  geom_jitter(data = df_weteuc, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Wet Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#moor euc
ggplot() +
  geom_jitter(data = df_moor, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Moorland", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#modified
ggplot() +
  geom_jitter(data = df_mod, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Modified", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#grass
ggplot() +
  geom_jitter(data = df_grass, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Grassland", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#rain
ggplot() +
  geom_jitter(data = df_rain, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Rainforest", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#high
ggplot() +
  geom_jitter(data = df_high, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Highland", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")


#salt
ggplot() +
  geom_jitter(data = df_salt, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Salt", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#scrub
ggplot() +
  geom_jitter(data = df_scrub, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Scrub", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#other
ggplot() +
  geom_jitter(data = df_other, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Other", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#non
ggplot() +
  geom_jitter(data = df_non, aes(x = fbi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Non euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0,65), breaks = seq(0, 65, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

#there appears to be a high number of points with higher than expected FRP for FBI=0 for many of the more dominant veg types
#investigate this

df_0 <- subset(df_af, df_af$fbi==0)
view(df_0)
#they occur across most veg types, and in any month
#found a set of them on dunalley day, in forest or modified land, with high fuel load, makes me question the rest of the data

#have decided that these are actually as expected as they bring down average FRP for particular FBI, there is a fair bit of noise in the data

#repeat basic modelling as same for FFDI, can focus on the best models. X^2, X^3, with interaction from sfl

#linear
mod_dryeuc <- glm(FRP ~ fbi*sfl+0, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

#x^2
mod_dryeuc <- glm(FRP ~ I(fbi^2)*sfl+0, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))

#this doubles predictions which almost exacty approximates FI in AFDR
ef_dryeuc$predicted <- 2*ef_dryeuc$predicted
ef_dryeuc$conf.low <- 2*ef_dryeuc$conf.low
ef_dryeuc$conf.high <- 2*ef_dryeuc$conf.high

ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)
#looks pretty good

#x^2 gam
gam_dryeuc <- gam(FRP ~ I(fbi^2)*sfl + 0, data = df_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
test <- subset(ef_gam_dryeuc, ef_gam_dryeuc$x < 50)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc)
#also looks good and has cool "deviance explained" score. nice



#x^3
mod_dryeuc <- glm(FRP ~ I(fbi^3)*sfl+0, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 110)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)
#looks pretty bad

#log1p
mod_dryeuc <- glm(log1p(FRP) ~ fbi*sfl+0, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)
#looks bad

#ok so now lets take best model and compare to raw data
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_dryeuc, aes(x = fbi, y = FRP, color = as.factor(sfl)), alpha=1, size=1.5) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2200), breaks = seq(0, 2200, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#looks good, and predicts similar FI as actual system (matthews 2019)
#now go through formal process to establish model

#have cleaned sfl data selection rules
#plot density distribution and select family for GLM
#include confidence variable to assess reationship and effect (veg type as interaction)
#select some transformations x^2, x^3, log1p
#assess diagnostic plots and lambda value
#select model from visual inspection, AIC, and diagnostics


#plot density distribution and select family for GLM
#FBI
ggplot(df_dryeuc, aes(x=fbi)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FBI", x = "FBI", y = "Density")
#is right skewed so apply log transform to FBI
df_dryeuc$log_fbi <- log(df_dryeuc$fbi + 1)

ggplot(df_dryeuc, aes(x=log_fbi)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FBI", x = "FBI", y = "Density")
#looks normal now

#SFL
ggplot(df_dryeuc, aes(x=sfl)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of SFL", x = "SFL", y = "Density")
#looks normal

#FRP
ggplot(df_dryeuc, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#right skewed so log transform

df_dryeuc$log_FRP <- log(df_dryeuc$FRP + 1)

ggplot(df_dryeuc, aes(x=log_FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")
#looks great
#refit the model
#x^2
mod_dryeuc <- glm(log1p(FRP) ~ I(fbi^2)*sfl, data = df_dryeuc)
mod_dryeuc <- glm(sqrt(FRP) ~ I(fbi^2)*sfl, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)
#looks better when we take out the + 0 term, diagnostics plots look good, one point in residual vs fitted is rogue
#what this implies however is that FBI 0-25 predicts similar FRP. Data is skewed by low confidence points. Data collection is poor.

#include confidence variable to assess reationship and effect (veg type as interaction)
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_jitter(data = df_dryeuc, aes(x = fbi, y = FRP, color = as.factor(conf)), alpha=1, size=1.5) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1100), breaks = seq(0, 1100, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")
#interesting that confidence has clear gradient from low FRP to high FRP. We can remove some lower value points and plot again

df_dryeuc <- subset(df_dryeuc, df_dryeuc$conf > 65 )

ggplot(df_dryeuc, aes(x=conf)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of conf", x = "conf", y = "Density")

#select some transformations x^2, x^3, log1p
#x^2
mod_dryeuc <- glm(FRP ~ I(fbi^2)*sfl+0, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

#x^3
mod_dryeuc <- glm(FRP ~ I(fbi^3)*sfl+0, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

#log1p
mod_dryeuc <- glm(log1p(FRP) ~ fbi*sfl+0, data = df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

#assess diagnostic plots and lambda value
par(mfrow=c(2,2))
plot(mod_dryeuc)
boxcox(mod_dryeuc)
#select model from visual inspection, AIC, and diagnostics




#fitdistrplus
descdist(df_dryeuc$FRP)
?family

mod_dryeuc <- glm(FRP ~ fbi*sfl, family = Gamma(link="inverse"), data = df_dryeuc, start=start_values)
df_dryeuc <- df_dryeuc[df_dryeuc$FRP > 0, ]
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)

ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("fbi","sfl"))
ef_dryeuc <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

mod_dryeuc <- glm(sqrt(FRP) ~ fbi*sfl, family = Gamma(link="inverse"), data = df_dryeuc, start=start_values)

par(mfrow=c(2,2))
plot(mod_dryeuc)

df_dryeuc[531,]
