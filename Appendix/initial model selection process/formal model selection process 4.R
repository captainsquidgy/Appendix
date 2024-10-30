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
df_af_jf <- df[month(df$date) < 3, ]
df_af_sum2 <- rbind(df_af_nd, df_af_jf)
df_af_sum2 <- df_af_sum2[is.finite(df_af_sum2$FRP), ]
df_af_sum2 <- df_af_sum2[complete.cases(df_af_sum2$FBI),]

#subset for vegetation type
df_veg <- df_af_sum2[df_af_sum2$VEG_GROUP == "Dry eucalypt forest and woodland",]
df_veg <- df_veg[is.finite(df_veg$FRP), ]


###############
#selecting data on an FFDI based rolling quantile for particular vegetation type
out <- list()

for (i in 1:50){
  print(i)
  
  df_roll <- df_veg[df_veg$FFDI > i-1 & df_veg$FFDI < i,]
  
  df_roll <- df_roll[df_roll$FRP > quantile(df_roll$FRP, probs=0.80, na.rm=TRUE),]
  
  out[[i]] <- df_roll
  
}

out_data <- do.call(rbind,out)
out_d <- as.data.frame(out_data)

###############
#selecting data on an FFDI based rolling quantile for all veg types
out <- list()

for (i in 1:50){
  print(i)
  
  df_roll <- df_af_sum2[df_af_sum2$FFDI > i-1 & df_af_sum2$FFDI < i,]
  
  df_roll <- df_roll[df_roll$FRP > quantile(df_roll$FRP, probs=0.80, na.rm=TRUE),]
  
  out[[i]] <- df_roll
  
}

out_data <- do.call(rbind,out)
out_d <- as.data.frame(out_data)
out_d <- out_d[is.finite(out_d$FRP), ]

###############
#create basic linear model (GLM)
mod_q <- glm(FRP ~ FFDI*SFL, data = out_d)
ef_q <- ggpredict(mod_q, terms = c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_q)

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_q)

#run descdist to find suggested family
par(mfrow=c(1,1))
descdist(out_d$FRP)

#density plot of response variable
ggplot(out_d, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#transform response variable to be normally distributed if it can
ggplot(out_d, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#descdist with transform as in density plot to find family
par(mfrow=c(1,1))
descdist(log1p(out_d$FRP))

#create linear model with suggested transform and family
mod_q <- glm(log1p(FRP) ~ FFDI*SFL, family = gaussian(link = "log"), data = out_d)
ef_q <- ggpredict(mod_q, terms = c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_q)

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_q)

#fit model with suggested terms
mod_q <- glm(log1p(FRP) ~ FFDI, family = gaussian(link = "log"), data = out_d)
ef_q <- ggpredict(mod_q, terms = c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_q)

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_q)

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FFDI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans = "log", limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


##################
##create GAM
gam_q <- gam(FRP ~ FFDI*SFL, data = out_d)
ef_q <- ggpredict(gam_q, terms = c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_q)

#run diagnostic plots
par(mfrow=c(2,2))
gam.check(gam_q)

#run descdist to find suggested family
par(mfrow=c(1,1))
descdist(out_d$FRP)

#density plot of response variable
ggplot(out_d, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#transform response variable to be normally distributed if it can
ggplot(out_d, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#descdist with transform as in density plot to find family
par(mfrow=c(1,1))
descdist(out_d$FRP)

#create linear model with suggested transform and family
gam_q <- glm(FRP ~ FFDI*SFL, family = ...(link = "..."), data = out_d)
ef_q <- ggpredict(gam_q, terms = c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_q)

#run diagnostic plots
par(mfrow=c(2,2))
plot(gam_q)

#fit model with suggested terms
gam_q <- gam(FRP ~ FFDI*SFL, family = ... (link = "..."), data = out_d)
ef_q <- ggpredict(gam_q, terms = c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_q)

#run diagnostic plots
par(mfrow=c(2,2))
gam.check(mod_q)

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FFDI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans = "log", limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

##########################
#repeat for FBI
##########################
#create basic linear model (GLM)
mod_q <- glm(FRP ~ FBI*SFL, data = out_d)
ef_q <- ggpredict(mod_q, terms = c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_q)

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_q)

#run descdist to find suggested family
par(mfrow=c(1,1))
descdist(out_d$FRP)

#density plot of response variable
ggplot(out_d, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#transform response variable to be normally distributed if it can
ggplot(out_d, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#descdist with transform as in density plot to find family
par(mfrow=c(1,1))
descdist(out_d$FRP)

#create linear model with suggested transform and family
mod_q <- glm(FRP ~ FBI*SFL, family = ...(link = "..."), data = out_d)
ef_q <- ggpredict(mod_q, terms = c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_q)

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_q)

#fit model with suggested terms
mod_q <- glm(FRP ~ FBI*SFL, family = ... (link = "..."), data = out_d)
ef_q <- ggpredict(mod_q, terms = c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_q)

#run diagnostic plots
par(mfrow=c(2,2))
plot(mod_q)

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans = "log", limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


##################
##create GAM
gam_q <- gam(FRP ~ FBI*SFL, data = out_d)
ef_q <- ggpredict(gam_q, terms = c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_q)

#run diagnostic plots
par(mfrow=c(2,2))
gam.check(gam_q)

#run descdist to find suggested family
par(mfrow=c(1,1))
descdist(out_d$FRP)

#density plot of response variable
ggplot(out_d, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#transform response variable to be normally distributed if it can
ggplot(out_d, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#descdist with transform as in density plot to find family
par(mfrow=c(1,1))
descdist(out_d$FRP)

#create linear model with suggested transform and family
gam_q <- glm(FRP ~ FBI*SFL, family = ...(link = "..."), data = out_d)
ef_q <- ggpredict(gam_q, terms = c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_q)

#run diagnostic plots
par(mfrow=c(2,2))
plot(gam_q)

#fit model with suggested terms
gam_q <- gam(FRP ~ FBI*SFL, family = ... (link = "..."), data = out_d)
ef_q <- ggpredict(gam_q, terms = c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_q)

#run diagnostic plots
par(mfrow=c(2,2))
gam.check(mod_q)

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Dry Euc", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans = "log", limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")