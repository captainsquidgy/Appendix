#creating a rolling percentile for each fbi "class"
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

df_af_jf <- df[month(df$date) < 3, ]
df_af_sum2 <- rbind(df_af_nd, df_af_jf)

#subset for each fbi value

df_roll <- df_af_sum[df_af_sum$FBI == 0,]

#give me the upper 20 percent

quantile(df_roll$FRP, probs=0.8)
df_roll <- df_roll[df_roll$FRP > quantile(df_roll$FRP, probs=0.8),]

#nice now we need to do this for all fbi


out <- list()

#lets do this in a loop

for (i in 1:50){
  print(i)
  
  df_roll <- df_af_sum2[df_af_sum2$FBI == i,]
  
  df_roll <- df_roll[df_roll$FRP > quantile(df_roll$FRP, probs=0.80),]
  
  out[[i]] <- df_roll
  
}

out_data <- do.call(rbind,out)
out_d <- as.data.frame(out_data)

#lets try and create a loop that increases the bucket size

se <- seq(1,50,by=10)

for (i in se){
  print(i)
  
  df_roll <- df_af_sum2[df_af_sum2$FBI == i | df_af_sum2$FBI == i+1 | df_af_sum2$FBI == i+2 | df_af_sum2$FBI == i+3| df_af_sum2$FBI == i+4| df_af_sum2$FBI == i+5| df_af_sum2$FBI == i+6| df_af_sum2$FBI == i+7| df_af_sum2$FBI == i+8| df_af_sum2$FBI == i+9,] 
  
  df_roll <- df_roll[df_roll$FRP > quantile(df_roll$FRP, probs=0.90),]
  
  out[[i]] <- df_roll
  
}

out_data <- do.call(rbind,out)
out_d <- as.data.frame(out_data)



#visually inspect model against scatter
ggplot() +
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=0.5, size=0.8) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Data", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 1000)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(out_d, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#fit a GLM to this

mod_q <- glm(log1p(FRP) ~ FBI, data = out_d)
ef_q <- ggpredict(mod_q, terms=c("FBI"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_q)


ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Data", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans="log",limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#break down by veg type

#dry euc

df_veg <- df_af_sum2[df_af_sum2$VEG_GROUP == "Dry eucalypt forest and woodland",]
df_veg <- df_veg[is.finite(df_veg$FRP), ]

#find upper quantile loop
out <- list()

#lets do this in a loop

for (i in 1:50){
  print(i)
  
  df_roll <- df_veg[df_veg$FBI == i,]
  
  df_roll <- df_roll[df_roll$FRP > quantile(df_roll$FRP, probs=0.80),]
  
  out[[i]] <- df_roll
  
}

out_data <- do.call(rbind,out)
out_d <- as.data.frame(out_data)


#fit a GLM to this
mod_q <- glm(log1p(FRP) ~ FBI+SFL, data = out_d)
ef_q <- ggpredict(mod_q, terms=c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_q)


ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Data", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans="log", limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#lets compare ffdi and fbi

#find upper quantile loop
out <- list()

#lets do this in a loop

for (i in 1:50){
  print(i)
  
  df_roll <- df_af_sum2[df_af_sum2$FBI == i,]
  
  df_roll <- df_roll[df_roll$FRP > quantile(df_roll$FRP, probs=0.80),]
  
  out[[i]] <- df_roll
  
}

out_data <- do.call(rbind,out)
out_d <- as.data.frame(out_data)


#fit a GLM to this
mod_q <- glm(log1p(FRP) ~ FBI+SFL, data = out_d)
ef_q <- ggpredict(mod_q, terms=c("FBI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_q)


ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Data", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans="log", limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#lets do this for FFDI to compare the pair
df_af_sum2 <- df_af_sum2[is.finite(df_af_sum2$FRP), ]
#find upper quantile loop
out <- list()

#lets do this in a loop

for (i in 1:50){
  print(i)
  
  df_roll <- df_veg[df_veg$FFDI > i-1 & df_veg$FFDI < i,]
  
  df_roll <- df_roll[df_roll$FRP > quantile(df_roll$FRP, probs=0.80, na.rm=TRUE),]
  
  out[[i]] <- df_roll
  
}

out_data <- do.call(rbind,out)
out_d <- as.data.frame(out_data)


#fit a GLM to this
mod_q <- glm(log1p(FRP) ~ FFDI+SFL, data = out_d)
ef_q <- ggpredict(mod_q, terms=c("FFDI","SFL"))
plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(mod_q)


ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FFDI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Data", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

