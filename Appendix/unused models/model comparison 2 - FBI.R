#trialling qgam again
library(terra)
library(lubridate)
library(tidyverse)
library(quantreg)
library(ggeffects)
library(viridis)
library(fitdistrplus)
library(qgam)
library(AICcmodavg)
library(mgcv)

setwd('C:/Users/zmajor/OneDrive - University of Tasmania/zac')
df <- read.table("afdr_dataframe4.csv", header = TRUE, sep = ",", row.names =  NULL)

#then constrain it for summer (november to march)

df_af_nd <- df[month(df$date) > 10,]
df_af_jf <- df[month(df$date) < 3, ]
df_af_sum2 <- rbind(df_af_nd, df_af_jf)

sum(is.na(df_af_sum2$FRP))
df_af_sum2 <- df_af_sum2[is.finite(df_af_sum2$FRP), ]

#subset for vegetation type
df <- df_af_sum2[df_af_sum2$VEG_GROUP == "Dry eucalypt forest and woodland",]

df <- df[is.finite(df$FRP), ]

###########################

#density plot of response variable
ggplot(df_af_sum2, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")

#transform response variable to be normally distributed if it can
ggplot(df_af_sum2, aes(x=log1p(FRP))) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of FRP", x = "FRP", y = "Density")


#models for the upper quantile (80th)
#FBI 
gam_q <- qgam(log1p(FRP) ~ FBI, qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI"))

#FBI + SFL
gam_q <- qgam(log1p(FRP) ~ FBI + SFL, qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI","SFL"))

#FBI + VEG
gam_q <- qgam(log1p(FRP) ~ FBI + VEG_GROUP, qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI", "VEG_GROUP"))

#FBI + SFL + VEG
gam_q <- qgam(log1p(FRP) ~ FBI + SFL + VEG_GROUP, qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI","SFL","VEG_GROUP"))

#FBI * SFL + VEG
gam_q <- qgam(log1p(FRP) ~ FBI * SFL + VEG_GROUP, qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI","SFL","VEG_GROUP"))

#s(FBI) 
gam_q <- qgam(log1p(FRP) ~ s(FBI, k=3), qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI"))

#s(FBI) + SFL
gam_q <- qgam(log1p(FRP) ~ s(FBI, k=3) + SFL, qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI","SFL"))

#s(FBI) + VEG
gam_q <- qgam(log1p(FRP) ~ s(FBI, k=3) + VEG_GROUP, qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI","VEG_GROUP"))

#s(FBI) + SFL + VEG
gam_q <- qgam(log1p(FRP) ~ s(FBI, k=3) + SFL + VEG_GROUP, qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI","SFL","VEG_GROUP"))

#s(FBI) * SFL + VEG
gam_q <- qgam(log1p(FRP) ~ s(FBI, k=3) * SFL + VEG_GROUP, qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI","SFL","VEG_GROUP"))

#1
na.omit(df_af_sum2)
gam_q <- qgam(log1p(FRP) ~ 1, qu = 0.8, data = df_af_sum2)
ef_q <- ggpredict(gam_q, terms=c("FBI","SFL","VEG_GROUP"))


my_colors <- colorRampPalette(c("brown", "#932667FF", "#DD513AFF", "#FCA50AFF", "yellow"))
color_palette <- my_colors(12)
plot(ef_q, colors = color_palette)


plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_q)
AIC(gam_q)
par(mfrow=c(2,2))
gam.check(gam_q)




#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = df_af_sum2, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Veg", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans = "log", limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = df_af_sum2, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Veg", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

###################
#aic comparison
gam_1 <- qgam(log1p(FRP) ~ FBI, qu = 0.8, data = df_af_sum2)

gam_2 <- qgam(log1p(FRP) ~ FBI + SFL, qu = 0.8, data = df_af_sum2)

gam_3 <- qgam(log1p(FRP) ~ FBI + VEG_GROUP, qu = 0.8, data = df_af_sum2)

gam_4 <- qgam(log1p(FRP) ~ FBI + SFL + VEG_GROUP, qu = 0.8, data = df_af_sum2)

gam_5 <- qgam(log1p(FRP) ~ FBI * SFL + VEG_GROUP, qu = 0.8, data = df_af_sum2)

gam_6 <- qgam(log1p(FRP) ~ s(FBI, k=3), qu = 0.8, data = df_af_sum2)

gam_7 <- qgam(log1p(FRP) ~ s(FBI, k=3) + SFL, qu = 0.8, data = df_af_sum2)

gam_8 <- qgam(log1p(FRP) ~ s(FBI, k=3) + VEG_GROUP, qu = 0.8, data = df_af_sum2)

gam_9 <- qgam(log1p(FRP) ~ s(FBI, k=3) + SFL + VEG_GROUP, qu = 0.8, data = df_af_sum2)

gam_10 <- qgam(log1p(FRP) ~ s(FBI, k=3) * SFL + VEG_GROUP, qu = 0.8, data = df_af_sum2)

aic_1 <- AIC(gam_1)
aic_2 <- AIC(gam_2)
aic_3 <- AIC(gam_3)
aic_4 <- AIC(gam_4)
aic_5 <- AIC(gam_5)
aic_6 <- AIC(gam_6)
aic_7 <- AIC(gam_7)
aic_8 <- AIC(gam_8)
aic_9 <- AIC(gam_9)


aic_tab <- as.data.frame(rbind(aic_1, aic_2, aic_3, aic_4, aic_5, aic_6, aic_7, aic_8, aic_9), row.names = c("log(FRP) ~ FBI", "log(FRP) ~ FBI + SFL", "log(FRP) ~ FBI + Veg_Group" , "log(FRP) ~ FBI + SFL + Veg_Group", "log(FRP) ~ FBI * SFL + Veg_Group", "log(FRP) ~ s(FBI)", "log(FRP) ~ s(FBI) + SFL", "log(FRP) ~ s(FBI) + Veg_Group" , "log(FRP) ~ s(FBI) + SFL + Veg_Group"))
write.csv(aic_tab, file = "formal model selection/RESULTS/aictab_FBI(QGAM).csv")

