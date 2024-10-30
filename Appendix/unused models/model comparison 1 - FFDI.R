#model comparison 1 - FFDI 
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
df_af_sum2 <- df_af_sum2[is.finite(df_af_sum2$FRP), ]
df_af_sum2 <- df_af_sum2[complete.cases(df_af_sum2$FFDI),]

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
out_d <- out_d[is.finite(out_d$FRP), ]

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

sum(is.na(out_d$FRP))
out_d <- out_d[is.finite(out_d$FRP), ]
out_d$FFDI <- as.numeric(out_d$FFDI)

###############
gam_1 <- gam(log1p(FRP) ~ SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on fuel load only
gam_2 <- gam(log1p(FRP) ~ FFDI, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FFDI only
gam_3 <- gam(log1p(FRP) ~ FFDI + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FFDI and differs between vegetation types
gam_4 <- gam(log1p(FRP) ~ FFDI + SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FFDI and fuel load, not vegetation type
gam_5 <- gam(log1p(FRP) ~ FFDI + SFL + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FFDI, fuel load and vegetation type
gam_6 <- gam(log1p(FRP) ~ FFDI * SFL + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FFDI and an interaction with fuel load, and vegetation type
gam_7 <- gam(log1p(FRP) ~ s(FFDI, k = 3), family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FFDI
gam_8 <- gam(log1p(FRP) ~ s(FFDI, k = 3) + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FFDI
gam_9 <- gam(log1p(FRP) ~ s(FFDI, k = 3) + SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FFDI
gam_10 <- gam(log1p(FRP) ~ s(FFDI, k = 3) + SFL + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FFDI
gam_11 <- gam(log1p(FRP) ~ s(FFDI, k = 3) * SFL + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FFDI
gam_12 <- gam(log1p(FRP) ~ 1, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FFDI



aic_1 <- AIC(gam_1)
aic_2 <- AIC(gam_2)
aic_3 <- AIC(gam_3)
aic_4 <- AIC(gam_4)
aic_5 <- AIC(gam_5)
aic_6 <- AIC(gam_6)
aic_7 <- AIC(gam_7)
aic_8 <- AIC(gam_8)
aic_9 <- AIC(gam_9)
aic_10 <- AIC(gam_10)
aic_11 <- AIC(gam_11)
aic_12 <- AIC(gam_12)

aic_tab <- as.data.frame(rbind(aic_1, aic_2, aic_3, aic_4, aic_5, aic_6, aic_7, aic_8, aic_9, aic_10, aic_12), row.names = c("log(FRP) ~ SFL", "log(FRP) ~ FFDI", "log(FRP) ~ FFDI  + Veg_Group", "log(FRP) ~ FFDI + SFL", "log(FRP) ~ FFDI + SFL + Veg_Group", "log(FRP) ~ FFDI * SFL + Veg_Group", "log(FRP) ~ s(FFDI)", "log(FRP) ~ s(FFDI)  + Veg_Group", "log(FRP) ~ s(FFDI) + SFL", "log(FRP) ~ s(FFDI) + SFL + Veg_Group", "log(FRP) ~ 1"))
write.csv(aic_tab, file = "formal model selection/RESULTS/aictab_FFDI(FFDI quantile).csv")


gam_q <- gam_1
gam_q <- gam_2
gam_q <- gam_3
gam_q <- gam_4
gam_q <- gam_5
gam_q <- gam_6
gam_q <- gam_7
gam_q <- gam_8
gam_q <- gam_9
gam_q <- gam_10
gam_q <- gam_11
gam_q <- gam_12

ef_q <- ggpredict(gam_q)
ef_q <- ggpredict(gam_q, terms = c("SFL"))
ef_q <- ggpredict(gam_q, terms = c("FFDI"))
ef_q <- ggpredict(gam_q, terms = c("FFDI","SFL"))
ef_q <- ggpredict(gam_q, terms = c("FFDI","VEG_GROUP"))
ef_q <- ggpredict(gam_q, terms = c("FFDI","SFL","VEG_GROUP"))

plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))


my_colors <- colorRampPalette(c("brown", "#932667FF", "#DD513AFF", "#FCA50AFF", "yellow"))
color_palette <- my_colors(11)
plot(ef_q, colors = color_palette)


#summary (AIC scores)
summary(gam_q)
AIC(gam_q)

#run diagnostic plots
par(mfrow=c(2,2))
gam.check(gam_q)

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FFDI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Veg", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans = "log",  limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")
