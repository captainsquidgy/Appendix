#matched subset method 
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
library(ggmap)

setwd('C:/Users/zmajor/OneDrive - University of Tasmania/zac')
df <- read.table("afdr_dataframe4.csv", header = TRUE, sep = ",", row.names =  NULL)

#then constrain it for summer (november to march)

df_af_nd <- df[month(df$date) > 10,]
df_af_jf <- df[month(df$date) < 3, ]
df_af_sum2 <- rbind(df_af_nd, df_af_jf)

df_af_march <- df[month(df$date) == 3, ]


sum(is.na(df_af_sum2$FRP))
df_af_sum2 <- df_af_sum2[is.finite(df_af_sum2$FRP), ]

#subset for vegetation type
table(out_d$VEG_GROUP)
df_af_sum2 <- df_af_sum2[df_af_sum2$VEG_GROUP == "Dry eucalypt forest and woodland",]
df_af_sum2 <- df_af_sum2[df_af_sum2$VEG_GROUP == "Moorland, sedgeland and rushland",]
df_af_sum2 <- df_af_sum2[df_af_sum2$VEG_GROUP == "Scrub, heathland and coastal complexes",]
df_af_sum2 <- df_af_sum2[df_af_sum2$VEG_GROUP == "Wet eucalypt forest and woodland",]
df_af_sum2 <- df_af_sum2[df_af_sum2$VEG_GROUP == "Modified land",]


dryeuc <- out_d
moor <- out_d
scrub <- out_d
weteuc <- out_d
mod <- out_d

out_d <- dryeuc
out_d <- moor
out_d <- scrub
out_d <- weteuc
out_d <- mod


###################
#determine quantiles based on FBI and FFDI separately

#FBI
out <- list()

for (i in 1:50){
  print(i)
  
  df_roll <- df_af_sum2[df_af_sum2$FBI == i,]
  
  df_roll <- df_roll[df_roll$FRP > quantile(df_roll$FRP, probs=0.80, na.rm = TRUE),]
  
  out[[i]] <- df_roll
  
}

out_data <- do.call(rbind,out)
out_d_fbi <- as.data.frame(out_data)

sum(is.na(out_d_fbi$FRP))

#FFDI
out <- list()

for (i in 1:50){
  print(i)
  
  df_roll <- df_af_sum2[df_af_sum2$FFDI > i-1 & df_af_sum2$FFDI < i,]
  
  df_roll <- df_roll[df_roll$FRP > quantile(df_roll$FRP, probs=0.80, na.rm=TRUE),]
  
  out[[i]] <- df_roll
  
}

out_data <- do.call(rbind,out)
out_d_ffdi <- as.data.frame(out_data)

sum(is.na(out_d_ffdi$FRP))
out_d_ffdi <- out_d_ffdi[is.finite(out_d_ffdi$FRP), ]


#for each bucket, select overlapping data points that fall within 
#desired quantile for both FBI and FFDI

common_obs <- intersect(rownames(out_d_fbi), rownames(out_d_ffdi))

over_fbi <- out_d_fbi[common_obs,]
over_ffdi <- out_d_ffdi[common_obs,]

out_d <- over_fbi


#fit both models on matched subset of data
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

length(coef(gam_q))
as.numeric(logLik(gam_q))


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
  #geom_line(data = ef_q, aes(x = x, y = predicted), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FFDI, y = FRP, color = SFL), alpha=0.82, size=0.88) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Predicted Values of Fire Radiative Power (MW/M^2) from the Forest Fire Danger Index ", x = "Forest Fire Danger Index", y = "Fire Radiative Power (MW/m^2)") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans = "log", limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis", name = "Vegetation Group") +
  scale_color_viridis_c(option = "viridis", name = "Surface Fuel Load (t/ha)") +
  theme_minimal() +
  theme(legend.position = "right")

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FFDI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Veg", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


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
write.csv(aic_tab, file = "formal model selection/RESULTS/aictab_FFDI(match subset).csv")

################################################

gam_1 <- gam(log1p(FRP) ~ SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on fuel load only
gam_2 <- gam(log1p(FRP) ~ FBI, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FBI only
gam_3 <- gam(log1p(FRP) ~ FBI + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FBI and differs between vegetation types
gam_4 <- gam(log1p(FRP) ~ FBI + SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FBI and fuel load, not vegetation type
gam_5 <- gam(log1p(FRP) ~ FBI + SFL + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FBI, fuel load and vegetation type
gam_6 <- gam(log1p(FRP) ~ FBI * SFL + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FBI and an interaction with fuel load, and vegetation type
gam_7 <- gam(log1p(FRP) ~ s(FBI, k=3), family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FBI
gam_8 <- gam(log1p(FRP) ~ s(FBI, k=3) + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FBI
gam_9 <- gam(log1p(FRP) ~ s(FBI, k=3) + SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FBI
gam_10 <- gam(log1p(FRP) ~ s(FBI, k=3) + SFL + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FBI
gam_11 <- gam(log1p(FRP) ~ s(FBI, k=3) * SFL + VEG_GROUP, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FBI
gam_12 <- gam(log1p(FRP) ~ 1, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FBI



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

length(coef(gam_q))
as.numeric(logLik(gam_q))

ef_q <- ggpredict(gam_q, terms = c("SFL"))
ef_q <- ggpredict(gam_q, terms = c("FBI"))
ef_q <- ggpredict(gam_q, terms = c("FBI","VEG_GROUP"))
ef_q <- ggpredict(gam_q, terms = c("FBI","SFL"))
ef_q <- ggpredict(gam_q, terms = c("FBI","SFL","VEG_GROUP"))
ef_q <- ggpredict(gam_q)

plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

ggplot() +
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  labs(title = "Predicted Values FIre Radiative Power", x = "Fire Behaviour Index", y = "Fire Radiative Power") 


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
  #geom_line(data = ef_q, aes(x = x, y = predicted), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FBI, y = FRP, color = SFL), alpha=0.82, size=0.88) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Predicted Values of Fire Radiative Power (MW/M^2) from the Fire Behaviour Index ", x = "Fire Behaviour Index", y = "Fire Radiative Power (MW/m^2)") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(trans = "log", limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis", name = "Vegetation Group") +
  scale_color_viridis_c(option = "viridis", name = "Surface Fuel Load (t/ha)") +
  theme_minimal() +
  theme(legend.position = "right")


ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Veg", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


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
write.csv(aic_tab, file = "formal model selection/RESULTS/aictab_FBI(match subset).csv")


#####################################

gam_1 <- gam(log1p(FRP) ~ SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on fuel load only
gam_2 <- gam(log1p(FRP) ~ FBI, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FBI only
gam_3 <- gam(log1p(FRP) ~ FBI + SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FBI and fuel load, not vegetation type
gam_4 <- gam(log1p(FRP) ~ FBI * SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FBI and an interaction with fuel load, and vegetation type
gam_5 <- gam(log1p(FRP) ~ s(FBI, k = 3), family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FBI
gam_6 <- gam(log1p(FRP) ~ s(FBI, k = 3) + SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FBI
gam_7 <- gam(log1p(FRP) ~ 1, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FBI

gam_q <- gam_1
gam_q <- gam_2
gam_q <- gam_3
gam_q <- gam_4
gam_q <- gam_5
gam_q <- gam_6
gam_q <- gam_7



ef_q <- ggpredict(gam_q)
ef_q <- ggpredict(gam_q, terms = c("SFL"))
ef_q <- ggpredict(gam_q, terms = c("FBI"))
ef_q <- ggpredict(gam_q, terms = c("FBI","SFL"))


plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_q)
AIC(gam_q)

#run diagnostic plots
par(mfrow=c(2,2))
gam.check(gam_q)

fire_col <- c("#932667FF", "#DD513AFF", "#FCA50AFF")

ef_q1 <- ef_q[ef_q$group==9.34, ]
ef_q2 <- ef_q[ef_q$group==13.5, ]
ef_q3 <- ef_q[ef_q$group==17.65, ]

#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted), linewidth = 1) +
  geom_line(data = ef_q2, aes(x = x, y = predicted), linewidth = 1) +
  geom_line(data = ef_q3, aes(x = x, y = predicted), linewidth = 1) +
  
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, show.legend = TRUE) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FBI, y = FRP, color = SFL), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Wet Eucalypt Forest and Woodland", x = "Fire Behaviour Index", y = "Fire Radiative Power (MW/m^2)") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous( limits = c(50, 5500), breaks = c(200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis", name = "95% Confidence Interval") +
  scale_color_viridis_c(option = "viridis", name = "Surface Fuel Load (t/ha) (Observed)") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "lightgrey", color = NA),  
        plot.background = element_rect(fill = "white", color = NA))

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Modified", x = "FBI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


aic_1 <- AIC(gam_1)
aic_2 <- AIC(gam_2)
aic_3 <- AIC(gam_3)
aic_4 <- AIC(gam_4)
aic_5 <- AIC(gam_5)
aic_6 <- AIC(gam_6)
aic_7 <- AIC(gam_7)


aic_tab <- as.data.frame(rbind(aic_1, aic_2, aic_3, aic_4, aic_5, aic_6, aic_7), row.names = c("log(FRP) ~ SFL", "log(FRP) ~ FBI", "log(FRP) ~ FBI + SFL", "log(FRP) ~ FBI * SFL", "log(FRP) ~ s(FBI)", "log(FRP) ~ s(FBI) + SFL",  "log(FRP) ~ 1"))
write.csv(aic_tab, file = "formal model selection/RESULTS/aictab_FBI(match subset_mod).csv")


###############################
gam_1 <- gam(log1p(FRP) ~ SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on fuel load only
gam_2 <- gam(log1p(FRP) ~ FFDI, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FFDI only
gam_3 <- gam(log1p(FRP) ~ FFDI + SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FFDI and fuel load, not vegetation type
gam_4 <- gam(log1p(FRP) ~ FFDI * SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # FRP depends on FFDI and an interaction with fuel load, and vegetation type
gam_5 <- gam(log1p(FRP) ~ s(FFDI, k = 3), family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FFDI
gam_6 <- gam(log1p(FRP) ~ s(FFDI, k = 3) + SFL, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FFDI
gam_7 <- gam(log1p(FRP) ~ 1, family = gaussian(link = "log"), data = out_d, method = "ML") # As above, but non-linear relationship with FFDI

gam_q <- gam_1
gam_q <- gam_2
gam_q <- gam_3
gam_q <- gam_4
gam_q <- gam_5
gam_q <- gam_6
gam_q <- gam_7



ef_q <- ggpredict(gam_q)
ef_q <- ggpredict(gam_q, terms = c("SFL"))
ef_q <- ggpredict(gam_q, terms = c("FFDI"))
ef_q <- ggpredict(gam_q, terms = c("FFDI","SFL"))


plot(ef_q, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_q)
AIC(gam_q)

#run diagnostic plots
par(mfrow=c(2,2))
gam.check(gam_q)

ef_q1 <- ef_q[ef_q$group==7.38, ]
ef_q2 <- ef_q[ef_q$group==10.49, ]
ef_q3 <- ef_q[ef_q$group==13.6, ]


#visually inspect model against scatter
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted), linewidth = 1) +
  geom_line(data = ef_q2, aes(x = x, y = predicted), linewidth = 1) +
  geom_line(data = ef_q3, aes(x = x, y = predicted), linewidth = 1) +
  
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, show.legend = TRUE) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FFDI, y = FRP, color = SFL), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Wet Eucalypt Forest and Woodland", x = "Forest Fire Danger Index", y = "Fire Radiative Power (MW/m^2)") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous( limits = c(50, 5500), breaks = c(200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis", name = "95% Confidence Interval") +
  scale_color_viridis_c(option = "viridis", name = "Surface Fuel Load (t/ha) (Observed)") +
  theme_minimal() +
  theme(legend.position = "right", panel.background = element_rect(fill = "lightgrey", color = NA),  
        plot.background = element_rect(fill = "white", color = NA))

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FFDI, y = FRP, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "Modified", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(50, 5500), breaks = c(50,100,200,400,800,1600,3200,6400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


aic_1 <- AIC(gam_1)
aic_2 <- AIC(gam_2)
aic_3 <- AIC(gam_3)
aic_4 <- AIC(gam_4)
aic_5 <- AIC(gam_5)
aic_6 <- AIC(gam_6)
aic_7 <- AIC(gam_7)


aic_tab <- as.data.frame(rbind(aic_1, aic_2, aic_3, aic_4, aic_5, aic_6, aic_7), row.names = c("log(FRP) ~ SFL", "log(FRP) ~ FFDI", "log(FRP) ~ FFDI + SFL", "log(FRP) ~ FFDI * SFL", "log(FRP) ~ s(FFDI)", "log(FRP) ~ s(FFDI) + SFL",  "log(FRP) ~ 1"))
write.csv(aic_tab, file = "formal model selection/RESULTS/aictab_FFDI(match subset_mod).csv")


###############################
ggplot() +
  # Add raw data points
  geom_jitter(data = out_d, aes(x = FFDI, y = FBI, color = as.factor(SFL)), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "All Veg", x = "FFDI", y = "FBI") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")


##################

fireline <- function(FRP, d, FRE){
  FI = ((FRP/FRE) * 10^3)/d
  return(FI)
}

fireline(900, 1000, 0.1)




flfunc <- function(i){
  if(out_d$FRP[i] > 500) {
    front = 1000
    fre = 0.10
    fl <- fireline(out_d$FRP[i], front, fre)
  }else{
    if(out_d$FRP[i] > 400) {
      front = 1000
      fre = 0.11
      fl <- fireline(out_d$FRP[i], front, fre)
    }else{
      if(out_d$FRP[i] > 300) {
        front = 1000
        fre = 0.125
        fl <- fireline(out_d$FRP[i], front, fre)
      }else{
        if(out_d$FRP[i] > 200) {
          front = 1000
          fre = 0.15
          fl <- fireline(out_d$FRP[i], front, fre)
        }else{
          if(out_d$FRP[i] > 100) {
            front = 1000
            fre = 0.20
            fl <- fireline(out_d$FRP[i], front, fre)
          }else{
            front = 1000
            fre = 0.35
            fl <- fireline(out_d$FRP[i], front, fre)
          }
        }
      }
    }
  }
  return(fl)
}

out_d <- over_fbi
out_d <- out_d_fbi

#FFDI
out <- list()

for (i in 1:1649){

  print(i)
  
  fl_out <- flfunc(i)
  
  out[[i]] <- fl_out
  
}

fi_meas <- do.call(rbind,out)
out_d_fi <- cbind(out_d, fi_meas)
out_d <- as.data.frame(out_d_fi)

mod <- gam(intensity ~ s(fi_meas, k = 3), data = out_d)
ef_q <- ggpredict(mod, terms = c("fi_meas"))
summary(mod)

line_df <- data.frame(x = c(400, 51200), y = c(400, 51200))

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = out_d, aes(x = fi_meas, y = intensity, color = SFL), alpha=1, size=1) +
  scale_color_viridis_d(option = "viridis") +
  geom_line(data = line_df, aes(x = x, y = y), colour = "red", linetype = "dashed", size = 1) +
  labs(title = "FRP Measured Fireline Intensity vs FBI Predicted Fireline Intensity", x = "Fireline Intensity from FRP Proxy (kW/m)", y = "Fireline Intensity from FBI Prediction (kW/m)") +
  scale_x_continuous(trans = "log", limits = c(400, 60000), breaks = c(400,800,1600,3200,6400,12800,25600,51200)) + # Customize x-axis
  scale_y_continuous(trans = "log", limits = c(50, 60000), breaks = c(400,800,1600,3200,6400,12800,25600,51200)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis", name = "95% Confidence Interval") +
  scale_color_viridis_c(option = "viridis", name = "SFL (t/ha) (Observed)") +
  #scale_color_manual(values = c("1:1 Line" = "red"), guide = "legend") +
  theme_minimal() +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "#E5E4E2"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"))

ggplot(out_d, aes(x=FRP)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of Fire Radiative Power (MW/m^2)", x = "Fire Radiative Power (MW/m^2)", y = "Density")

ggplot(out_d, aes(x=FBI)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of Fire Behaviour Index", x = "Fire Behaviour Index", y = "Density")

ggplot(out_d, aes(x=FFDI)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  labs(title = "Density Distribution of Forest Fire Danger Index)", x = "Forest Fire Danger Index", y = "Density")


gam_plot <- gam(log1p(FRP) ~ FBI + SFL, family = gaussian(link = "log"), data = df_af_march, method = "ML")
gam_plot <- glm(log1p(FRP) ~ FBI + SFL, data = df_af_march)
ef_q <- ggpredict(gam_plot, terms = c("FBI","SFL"))

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_q, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_q, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  
  # Add raw data points
  geom_jitter(data = df_af_march, aes(x = FBI, y = FRP, color = as.factor(SFL)), alpha=0.7, size=0.81) +
  scale_color_viridis_d(option = "viridis") +
  labs(title = "FBI vs FRP in All Vegetation Groups for March", x = "Fire Behaviour Index (FBI)", y = "Fire Radiative Power (FRP) (MW/m^2)") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 400), breaks = c(50,100,200,400)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal() +
  theme(legend.position = "none")

register_stadiamaps("c1636cef-e5fb-4975-bc95-399ed29424d2", write = FALSE)

bbox <- c(left = 143.5, bottom = -44, right = 149, top = -39.4)
tas <- get_stadiamap(bbox, zoom = 8)
ggmap(tas) + 
  scalebar()
