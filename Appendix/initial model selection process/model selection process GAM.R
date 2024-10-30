#model selection process but with gam

#create basic linear model (GAM)
gam_dryeuc <- gam(FRP ~ fbi*sfl, data = df_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_dryeuc) #GCV = 60255, deviance exp = 4.38%

#run diagnostic plots
gam.check(gam_dryeuc)

#run descdist to find suggested family
descdist(df_dryeuc$FRP)

#create linear model with suggested family
df_dryeuc <- df_dryeuc[df_dryeuc$FRP > 0, ]
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
gam_dryeuc <- gam(FRP ~ fbi*sfl, family = Gamma(link = "log"), data = df_dryeuc, start=start_values)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_dryeuc)

#run diagnostic plots to find suggested transform
gam.check(gam_dryeuc)

#create transformed model
start_values <- c(fbi = 0, sfl = 0, `fbi:sfl` = 0)
gam_dryeuc <- gam(FRP ~ fbi*sfl+0, family = Gamma(link = "log"), data = df_dryeuc, start=start_values)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
test <- subset(ef_gam_dryeuc, ef_gam_dryeuc$x < 40)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#summary (AIC scores)
summary(gam_dryeuc)

#run diagnostic plots to assess fit
gam.check(gam_dryeuc)

#visually inspect model against scatter
df_dryeuc <- df_dryeuc[df_dryeuc$FRP > 0, ]
start_values <- c(Intercept = 1, fbi = 0, sfl = 0, `fbi:sfl` = 0)
gam_dryeuc <- gam(FRP ~ fbi*sfl, family = Gamma(link = "log"), data = df_dryeuc, start=start_values)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_gam_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
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
  geom_line(data = ef_gam_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
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

#treat fbi and sfl as non-linear spline associations
gam_dryeuc <- gam(log1p(FRP) ~ s(fbi,k=3)+s(sfl,k=3), family = gaussian(link = "identity"), data = df_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

gam.check(gam_dryeuc)

#with tensor interaction
gam_dryeuc <- gam(log1p(FRP) ~ s(fbi,k=3) + s(sfl,k=3) + ti(fbi,sfl,k=3), family = gaussian(link = "identity"), data = df_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("fbi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
