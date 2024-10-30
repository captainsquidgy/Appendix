df2 <- read.csv("dataframe.csv")

# Example model - 
# Simple gaussian GLM, with effect of both vegetation group and FFDI
# Log+1 transform the response variable (FRP
mod <- glm(log1p(FRP) ~ VEG_GROUP + ffdi, data=df)

# Display model summary - FFDI has significant effect
summary(mod)

# Good package to visualize statistical model output
# If you don't have it installed, install it first with
# install.packages("ggeffects")
library(ggeffects)
library(viridis)

# Generate the data behind an 'effect plot' showing the effect of a predictor
# variable on the response variable, with error bars
# In this case we want to see two factors, FFDI and VegGroup represented on the plot
ef <- ggpredict(mod,terms=c("ffdi","VEG_GROUP"))

# Define a colour palette to use for the veg group
pal <- colorRampPalette(c("red","orange","yellow","green","blue","purple"))(12)

# Plot the effects
plot(ef,colors=pal)

# Can see that FRP tends to increase with FFDI
# And that there is variation between vegetation groups in the FRP valies

#lets plot a model for all data and sfl groups

df <- df %>%
  mutate(sfl_range20 = cut(sfl, breaks = c(-1,1:20), 
                           labels = c("0-1","1-2","2-3", "3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20")))

mod_all <- glm(log1p(FRP) ~ sfl_range20 + ffdi, data=df)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range20"))
pal_sfl <- colorRampPalette(c("red","yellow", "cyan"))(20)
plot(ef_all, colors=inferno(22), show_ci=FALSE, line_size=1)

#we are seeing a lot of overlap between colour groups, need to separate by veg type and group some fuel loads

df <- df %>%
  mutate(sfl_range10 = cut(sfl, breaks = c(-1,2,4,6,8,10,12,14,16,18,20), 
                           labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20")))

mod_all <- glm(log1p(FRP) ~ sfl_range10 + ffdi, data=df)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range10"))
pal_sfl <- colorRampPalette(c("red","yellow", "cyan"))(10)
plot(ef_all, colors=inferno(11), show_ci=FALSE, line_size=1)


#cut down number of groups again (7)
df <- df %>%
  mutate(sfl_range7 = cut(sfl, breaks = c(-1,3,6,9,12,15,18,21), 
                           labels = c("0-3","3-6","6-9","9-12","12-15","15-18","18-20")))

mod_all <- glm(log1p(FRP) ~ sfl_range7 + ffdi, data=df)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range7"))
pal_sfl <- colorRampPalette(c("red","yellow", "cyan"))(10)
plot(ef_all, colors=inferno(8), show_ci=FALSE, line_size=1)

#cut down number of groups again (5)
df <- df %>%
  mutate(sfl_range5 = cut(sfl, breaks = c(-1,4,8,12,16,20), 
                          labels = c("0-4","4-8","8-12","12-16","16-20")))

mod_all <- glm(log1p(FRP) ~ sfl_range5 + ffdi, data=df)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range5"))
pal_sfl <- colorRampPalette(c("red","yellow", "cyan"))(10)
plot(ef_all, colors=inferno(6), show_ci=FALSE, line_size=1)

#cut down number of groups again
df <- df %>%
  mutate(sfl_range4 = cut(sfl, breaks = c(-1,5,10,15,20), 
                          labels = c("0-5","5-10","10-15","15-20")))

mod_all <- glm(log1p(FRP) ~ sfl_range4 + ffdi, data=df)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range4"))
pal_sfl <- colorRampPalette(c("red","yellow", "cyan"))(10)
plot(ef_all, colors=inferno(6), show_ci=FALSE, line_size=1)

#cut down number of groups again
df <- df %>%
  mutate(sfl_range3 = cut(sfl, breaks = c(-1,7,14,21), 
                          labels = c("0-7","7-14","14-21")))

mod_all <- glm(log1p(FRP) ~ sfl_range3 + ffdi, data=df)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range3"))
pal_sfl <- colorRampPalette(c("red","yellow", "cyan"))(10)
plot(ef_all, colors=inferno(4), show_ci=TRUE, line_size=1)



#dryeuc
df_dryeuc <- df_dryeuc %>%
  mutate(sfl_range10 = cut(sfl, breaks = c(-1,1:10), 
                           labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20")))

mod_dryeuc <- glm(log1p(FRP) ~ sfl_range10 + ffdi, data=df_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl_range10"))
pal_sfl <- colorRampPalette(c("red","cyan"))(10)
plot(ef_dryeuc, colors=plasma(10), show_ci=FALSE, line_size=1)

#large outlier for one of the sfl groups makes it hard to assess the rest, have a feeling the sfl data has a skew of "zero sfl" points that arent zero
#model some other fuel types

#weteuc
df_weteuc <- df_weteuc %>%
  mutate(sfl_range20 = cut(sfl, breaks = c(-1,1:20), 
                           labels = c("0-1","1-2","2-3", "3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20")))

mod_weteuc <- glm(log1p(FRP) ~ sfl_range20 + ffdi, data=df_weteuc)
ef_weteuc <- ggpredict(mod_weteuc, terms=c("ffdi","sfl_range20"))
pal_sfl <- colorRampPalette(c("red","blue"))(20)
plot(ef_weteuc, colors=pal_sfl, show_ci=FALSE)


#mod
df_mod <- df_mod %>%
  mutate(sfl_range10 = cut(sfl, breaks = c(-1,1:10), 
                           labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20")))

mod_mod <- glm(log1p(FRP) ~ sfl_range10 + ffdi, data=df_mod)
ef_mod <- ggpredict(mod_mod, terms=c("ffdi","sfl_range10"))
pal_sfl <- colorRampPalette(c("red","blue"))(16)
plot(ef_mod, colors=inferno(10), show_ci=FALSE, line_size=1)

#moorland
df_moor <- df_moor %>%
  mutate(sfl_range10 = cut(sfl, breaks = c(-1,1:10), 
                           labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20")))

mod_moor <- glm(log1p(FRP) ~ sfl_range10 + ffdi, data=df_moor)
ef_moor <- ggpredict(mod_moor, terms=c("ffdi","sfl_range10"))
plot(ef_moor, colors=inferno(11), show_ci=FALSE, line_size=1)

#scrub
df_scrub <- df_scrub %>%
  mutate(sfl_range10 = cut(sfl, breaks = c(-1,1:10), 
                           labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20")))

mod_scrub <- glm(log1p(FRP) ~ sfl_range10 + ffdi, data=df_scrub)
ef_scrub <- ggpredict(mod_scrub, terms=c("ffdi","sfl_range10"))
plot(ef_scrub, colors=inferno(11), show_ci=FALSE, line_size=1)


#rain
df_rain <- df_rain %>%
  mutate(sfl_range10 = cut(sfl, breaks = c(-1,1:10), 
                           labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20")))

mod_rain <- glm(log1p(FRP) ~ sfl_range10 + ffdi, data=df_rain)
ef_rain <- ggpredict(mod_rain, terms=c("ffdi","sfl_range10"))
plot(ef_rain, colors=inferno(11), show_ci=FALSE, line_size=1)

#grass
df_grass <- df_grass %>%
  mutate(sfl_range10 = cut(sfl, breaks = c(-1,1:10), 
                           labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20")))

mod_grass <- glm(log1p(FRP) ~ sfl_range10 + ffdi, data=df_grass)
ef_grass <- ggpredict(mod_grass, terms=c("ffdi","sfl_range10"))
plot(ef_grass, colors=inferno(5), show_ci=FALSE, line_size=1)

#the sfl might be wrong, how do we check this
#lets plot all the sfl charts side by side
png()
plot(sfl_2001)
plot(sfl_2002)
plot(sfl_2003)
plot(sfl_2004)
plot(sfl_2005)
plot(sfl_2006)
plot(sfl_2007)
plot(sfl_2008)
plot(sfl_2009)
plot(sfl_2010)
plot(sfl_2011)
plot(sfl_2012)
plot(sfl_2013)
plot(sfl_2014)
plot(sfl_2015)
plot(sfl_2016)
plot(sfl_2017)
plot(sfl_2018)
plot(sfl_2019)
dev.off()


#got a feeling the sfl data is out by a year so going to create new dataframe with 2 years ago sfl and attach 1pm ffdi times

for (i in 1:length(data_join)) {
  #we find the year on the hotspot, but we need last years sfl (maybe even 2 years ago) 
  last_year <- year(data_join$date[i])-2
  print(i)
  
  #which layer in combined_sfl has this year
  com_sfl_index <- which(names(combined_sfl) == last_year)
  
  #then we use this index to find the right surface fuel load at this hotspot
  ex_sfl <- terra::extract(combined_sfl[[com_sfl_index]], data_join[i],bind=TRUE)
  
  #lets call it sfl
  names(ex_sfl)[6]<- "sfl"
  
  #save this to a list
  out[[i]]=ex_sfl
}

data_j_sfl2 <- do.call(rbind,out)

df2 <- as.data.frame(data_j_sfl2)

#now we gotta get ffdi1pm in there

df2$ffdi <- df$ffdi

#lets remove the NA points here
df2 <- df2 %>% filter(!is.na(ffdi))

#lets look at all data with lines for sfl
df2 <- df2 %>%
  mutate(sfl_range20 = cut(sfl, breaks = c(-1,1:20), 
                           labels = c("0-1","1-2","2-3", "3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20")))

mod_all <- glm(log1p(FRP) ~ sfl_range20 + ffdi, data=df2)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range20"))
plot(ef_all, colors=inferno(22), show_ci=FALSE, line_size=1)

#cut down groups (10)
df2 <- df2 %>%
  mutate(sfl_range10 = cut(sfl, breaks = c(-1,2,4,6,8,10,12,14,16,18,20), 
                           labels = c("0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20")))

mod_all <- glm(log1p(FRP) ~ sfl_range10 + ffdi, data=df2)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range10"))
plot(ef_all, colors=inferno(11), show_ci=FALSE, line_size=1)


#cut down number of groups again (7)
df2 <- df2 %>%
  mutate(sfl_range7 = cut(sfl, breaks = c(-1,3,6,9,12,15,18,21), 
                          labels = c("0-3","3-6","6-9","9-12","12-15","15-18","18-20")))

mod_all <- glm(log1p(FRP) ~ sfl_range7 + ffdi, data=df2)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range7"))
plot(ef_all, colors=inferno(8), show_ci=FALSE, line_size=1)

#cut down number of groups again (5)
df2 <- df2 %>%
  mutate(sfl_range5 = cut(sfl, breaks = c(-1,4,8,12,16,20), 
                          labels = c("0-4","4-8","8-12","12-16","16-20")))

mod_all <- glm(log1p(FRP) ~ sfl_range5 + ffdi, data=df2)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range5"))
plot(ef_all, colors=inferno(6), show_ci=FALSE, line_size=1)

#cut down number of groups again
df2 <- df2 %>%
  mutate(sfl_range4 = cut(sfl, breaks = c(-1,5,10,15,20), 
                          labels = c("0-5","5-10","10-15","15-20")))

mod_all <- glm(log1p(FRP) ~ sfl_range4 + ffdi, data=df2)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range4"))
plot(ef_all, colors=inferno(5), show_ci=FALSE, line_size=1)

#cut down number of groups again
df2 <- df2 %>%
  mutate(sfl_range3 = cut(sfl, breaks = c(-1,7,14,21), 
                          labels = c("0-7","7-14","14-21")))

mod_all <- glm(log1p(FRP) ~ sfl_range3 + ffdi, data=df2)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl_range3"))
plot(ef_all, colors=inferno(4), show_ci=TRUE, line_size=1)
summary(mod_all)

#now we do this but for veg type

#dry euc (10)
df2_dryeuc <- subset(df2,df2$VEG_GROUP == "Dry eucalypt forest and woodland")

mod_dryeuc <- glm(log1p(FRP) ~ sfl_range10 + ffdi, data=df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl_range10"))
plot(ef_dryeuc, colors=inferno(11), show_ci=FALSE, line_size=1)

#dry euc (7)
mod_dryeuc <- glm(log1p(FRP) ~ sfl_range7 + ffdi, data=df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl_range7"))
plot(ef_dryeuc, colors=inferno(8), show_ci=FALSE, line_size=1)

#dry euc (5)
mod_dryeuc <- glm(log1p(FRP) ~ sfl_range5 + ffdi, data=df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl_range5"))
plot(ef_dryeuc, colors=inferno(6), show_ci=FALSE, line_size=1)

#dry euc (4)
mod_dryeuc <- glm(log1p(FRP) ~ sfl_range4 + ffdi, data=df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl_range4"))
plot(ef_dryeuc, colors=inferno(5), show_ci=FALSE, line_size=1)

#dry euc (3)
mod_dryeuc <- glm(log1p(FRP) ~ sfl_range3 + ffdi, data=df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl_range3"))
plot(ef_dryeuc, colors=inferno(4), show_ci=FALSE, line_size=1)

#grant strongly reccomends treating fuel load as continuous variable sooo
mod_all <- glm(log1p(FRP) ~ sfl + ffdi, data=df2)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl"))
plot(ef_all)

#lets do this continuous sfl for all veg types
#dry euc
mod_dryeuc <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#wet euc
df2_weteuc <- subset(df2, df2$VEG_GROUP == "Wet eucalypt forest and woodland")

mod_weteuc <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_weteuc)
ef_weteuc <- ggpredict(mod_weteuc, terms=c("ffdi","sfl"))
plot(ef_weteuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#modified
df2_mod <- subset(df2, df2$VEG_GROUP == "Modified land")

mod_mod <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_mod)
ef_mod <- ggpredict(mod_mod, terms=c("ffdi","sfl"))
plot(ef_mod, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#grass
df2_grass <- subset(df2, df2$VEG_GROUP == "Native grassland")

mod_grass <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_grass)
ef_grass <- ggpredict(mod_grass, terms=c("ffdi","sfl"))
plot(ef_grass, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#moorland
df2_moor <- subset(df2, df2$VEG_GROUP == "Moorland, sedgeland and rushland")

mod_moor <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_moor)
ef_moor <- ggpredict(mod_moor, terms=c("ffdi","sfl"))
plot(ef_moor, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#scrub
df2_scrub <- subset(df2, df2$VEG_GROUP == "Scrub, heathland and coastal complexes")

mod_scrub <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_scrub)
ef_scrub <- ggpredict(mod_scrub, terms=c("ffdi","sfl"))
plot(ef_scrub, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#rain
df2_rain <- subset(df2, df2$VEG_GROUP == "Rainforest and related scrub")

mod_rain <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_rain)
ef_rain<- ggpredict(mod_rain, terms=c("ffdi","sfl"))
plot(ef_rain, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#highland
df2_high <- subset(df2, df2$VEG_GROUP == "Highland and treeless vegetation")

mod_high <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_high)
ef_high <- ggpredict(mod_high, terms=c("ffdi","sfl"))
plot(ef_high, colors = inferno(7))

#saltmarsh
df2_salt <- subset(df2, df2$VEG_GROUP == "Saltmarsh and wetland")

mod_salt <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_salt)
ef_salt <- ggpredict(mod_salt, terms=c("ffdi","sfl"))
plot(ef_salt, colors = inferno(10))

#other
df2_other <- subset(df2, df2$VEG_GROUP == "Other natural environments")

mod_other <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_other)
ef_other <- ggpredict(mod_other, terms=c("ffdi","sfl"))
plot(ef_other, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#non euc
df2_noneuc <- subset(df2, df2$VEG_GROUP == "Non eucalypt forest and woodland")

mod_noneuc <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_noneuc)
ef_noneuc <- ggpredict(mod_noneuc, terms=c("ffdi","sfl"))
plot(ef_noneuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))


#experiment with different transformations
mod_all <- glm(log1p(FRP) ~ sfl + ffdi, data=df2)
ef_all <- ggpredict(mod_all, terms=c("ffdi","sfl"))
plot(ef_all)

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_all, aes(x = x, y = predicted, color = group), linewidth = 1) +
  # Add raw data points
  geom_point(data = df2, aes(x = ffdi, y = FRP, color = as.factor(sfl))) +
  labs(title = "Dry Euc", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 1200), breaks = seq(0, 1200, 300)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")


c("#932667FF", "#DD513AFF", "#FCA50AFF")

#lets do this continuous sfl for all veg types
#dry euc
mod_dryeuc <- glm(log1p(FRP) ~ ffdi + sfl, data = df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#this gives us the raw data and model on one plot
ggplot() +
  # Plot the predicted values
  geom_line(data = ef_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_point(data = df2_dryeuc, aes(x = ffdi, y = FRP, color = as.factor(sfl)), alpha=0.9, size=0.9) +
  scale_color_viridis_d(option = "inferno") +
  labs(title = "Dry Euc", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 300)) + # Customize y-axis
  scale_fill_viridis_d(option = "inferno") +
  scale_color_viridis_d(option = "inferno") +
  theme_minimal() +
  theme(legend.position = "none")

#now lets do the transformations
#linear
mod_dryeuc <- glm(FRP ~ ffdi + sfl, data = df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl"))
plot(ef_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#ffdi squared, with interaction from SFL
mod_dryeuc <- glm(FRP ~ I(ffdi^2)*sfl+0, data = df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl"))
test <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

par(mfrow = c(2,2))
plot(mod_dryeuc)

#looks pretty sick
#lets try exponential
mod_dryeuc <- glm(FRP ~ I(exp(ffdi)) + sfl, data = df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl"))
test <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))

#looks shit
#what about cubed for fun
mod_dryeuc <- glm(FRP ~ I(ffdi^3)*sfl, data = df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl"))
test <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
#decent but a bit aggresive, we ask, what is the expected relationship between FFDI and FRP if we go by units?
#getting into physics now so can probably wait untill statistical model is developed

#try scalar with sfl as factor not interaction term (similar to what FFDI was originally designed to do - predict RoS in dry euc with scalar and multiplied by SFL)
mod_dryeuc <- glm(FRP ~ I(ffdi^2*sfl), data = df2_dryeuc)
ef_dryeuc <- ggpredict(mod_dryeuc, terms=c("ffdi","sfl"))
test <- subset(ef_dryeuc, ef_dryeuc$x < 50)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(mod_dryeuc)

#lets see if we can do a GAM
library(mgcv)

#linear
gam_dryeuc <- gam(FRP ~ ffdi + sfl, data = df2_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("ffdi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc)

#log transform, needs interaction
gam_dryeuc <- gam(log1p(FRP) ~ ffdi*sfl, data = df2_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("ffdi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc)
#model summary is kind of shit

#ffdi squared with interaction from sfl
gam_dryeuc <- gam(FRP ~ I(ffdi^2)*sfl + 0, data = df2_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("ffdi","sfl"))
test <- subset(ef_gam_dryeuc, ef_gam_dryeuc$x < 50)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc)

#ffdi squared with multiplication from sfl
gam_dryeuc <- gam(FRP ~ I(ffdi^2*sfl) + 0, data = df2_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("ffdi","sfl"))
test <- subset(ef_gam_dryeuc, ef_gam_dryeuc$x < 50)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc)

#scalar
gam_dryeuc <- gam(FRP ~ I(ffdi*sfl), data = df2_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("ffdi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc)

#cubed
gam_dryeuc <- gam(FRP ~ I(ffdi^3)*sfl + 0, data = df2_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("ffdi","sfl"))
test <- subset(ef_gam_dryeuc, ef_gam_dryeuc$x < 50)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc)

#interaction
gam_dryeuc <- gam(FRP ~ ffdi*sfl + 0, data = df2_dryeuc)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("ffdi","sfl"))
plot(ef_gam_dryeuc, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc)

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_gam_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_point(data = df2_dryeuc, aes(x = ffdi, y = FRP, color = as.factor(sfl)), alpha=1, size=1.5) +
  scale_color_viridis_d(option = "inferno") +
  labs(title = "Dry Euc", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2200), breaks = seq(0, 2200, 250)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "inferno") +
  theme_minimal() +
  theme(legend.position = "none")

#can we filter for non prescribed burns?
df2_dryeuc_npb <- subset(df2_dryeuc, month(df2_dryeuc$date) <  03)
df2_dryeuc_npb2 <- subset(df2_dryeuc, month(df2_dryeuc$date) > 08)
df2_dryeuc_n <- rbind(df2_dryeuc_npb,df2_dryeuc_npb2)

ggplot() +
  # Plot the predicted values
  geom_line(data = ef_gam_dryeuc, aes(x = x, y = predicted, color = group), linewidth = 1) +
  geom_ribbon(data = ef_gam_dryeuc, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  # Add raw data points
  geom_point(data = df2_dryeuc_n, aes(x = ffdi, y = FRP, color = as.factor(sfl)), alpha=0.9, size=0.9) +
  scale_color_viridis_d(option = "inferno") +
  labs(title = "Dry Euc", x = "FFDI", y = "FRP") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 100)) + # Customize y-axis
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "inferno") +
  theme_minimal() +
  theme(legend.position = "none")

gam_dryeuc <- gam(FRP ~ I(ffdi^2)*sfl + 0, data = df2_dryeuc_n)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("ffdi","sfl"))
test <- subset(ef_gam_dryeuc, ef_gam_dryeuc$x < 50)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc)

gam_dryeuc <- gam(FRP ~ I(ffdi^2*sfl) + 0, data = df2_dryeuc_n)
ef_gam_dryeuc <- ggpredict(gam_dryeuc, terms = c("ffdi","sfl"))
test <- subset(ef_gam_dryeuc, ef_gam_dryeuc$x < 50)
plot(test, colors = c("#932667FF", "#DD513AFF", "#FCA50AFF"))
summary(gam_dryeuc)
