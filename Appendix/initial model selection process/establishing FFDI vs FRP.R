#we want to build a model of predicting FRP with FFDI or AFDR controlling for vegtype and fuel load
#we will start with FFDI so..
#FRP ~ FFDI + vegtype + fuel load

#to establish statistical relationship we must plot FRP against FFDI for all of tas

#packages
library(terra)
library(lubridate)
library(tidyverse)
library(quantreg)

#load data
setwd('C:/Users/zmajor/OneDrive - University of Tasmania/zac')
data_join <- vect("hs_ftp_joined.gpkg")
sfl_2000 <- rast("SFL/SFL_2000.tif")
sfl_2001 <- rast("SFL/SFL_2001.tif")
sfl_2002 <- rast("SFL/SFL_2002.tif")
sfl_2003 <- rast("SFL/SFL_2003.tif")
sfl_2004 <- rast("SFL/SFL_2004.tif")
sfl_2005 <- rast("SFL/SFL_2005.tif")
sfl_2006 <- rast("SFL/SFL_2006.tif")
sfl_2007 <- rast("SFL/SFL_2007.tif")
sfl_2008 <- rast("SFL/SFL_2008.tif")
sfl_2009 <- rast("SFL/SFL_2009.tif")
sfl_2010 <- rast("SFL/SFL_2010.tif")
sfl_2011 <- rast("SFL/SFL_2011.tif")
sfl_2012 <- rast("SFL/SFL_2012.tif")
sfl_2013 <- rast("SFL/SFL_2013.tif")
sfl_2014 <- rast("SFL/SFL_2014.tif")
sfl_2015 <- rast("SFL/SFL_2015.tif")
sfl_2016 <- rast("SFL/SFL_2016.tif")
sfl_2017 <- rast("SFL/SFL_2017.tif")
sfl_2018 <- rast("SFL/SFL_2018.tif")
sfl_2019 <- rast("SFL/SFL_2019.tif")


#dates are numeric so fix that and remove other date columns
data_join$date <- as.Date(as.POSIXct(as.numeric(data_join$date),origin="1970-01-01"))

print(data_join)
data_join <- cbind(data_join[,1:4], data_join[,7])
print(data_join)

#only have sfl data to 2000 so we need to filter data_join so for loop doesnt get errors
data_join <- subset(data_join,year(data_join$date) > 2000)
range(data_join)

#we want to join surface fuel load data to this dataframe
#we will do this by tagging each hotspot with the previous years data

#need an empty list for output data
out <- list()

sfl_rasters <- list()
sfl_rasters[[1]] <- sfl_2000
sfl_rasters[[2]] <- sfl_2001
sfl_rasters[[3]] <- sfl_2002
sfl_rasters[[4]] <- sfl_2003
sfl_rasters[[5]] <- sfl_2004
sfl_rasters[[6]] <- sfl_2005
sfl_rasters[[7]] <- sfl_2006
sfl_rasters[[8]] <- sfl_2007
sfl_rasters[[9]] <- sfl_2008
sfl_rasters[[10]] <- sfl_2009
sfl_rasters[[11]] <- sfl_2010
sfl_rasters[[12]] <- sfl_2011
sfl_rasters[[13]] <- sfl_2012
sfl_rasters[[14]] <- sfl_2013
sfl_rasters[[15]] <- sfl_2014
sfl_rasters[[16]] <- sfl_2015
sfl_rasters[[17]] <- sfl_2016
sfl_rasters[[18]] <- sfl_2017
sfl_rasters[[19]] <- sfl_2018
sfl_rasters[[20]] <- sfl_2019


combined_sfl <- rast(sfl_rasters)
print(combined_sfl)
names(combined_sfl) <- paste0(2000:2019)
print(names(combined_sfl))

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

data_j_sfl <- do.call(rbind,out)

df <- as.data.frame(data_j_sfl)

#lets remove the NA points here
df <- df %>% filter(!is.na(ffdi))

#now we have a data frame with all variables attached 

#have recieved ffdi at 1pm so lets insert that
ffdi1pm <- rast("ffdi_1pm/ffdi_1pm.tif")

#need to attach to dataframe, so fix the date to be Homebart time
timelist1pm <- time(ffdi1pm)
print(timelist1pm)

format(timelist1pm,tz="Australia/Hobart")

# We could also convert them to dates
datelist1pm <- as.Date(format(timelist1pm,tz="Australia/Hobart"))
print(datelist1pm)

#list to extract data to
out1 <- list()

for (i in 1:length(data_j_sfl)) {
#so we need the date in vector data frame
  date_1pm <- data_j_sfl$date[i]
  print(i)

  #find out which layer of 1pm data has this date
  ffdi1pm_layer <- which(datelist1pm == date_1pm)

  #subset 1pm stack to get this date
  ss_ffdi1pm <- subset(ffdi1pm, ffdi1pm_layer)

  #extract ffdi value from that day for that point
  ex1 <- terra::extract(ss_ffdi1pm, data_j_sfl[i], bind=TRUE)

  #better name
  names(ex1)[7]<- "ffdi_1pm"

  out1[[i]]=ex1
}

#now we have data_j_sfl with new ffdi data as ffdi_1pm

#bind em
out1_data <- do.call(rbind,out1)

#now we have spatial data frame with both ffdi values, lets replace ffdi with ffdi_1pm and run rest of code
data_j_1pm <- out1_data

#replace ffdi with ffdi_1pm and run rest of code
data_j_1pm$ffdi <- data_j_1pm$ffdi_1pm
data_j_1pm <- data_j_1pm[,1:6]
data_j_1pm

#convert dataframe
df <- as.data.frame(data_j_1pm)

write.csv(df, file = "dataframe.csv", row.names = FALSE)

#to establish statistical relation we plot FRP against FFDI

#plot FFDI vs FRP for Tas
ggplot(df, aes(x=ffdi, y=FRP)) + 
  geom_point()

#so we have exponential curve with large block of points with low ffdi and high FRP, I think these are
#fires from the previous day still burning but ffdi for next day has gone down significantly
#need to filter to remove points that correspond to day after peak ffdi
#lets take a close look at data from one year, use 2013

d_2013 <- subset(data_j_sfl, year(data_j_sfl$date) == 2013)


#this gives us a hotspot map
col_p <- colorRampPalette(c("yellow","orange","red","purple"))(10)
plot(d_2013,"FRP",col=col_p,breaks=c(0,10,20,50,100,500,1000,5000),alpha=0.6,cex=0.5)



#this gives us a scaled ffdi to compare (comparison only approx)
scale_ffdi_to_frp <- function(ffdi) {
  ffdi * (5000 / 25)
}
df_2013$scaled_ffdi <- scale_ffdi_to_frp(df_2013$ffdi)

df_2013 <- as.data.frame(d_2013)

#plots FRP and FFDI against date for comparison, includes all locations
ggplot(df_2013, aes(x = date)) +
  geom_point(aes(y = FRP, color = "FRP"), size=1, position=position_nudge(x=0.25)) +
  geom_point(aes(y = scaled_ffdi, color = "FFDI"), size=1, position=position_nudge(x=-0.25)) +
  scale_y_continuous(
    name = "FRP",
    sec.axis = sec_axis(~ .*(25/5000), name = "FFDI", breaks = seq(0, 35, by = 5))
  ) +
  labs(title = "FRP and FFDI over Time",
       x = "Date") +
  scale_color_manual(name = "Legend", values = c("FRP" = "blue", "FFDI" = "red")) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red")
  )

#this shows us that setting a linear scalar to FRP ~ FFDI anchored at the peak FRP for dunalley, consistently overpredicts FRP
#likely an exponential model, so we must filter data for values after peak
#at lower values we can approximate an exponential curve pretty closely with a linear line, so we can filter above this line
#define this line to go from (0,0) to (15,500)
500/15
#33.333 so FRP = 33.33*FFDI
#we want to filter above this line

m <- 5000/25

m <- 33

#df_2013_filt <- df_2013[df_2013$FRP <= m * df_2013$ffdi | df_2013$ffdi > 15,]

#ggplot(df_2013_filt, aes(x=ffdi, y=FRP)) + 
  #geom_point()

#take a closer look at where this line should be
#so lets filter for

#lets plot this with data for all of tassie
df_filt <- df[df$FRP <= m * df$ffdi | df$ffdi > 15,]

ggplot(df_filt, aes(x=ffdi, y=FRP)) + 
  geom_point()

#now we fit a curve to the top of this data

fit_exponential_curve <- function(data) {
  # Use the upper quantile to fit the model
  quantile_level <- 0.999
  upper_quantile <- quantile(df_filt$FRP, probs = quantile_level)
  
  # Filter data to only include points near the upper boundary
  upper_data <- df_filt[df_filt$FRP >= upper_quantile, ]
  
  # Fit an exponential model to the upper data
  model <- nls(FRP ~ a * ffdi ^ b, data = upper_data, start = list(a = 5, b = 3))
  
  return(model)
}

model <- fit_exponential_curve(data_filt)

#predict values using fitted model
ffdi_range <- seq(min(df_filt$ffdi), max(df_filt$ffdi), length.out = 1000)
predicted_frp <- predict(model, newdata = data.frame(ffdi = ffdi_range))

# Create a data frame for the predicted values
predicted_data <- data.frame(ffdi = ffdi_range, FRP = predicted_frp)

#plot it
ggplot(df_filt, aes(x = ffdi, y = FRP)) +
  geom_point(color = "blue") +
  geom_line(data = predicted_data, aes(x = ffdi, y = FRP), color = "red", linetype = "dashed") +
  labs(title = "Scatter Plot with Power Curve Fit",
       x = "FFDI",
       y = "FRP")

model



#lets filter with an power curve instead
n <- 0.3
p <- 3.1

df_filt_sq <- df[df$FRP <= n * df$ffdi ^ p | df$ffdi > 15,]

#ggplot(df_filt_sq, aes(x=ffdi, y=FRP)) + 
 # geom_point()

#now fit a model to it
#now we fit a curve to the top of this data

fit_exponential_curve <- function(data) {
  # Use the upper quantile to fit the model
  quantile_level <- 0.99
  upper_quantile <- quantile(df_filt_sq$FRP, probs = quantile_level)
  
  # Filter data to only include points near the upper boundary
  upper_data <- df_filt_sq[df_filt_sq$FRP >= upper_quantile, ]
  
  # Fit an exponential model to the upper data
  model <- nls(FRP ~ a * ffdi ^ b, data = upper_data, start = list(a = 2, b = 2))
  
  return(model)
}

model <- fit_exponential_curve(data_filt)

#predict values using fitted model
ffdi_range <- seq(min(df_filt_sq$ffdi), max(df_filt_sq$ffdi), length.out = 100)
predicted_frp <- predict(model, newdata = data.frame(ffdi = ffdi_range))

# Create a data frame for the predicted values
predicted_data <- data.frame(ffdi = ffdi_range, FRP = predicted_frp)

#plot it
ggplot(df_filt_sq, aes(x = ffdi, y = FRP)) +
  geom_point(color = "blue") +
  geom_line(data = predicted_data, aes(x = ffdi, y = FRP), color = "red", linetype = "dashed") +
  labs(title = "Scatter Plot with Power Curve Fit",
       x = "FFDI",
       y = "FRP")

model



#here we are going to try and bring the model and filter curves to convergence


#lets filter with an power curve instead
n <- 0.5
p <- 2.201

df_filt_sq <- df[df$FRP <= n * df$ffdi ^ p | df$ffdi > 15,]

#ggplot(df_filt_sq, aes(x=ffdi, y=FRP)) + 
# geom_point()

#now fit a model to it
#now we fit a curve to the top of this data

fit_exponential_curve <- function(data) {
  # Use the upper quantile to fit the model
  quantile_level <- 0.993
  upper_quantile <- quantile(df_filt_sq$FRP, probs = quantile_level)
  
  # Filter data to only include points near the upper boundary
  upper_data <- df_filt_sq[df_filt_sq$FRP >= upper_quantile, ]
  
  # Fit an exponential model to the upper data
  model <- nls(FRP ~ a * ffdi ^ b, data = upper_data, start = list(a = 2, b = 2))
  
  return(model)
}

model <- fit_exponential_curve(data_filt)

#predict values using fitted model
ffdi_range <- seq(min(df_filt_sq$ffdi), max(df_filt_sq$ffdi), length.out = 100)
predicted_frp <- predict(model, newdata = data.frame(ffdi = ffdi_range))

# Create a data frame for the predicted values
predicted_data <- data.frame(ffdi = ffdi_range, FRP = predicted_frp)

#plot it
ggplot(df_filt_sq, aes(x = ffdi, y = FRP)) +
  geom_point(color = "blue") +
  geom_line(data = predicted_data, aes(x = ffdi, y = FRP), color = "red", linetype = "dashed") +
  labs(title = "Scatter Plot with Exponential Curve Fit",
       x = "FFDI",
       y = "FRP")

model

#scatter for each vegetation type FFDI vs FRP
#create table to know what veg types we have
table(df$VEG_GROUP)

#subset for dry eucalypt 

df_dryeuc <- subset(df, df$VEG_GROUP == "Dry eucalypt forest and woodland")
head(df_dryeuc)
ggplot(df_dryeuc, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() +
  labs(title = "Dry Euc") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme(legend.position = "none")

#grassland
df_grass <- subset(df, df$VEG_GROUP == "Native grassland")
head(df_grass)
ggplot(df_grass, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() +
  labs(title = "Grass") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) +  # Customize y-axis
  scale_color_viridis_d() +
  theme(legend.position = "none")

#modified land
df_mod <- subset(df, df$VEG_GROUP == "Modified land")
head(df_mod)
ggplot(df_mod, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() +
  labs(title = "Modified") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme(legend.position = "none")

#wet eucalpyt
df_weteuc <- subset(df, df$VEG_GROUP == "Wet eucalypt forest and woodland")
head(df_weteuc)
ggplot(df_weteuc, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() + 
  labs(title = "Wet Euc") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme(legend.position = "none")

#moorland
df_moor <- subset(df, df$VEG_GROUP == "Moorland, sedgeland and rushland")
head(df_moor)
ggplot(df_moor, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() + 
  labs(title = "Moorland") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) +  # Customize y-axis
  scale_color_viridis_d() +
  theme(legend.position = "none")


#scrub, heath and coast
df_scrub <- subset(df, df$VEG_GROUP == "Scrub, heathland and coastal complexes")
head(df_scrub)
ggplot(df_scrub, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() + 
  labs(title = "Scrub") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme(legend.position = "none")

#rainforest
df_rain <- subset(df, df$VEG_GROUP == "Rainforest and related scrub")
head(df_rain)
ggplot(df_rain, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() + 
  labs(title = "Rainforest")  +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis 
  scale_color_viridis_d() +
  theme(legend.position = "none")

#non eucalpyt
df_noneuc <- subset(df, df$VEG_GROUP == "Non eucalypt forest and woodland")
head(df_noneuc)
ggplot(df_noneuc, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() + 
  labs(title = "Non Euc") + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme(legend.position = "none")

#saltmarsh and wetland
df_salt <- subset(df, df$VEG_GROUP == "Saltmarsh and wetland")
head(df_salt)
ggplot(df_salt, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() + 
  labs(title = "Salt") + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme(legend.position = "none")

#highland and treeless veg
df_high <- subset(df, df$VEG_GROUP == "Highland and treeless vegetation")
head(df_high)
ggplot(df_high, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() + 
  labs(title = "Highland") + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme(legend.position = "none")

#other natural environments
df_other <- subset(df, df$VEG_GROUP == "Other natural environments")
head(df_other)
ggplot(df_other, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point() + 
  labs(title = "Other Natural") + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) +  # Customize y-axis
  scale_color_viridis_d() +
  theme(legend.position = "none")

#inspection of the plots reveal similar shape in most vegetation types
#a steep increase in FRP, then a less steep decrease before a steep increase again in the higher range of FFDI

#now we attempt to account for high fuel load
head(df)

#question is, does FRP increase faster with higher fuel load
#could subset sfl for buckets or use colour shading for sfl=1,2,3,...
#buckets is simpler so we will try that first

df_sfl1 <- subset(df, df$sfl < 5)
df_sfl2 <- subset(df, df$sfl >= 5 & df$sfl < 10)
df_sfl3 <- subset(df, df$sfl >= 10 & df$sfl < 15)
df_sfl4 <- subset(df, df$sfl >= 15 & df$sfl < 20)

ggplot(df_sfl1, aes(x=ffdi, y=FRP)) + 
  geom_point() + 
  labs(title = "Surface Fuel Load 0-5")

ggplot(df_sfl2, aes(x=ffdi, y=FRP)) + 
  geom_point() + 
  labs(title = "Surface Fuel Load 5-10")

ggplot(df_sfl3, aes(x=ffdi, y=FRP)) + 
  geom_point() + 
  labs(title = "Surface Fuel Load 10-15")

ggplot(df_sfl4, aes(x=ffdi, y=FRP)) + 
  geom_point() + 
  labs(title = "Surface Fuel Load 15-20")

ggplot(df_sfl1, aes(x=ffdi, y=FRP)) + 
  geom_point() + 
  labs(title = "Surface Fuel Load 0-5")

#this doesn't tell us much so we go by shade

ggplot(df, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point(size = 1, alpha = 1 ) + 
  scale_color_viridis_d() +
  labs(title = "Surface Fuel Load") +
  theme(legend.position = "none")

#we want to assess the impact of next day FFDI on creating that block of data with high FRP low FFDI so single out event and go by day 

d_jan2013 <- subset(data_j_sfl, "2013-01-01" < data_j_sfl$date)
d_jan2013 <- subset(d_jan2013, "2013-01-31" > d_jan2013$date)

bb_dunalley <- ext(147.6229, 148.0184, -43.0685, -42.7532)

d_dun <- crop(d_jan2013, bb_dunalley)
print(d_dun)
col_p <- colorRampPalette(c("yellow","orange","red","purple"))(10)

plot(d_dun,"FRP",col=col_p,breaks=c(0,10,20,50,100,500,1000,5000),alpha=0.6,cex=0.5)
df_dun <- as.data.frame(d_dun)

#plots FFDI vs FRP for dunalley
ggplot(df_dun, aes(x=ffdi, y=FRP)) + 
  geom_point()


#appears that for singular event in dry SE the FFDI predicts well with no skew, upon closer inspection of 2013 FFDI and FRP comparison we arent seeing next day skew so try a few other years to confirm
#current hunch is now that events that occur in varying veg types or with high fuel load is creating our skewed data

df_2005 <- subset(df, "2005-01-01" < df$date)
df_2005 <- subset(df_2005, "2005-12-31" > df_2005$date)

range(df_2005$date)
#this gives us a scaled ffdi to compare (comparison only approx)
scale_ffdi_to_frp <- function(ffdi) {
  ffdi * (3000 / 7.5)
}
df_2005$scaled_ffdi <- scale_ffdi_to_frp(df_2005$ffdi)



#plots FRP and FFDI against date for comparison, includes all locations
ggplot(df_2005, aes(x = date)) +
  geom_point(aes(y = FRP, color = "FRP"), size=1, position=position_nudge(x=0.17)) +
  geom_point(aes(y = scaled_ffdi, color = "FFDI"), size=1, position=position_nudge(x=-0.17)) +
  scale_y_continuous(
    name = "FRP",
    sec.axis = sec_axis(~ .*(7.5/3000), name = "FFDI", breaks = seq(0, 35, by = 5))
  ) +
  labs(title = "FRP and FFDI over Time",
       x = "Date") +
  scale_color_manual(name = "Legend", values = c("FRP" = "blue", "FFDI" = "red")) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red")
  )

#after inspecting 2000-05 it is clear that "next day" has smaller effect than originally thought, effect of fuel load is of greater potential
#lets have a look at these low FFDI high FRP points

m <- 3000/7.5
df_highrp_lowff <- df[df$FRP <= m * df$ffdi,]

ggplot(df_highrp_lowff, aes(x=ffdi, y=FRP, color = as.factor(sfl))) + 
  geom_point(size = 1, alpha = 1 ) + 
  scale_color_viridis_d() +
  labs(title = "Surface Fuel Load") +
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) # Customize y-axis


100*table(df$VEG_GROUP)/13925 
100*table(df_highrp_lowff$VEG_GROUP)/652

#high FRP low FFDI points are disproportionately made up of wet eucalypt, moorland, rainforest, significantly less dry eucalypt
#lets see if they suit next day hypothesis
scale_ffdi_to_frp <- function(ffdi) {
  ffdi * (3000 / 7.5)
}
df_highrp_lowff$scaled_ffdi <- scale_ffdi_to_frp(df_highrp_lowff$ffdi)

df_highrp_lowff_t <- subset(df_highrp_lowff, year(df_highrp_lowff$date) ==  2014)
df_highrp_lowff_t$date

ggplot(df_highrp_lowff_t, aes(x = date)) +
  geom_point(aes(y = FRP, color = "FRP"), size=1, position=position_nudge(x=0.17)) +
  geom_point(aes(y = scaled_ffdi, color = "FFDI"), size=1, position=position_nudge(x=-0.17)) +
  scale_y_continuous(
    name = "FRP",
    sec.axis = sec_axis(~ .*(7.5/3000), name = "FFDI", breaks = seq(0, 35, by = 5))
  ) +
  labs(title = "FRP and FFDI over Time",
       x = "Date") +
  scale_color_manual(name = "Legend", values = c("FRP" = "blue", "FFDI" = "red")) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red")
  )

#after inspecting the months that these high FRP low FFDI values occurred, it is clear that the bulk of them are prescribed burns occurring in april-june
#look at plot of what range of ffdi values we see and how often
dff$ffdi_bin <- cut(dff$ffdi, breaks = seq(0, 25, by = 1), right = FALSE)

ggplot(dff, aes(x = ffdi_bin)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Number of Values for FFDI in Each Integer Range",
       x = "FFDI Range",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
  
#lets look at high ffdi

dff <- df[df$ffdi > 10, ]


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

# We can plot the effects individually too eg.
# Just FFDI
ef <- ggpredict(mod,terms=c("ffdi"))
plot(ef)

# Just veg group - and flip the axes so they are easier to read
ef <- ggpredict(mod,terms=c("VEG_GROUP"))
plot(ef) + coord_flip()


#this was an example, lets do out own below
#with new 1pm ffdi lets have a go as doing a glm and gam 
#one plot of raw data for each veg type, and colour gradient for sfl (8 plots)

#one plot of regression for each veg type, and one line for each sfl, repeat for glm and gam (16 plots) (24 TOTAL)

library(MASS)
library(mgcv)

#start with all data
#next do dry euc
df <- df %>%
  mutate(sfl_range = cut(sfl, breaks = c(-1, 7, 14, 20), 
                         labels = c("0-7","7-14","14-20")))
#linear line anchored at zero
ggplot(df, aes(x=ffdi, y=FRP, color = sfl_range)) +
  geom_point() +
  labs(title = "All Data") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  geom_quantile(quantiles = 0.95, formula = y ~  I(0.01*x) + I(-x^2*0.1) + 0 , method = "rq") 

#try loess
ggplot(df, aes(x=ffdi, y=FRP, color = sfl_range)) +
  geom_point() +
  labs(title = "All Data") + 
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  geom_smooth(method= "loess", span = 0.95, formula = y ~ x + I(x^2))

#we are having problems where outliers for high fuel load are affecting the shape of the plot a lot so we do some subsetting
df_highsfl <- subset(df, df$sfl_range =="14-20")
df_highsfl <- subset(df_highsfl, df_highsfl$ffdi < 11)

ggplot(df_highsfl, aes(x=ffdi, y=FRP, color = sfl_range)) +
  geom_point() +
  labs(title = "All Data") + 
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  geom_quantile(quantiles = 0.95, formula = y ~  x + 0 , method = "rq") 

#keep getting downward curve so go to grant's technique, new script


#next do dry euc
df_dryeuc <- df_dryeuc %>%
  mutate(sfl_range = cut(sfl, breaks = c(-1, 4, 8, 12, 16, 20), 
                         labels = c("0-4", "5-8", "9-12", "13-16", "17-20")))

df_dryeuc <- df_dryeuc %>%
  mutate(sfl_range20 = cut(sfl, breaks = c(-1, 1:20), 
                         labels = c("0-1","1-2","2-3", "3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-11","11-12","12-13","13-14","14-15","15-16","16-17","17-18","18-19","19-20")))



df_dryeuc <- subset(df, df$VEG_GROUP == "Dry eucalypt forest and woodland")
head(df_dryeuc)
ggplot(df_dryeuc, aes(x=ffdi, y=FRP, color = sfl_range)) + 
  geom_point() +
  labs(title = "Dry euc") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  geom_quantile(quantiles = 0.90, formula = y ~  x + 0 , method = "rq") 

#wet euc
df_weteuc <- df_weteuc %>%
  mutate(sfl_range = cut(sfl, breaks = c(-1, 4, 8, 12, 16, 20), 
                         labels = c("0-4", "5-8", "9-12", "13-16", "17-20")))

ggplot(df_weteuc, aes(x=ffdi, y=FRP, color = sfl_range)) + 
  geom_point() +
  labs(title = "wet euc") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  geom_quantile(quantiles = 0.90, formula = y ~  x + 0 , method = "rq") 

#modifed land (farm)

df_mod <- df_mod %>%
  mutate(sfl_range = cut(sfl, breaks = c(-1, 4, 8, 12, 16, 20), 
                         labels = c("0-4", "5-8", "9-12", "13-16", "17-20")))

ggplot(df_mod, aes(x=ffdi, y=FRP, color = sfl_range)) + 
  geom_point() +
  labs(title = "mod") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  geom_quantile(quantiles = 0.90, formula = y ~  x + 0 , method = "rq")

#moorland
df_moor <- df_moor %>%
  mutate(sfl_range = cut(sfl, breaks = c(-1, 4, 8, 12, 16, 20), 
                         labels = c("0-4", "5-8", "9-12", "13-16", "17-20")))

ggplot(df_moor, aes(x=ffdi, y=FRP, color = sfl_range)) + 
  geom_point() +
  labs(title = "moor") + 
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 5)) + # Customize x-axis
  scale_y_continuous(limits = c(0, 5500), breaks = seq(0, 5500, 1000)) + # Customize y-axis
  scale_color_viridis_d() +
  theme_minimal() +
  geom_quantile(quantiles = 0.90, formula = y ~  x + 0 , method = "rq")


mod <- glm(log1p(FRP) ~ ffdi + sfl, data=df)
ef <- ggpredict(mod,terms=c("ffdi", "sfl"))
plot(ef)

