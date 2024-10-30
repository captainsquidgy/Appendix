library(terra)
library(lubridate)
library(tidyverse)
library(quantreg)
library(ggeffects)
library(viridis)
library(raster)
library(sf)
library(dplyr)


setwd('C:/Users/zmajor/OneDrive - University of Tasmania/zac')

hs_allvars <- read.csv("hourly/hourly/hotspot_AllVars_join.csv")


#we want a dataframe with lat/lon, date, sat, conf, dn, FRP, FBI, FFDI, Intensity, SFL

df <- cbind(hs_allvars[,3:5], hs_allvars[,9:10], hs_allvars[,12:13], hs_allvars[,15:17])

df$timestamp_h <- ymd_hms(df$timestamp, tz = "UTC")

df$timestamp_h <- with_tz(df$timestamp_h, "Australia/Hobart")

df$timestamp_h <- as.Date(as.POSIXct(df$timestamp_h,origin="1970-01-01"))

df$date <- df$timestamp_h

df <- cbind(df[,1:6] , df[,8:10], df[,12])

names(df)[10] <- "date"

#we also need fuel load

#SFL, DATE
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

#ok so now join sfl to this dataframe 
#sfl data was suss last time so lets fix that

#we need to subset for post 2003ish and before 2019ish
sum(is.na(df$FBI))
sum(is.nan(df$FBI))
sum(is.infinite(df$FBI))
sum(is.na(df$intensity))
sum(is.nan(df$intensity))
sum(is.infinite(df$intensity))

df <- df[complete.cases(df$FBI),]

yearlist <- names(combined_sfl)
out <- list()

for(i in 1:nrow(df)){
  obs_month <- month(df$date[i])
  print(i)
  #so if hs occurred in 1st half of year we need sfl from "2 years prior"
  if (obs_month < 7){
    #for each observation give me the
    #year
    obs_year <- year(df$date[i])-2
    
    #lat
    obs_lat <- df$lat[i]
    
    #long
    obs_lon <- df$lon[i]
    
    #which layer of combined sfl has year = obs year
    year_lyr <- which(yearlist == obs_year)
    
    #extract this layer at lon/lat
    ex_sfl <- terra::extract(combined_sfl[[year_lyr]], cbind(obs_lon, obs_lat))
    
    #name it sfl
    names(ex_sfl) <- "sfl"
    
    #save this to a list
    out[[i]]=ex_sfl
    } else { 
      #for each observation give me the
      #year
      obs_year <- year(df$date[i])-1
      
      #lat
      obs_lat <- df$lat[i]
      
      #long
      obs_lon <- df$lon[i]
      
      #which layer of combined sfl has year = obs year
      year_lyr <- which(yearlist == obs_year)
      
      #extract this layer at lon/lat
      ex_sfl <- terra::extract(combined_sfl[[year_lyr]], cbind(obs_lon, obs_lat))
      
      #name it sfl
      names(ex_sfl) <- "sfl"
      
      #save this to a list
      out[[i]]=ex_sfl
   
    }
}

data_j_sfl <- do.call(rbind,out)
print(data_j_sfl)
df_sfl <- as.data.frame(data_j_sfl)

df <- cbind(df, df_sfl)

#ok so now we want to attach Veg_group to this data
df <- read.csv("afdr_dataframe3.csv")

df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

veg_data <- st_read("tasveg_4_group/tasveg_4_group.gpkg")

veg_data_t <- st_transform(veg_data, crs = 32755)

veg_data_t <- st_simplify(veg_data_t, dTolerance = 0.0001) # Uncomment if needed

veg_data_f <- st_transform(veg_data_t, crs = 4326)

sf_use_s2(FALSE)

df_joined <- st_join(df_sf, veg_data_f, join = st_intersects)

df_final <- as.data.frame(df_joined)



d <- cbind(df_final[1:9], df_final[19], df_final[27])

write.csv(d, file = "afdr_dataframe4.csv")# , row.names = FALSE)

d_r1 <- read.csv("afdr_dataframe4.csv", header = TRUE, sep = ",")

col_names <- c("sat","FRP","conf","dn","FFDI","FBI","intensity","date","SFL","VEG_GROUP","geometry_lon","geometry_lat")

d_r1$geometry_lon <- gsub("c\\(", "", d_r1$geometry_lon)  # Remove "c("
d_r1$geometry_lat <- gsub("\\)", "", d_r1$geometry_lat)     # Remove ")"

# Step 2: Convert the cleaned strings to numeric values
d_r1$geometry_lon <- as.numeric(d_r1$geometry_lon)
d_r1$geometry_lat <- as.numeric(d_r1$geometry_lat)

col_names <- c("sat","FRP","conf","dn","FFDI","FBI","intensity","date","SFL","VEG_GROUP","lon","lat")

colnames(d_r1) <- col_names

d_r1 <- read.csv("afdr_dataframe4.csv", header = TRUE, col.names = col_names)

write.csv(d_r1, file = "afdr_dataframe4.csv")

d_r2 <- d_r1[2:13]

write_csv(d_r2, file = "afdr_dataframe4.csv")

d_r3 <- read.csv("afdr_dataframe4.csv")





