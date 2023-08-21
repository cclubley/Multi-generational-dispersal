# Load packages
library(ncdf4)
library(reshape2)
library(stringr)
library(tm)
library(dplyr)
library(sdmpredictors)
library(sp)
library(rgdal)
library(rgeos)

# Load netCDF file
mydata <- nc_open("./Model outputs/2000_Random_100_10_4_14_passive_2D_1deg_output.nc")

# Get variables
lat <- ncvar_get(mydata,"lat")
lon <- ncvar_get(mydata,"lon")
time <- ncvar_get(mydata, "time")
dist <- ncvar_get(mydata, "distance")
settle <- ncvar_get(mydata, "settle")

# Get time units
tunits <- ncatt_get(mydata,"time","units")
time_origin <- mydata$var$time$units
time_origin2 <- str_sub(time_origin, 15, 24)
time_origin3 <- str_sub(time_origin, 26, 33)
time_origin <- paste(time_origin2, " ", time_origin3)

rm(time_origin2, time_origin3, tunits)
nc_close(mydata)
rm(mydata)

# Remove NA values
lat <- na.omit(lat)
lon <- na.omit(lon)
time <- na.omit(time)
dist <- na.omit(dist)
settle <- na.omit(settle)

# Get first row
lat_first <- lat[1,]
lon_first <- lon[1,]
time_first <- time[1,]
dist_first <- dist[1,]
settle_first <- settle[1,]

# Get last row
lat_last <- lat[nrow(lat), ]
lon_last <- lon[nrow(lon), ]
time_last  <- time[nrow(time), ]
dist_last <- dist[nrow(dist), ]
settle_last <- settle[nrow(settle), ]

rm(lat, lon, time, dist, settle)

# Bind first
lat_first <- as.data.frame(lat_first)
lon_first <- as.data.frame(lon_first)
time_first <- as.data.frame(time_first)
dist_first <- as.data.frame(dist_first)
settle_first <- as.data.frame(settle_first)

first <- cbind(lat_first, lon_first, time_first, dist_first, settle_first)
first$Particle <- paste(1:nrow(first)) 
first$Connectivity <- "Source"
colnames(first) <- paste(c("Lat", "Lon", "Time", "Distance", "Settled", "Particle", "Connectivity"))
rm(lat_first, lon_first, time_first, dist_first, settle_first)

# Bind last
lat_last <- as.data.frame(lat_last)
lon_last <- as.data.frame(lon_last)
time_last <- as.data.frame(time_last)
dist_last <- as.data.frame(dist_last)
settle_last <- as.data.frame(settle_last)

last <- cbind(lat_last, lon_last, time_last, dist_last, settle_last)
last$Particle <- paste(1:nrow(last)) 
last$Connectivity <- "Sink"
colnames(last) <- paste(c("Lat", "Lon", "Time", "Distance", "Settled", "Particle", "Connectivity"))
rm(lat_last, lon_last, time_last, dist_last, settle_last)

# Combine first and last
mydata <- rbind(first, last)
rm(first, last)

# Sort data by particle columns
mydata <- mydata[with(mydata, order(Particle)), ]

# Remove unsettled particles
mydata <- mydata %>%
  group_by(Particle) %>% 
  filter(any(Connectivity=="Sink" & Settled > 0)) 

# Convert time to datetime
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)
rm(time_origin)

write.csv(mydata, "./csv file/2000_Random_100_10_4_14_passive_2D_1deg_settled.csv", row.names = FALSE)


