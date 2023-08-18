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
library(zoo)

# Load netCDF file
mydata <- nc_open("./Model outputs/2004_Random_100000_passive_2D_1deg_output.nc")

# Get variables
lat <- ncvar_get(mydata,"lat")
lon <- ncvar_get(mydata,"lon")
time <- ncvar_get(mydata, "time")
dist <- ncvar_get(mydata, "distance")

# Get time units
tunits <- ncatt_get(mydata,"time","units")
time_origin <- mydata$var$time$units
time_origin2 <- str_sub(time_origin, 15, 24)
time_origin3 <- str_sub(time_origin, 26, 33)
time_origin <- paste(time_origin2, " ", time_origin3)

rm(time_origin2, time_origin3, tunits)
nc_close(mydata)
rm(mydata)

print("28 day PLD")

# Fill NA values with last observation
lat <- na.locf(na.locf(lat), fromLast=T)
lon <- na.locf(na.locf(lon), fromLast=T)
time <- na.locf(na.locf(time), fromLast=T)
dist <- na.locf(na.locf(dist), fromLast=T)

# Get first row
lat_first <- lat[1,]
lon_first <- lon[1,]
time_first <- time[1,]
dist_first <- dist[1,]

# each row is four hours
# so 6 rows in 1 day
# so 28 days is 168 rows
lat_last <- lat[168, ]
lon_last <- lon[168, ]
time_last  <- time[168, ]
dist_last <- dist[168, ]

# Bind first
lat_first <- as.data.frame(lat_first)
lon_first <- as.data.frame(lon_first)
time_first <- as.data.frame(time_first)
dist_first <- as.data.frame(dist_first)

first <- cbind(lat_first, lon_first, time_first, dist_first)
first$Particle <- paste(1:nrow(first)) 
first$Connectivity <- "Source"
colnames(first) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_first, lon_first, time_first, dist_first)

# Bind last
lat_last <- as.data.frame(lat_last)
lon_last <- as.data.frame(lon_last)
time_last <- as.data.frame(time_last)
dist_last <- as.data.frame(dist_last)

last <- cbind(lat_last, lon_last, time_last, dist_last)
last$Particle <- paste(1:nrow(last)) 
last$Connectivity <- "Sink"
colnames(last) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_last, lon_last, time_last, dist_last)

# Combine first and last
mydata <- rbind(first, last)
rm(last)

# Sort data by particle columns
mydata <- mydata[with(mydata, order(Particle)), ]

# Convert time to datetime
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)

write.csv(mydata, "./csv file/2004_28_Random_100000_passive_2D_1deg_settled.csv", row.names = FALSE)

print("25 day PLD")

# 25 * 6 = 150 rows
# Get last row
lat_last <- lat[150, ]
lon_last <- lon[150, ]
time_last  <- time[150, ]
dist_last <- dist[150, ]

# Bind last
lat_last <- as.data.frame(lat_last)
lon_last <- as.data.frame(lon_last)
time_last <- as.data.frame(time_last)
dist_last <- as.data.frame(dist_last)

last <- cbind(lat_last, lon_last, time_last, dist_last)
last$Particle <- paste(1:nrow(last)) 
last$Connectivity <- "Sink"
colnames(last) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_last, lon_last, time_last, dist_last)

# Combine first and last
mydata <- rbind(first, last)

# Sort data by particle columns
mydata <- mydata[with(mydata, order(Particle)), ]

# Convert time to datetime
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)

write.csv(mydata, "./csv file/2004_25_Random_100000_passive_2D_1deg_settled.csv", row.names = FALSE)

print("17 day PLD")

# 17 * 6 = 102
# Get last row
lat_last <- lat[102, ]
lon_last <- lon[102, ]
time_last  <- time[102, ]
dist_last <- dist[102, ]

# Bind last
lat_last <- as.data.frame(lat_last)
lon_last <- as.data.frame(lon_last)
time_last <- as.data.frame(time_last)
dist_last <- as.data.frame(dist_last)

last <- cbind(lat_last, lon_last, time_last, dist_last)
last$Particle <- paste(1:nrow(last)) 
last$Connectivity <- "Sink"
colnames(last) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_last, lon_last, time_last, dist_last)

# Combine first and last
mydata <- rbind(first, last)

# Sort data by particle columns
mydata <- mydata[with(mydata, order(Particle)), ]

# Convert time to datetime
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)

write.csv(mydata, "./csv file/2004_17_Random_100000_passive_2D_1deg_settled.csv", row.names = FALSE)

print("14 day PLD")

# 14 * 6 = 84
# Get last row
lat_last <- lat[84, ]
lon_last <- lon[84, ]
time_last  <- time[84, ]
dist_last <- dist[84, ]

# Bind last
lat_last <- as.data.frame(lat_last)
lon_last <- as.data.frame(lon_last)
time_last <- as.data.frame(time_last)
dist_last <- as.data.frame(dist_last)

last <- cbind(lat_last, lon_last, time_last, dist_last)
last$Particle <- paste(1:nrow(last)) 
last$Connectivity <- "Sink"
colnames(last) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_last, lon_last, time_last, dist_last)

# Combine first and last
mydata <- rbind(first, last)

# Sort data by particle columns
mydata <- mydata[with(mydata, order(Particle)), ]

# Convert time to datetime
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)

write.csv(mydata, "./csv file/2004_14_Random_100000_passive_2D_1deg_settled.csv", row.names = FALSE)
