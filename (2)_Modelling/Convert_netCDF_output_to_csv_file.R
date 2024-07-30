# The following script takes the output of the biophysical model in NetCDF
# format and converts it to .csv format, keeping only the information that is of
# interest for post-processing in order to reduce the size of the data. Keep 
# only the source and sink information for each particle as the actual particle 
# trajectory was not of interest for this study, and to assess the connectivity
# of populations using graph network theory only the source and sink information
# is needed.

# Load the data and the required packages ---------------------------------
{library(ncdf4)
library(reshape2)
library(stringr)
library(tm)
library(dplyr)
library(sdmpredictors)
library(sp)
library(rgdal)
library(rgeos)
library(zoo)
}

# Load the NetCDF  output of the biophysical model
mydata <- nc_open("./Model outputs/2000_Random_100000_passive_2D_1deg_output.nc")

# Extract the variables of interest ---------------------------------------

# Extract the variables corresponding to the particle trajectories
lat <- ncvar_get(mydata,"lat") # Latitude
lon <- ncvar_get(mydata,"lon") # Longitude
time <- ncvar_get(mydata, "time") # Time
tunits <- ncatt_get(mydata,"time","units") # Unit of time
dist <- ncvar_get(mydata, "distance") # Distance traveled 

# Identify the time of origin of particles
time_origin <- mydata$var$time$units 
time_origin2 <- str_sub(time_origin, 15, 24)
time_origin3 <- str_sub(time_origin, 26, 33)
time_origin <- paste(time_origin2, " ", time_origin3)

# Now all variables of interest have been extracted, clear the environment and 
# close the NetCDF file
rm(time_origin2, time_origin3, tunits)
nc_close(mydata)
rm(mydata)

# Create a .csv file for a 28 day PLD -------------------------------------

# As each particle has a unique 'death' date, fill any NA values in each 
# variable with the last recorded observation so that each is the same length.
lat <- na.locf(na.locf(lat), fromLast=T)
lon <- na.locf(na.locf(lon), fromLast=T)
time <- na.locf(na.locf(time), fromLast=T)
dist <- na.locf(na.locf(dist), fromLast=T)

# For each particle in each variable, extract the first row of data which 
# corresponds to the particles source.
lat_first <- lat[1,]
lon_first <- lon[1,]
time_first <- time[1,]
dist_first <- dist[1,]

# The biophysical model has an hourly output, so each row corresponds to one 
# hour, or 3600 seconds. Therefore, 24 rows corresponds to 1 day (86400 
# seconds). 28 days is equal to 672 rows. 
# So to get the sink information for each particle, extract the 672nd row.
lat_last <- lat[672, ]
lon_last <- lon[672, ]
time_last  <- time[672, ]
dist_last <- dist[672, ]

# Bind the particle source information together into one data 
# frame. To do this, first convert each variable to a data frame
lat_first <- as.data.frame(lat_first)
lon_first <- as.data.frame(lon_first)
time_first <- as.data.frame(time_first)
dist_first <- as.data.frame(dist_first)
# Then use cbind to combine them as columns
first <- cbind(lat_first, lon_first, time_first, dist_first)
# Give each particle a number starting from 1 to the number of rows in the 
# data frame
first$Particle <- paste(1:nrow(first)) 
# Label the data by assigning a column called 'Connectivity' and labelling 
# each as row as 'source'
first$Connectivity <- "Source"
# Then change the column names to make the data frame clean
colnames(first) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
# And finally clear the environment of the data that is no longer useful
rm(lat_first, lon_first, time_first, dist_first)

# Repeat the exact same process for the final lcoation data
lat_last <- as.data.frame(lat_last)
lon_last <- as.data.frame(lon_last)
time_last <- as.data.frame(time_last)
dist_last <- as.data.frame(dist_last)
last <- cbind(lat_last, lon_last, time_last, dist_last)
last$Particle <- paste(1:nrow(last)) 
last$Connectivity <- "Sink"
colnames(last) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_last, lon_last, time_last, dist_last)

# Combine the information for the particle source and the particle sink into
# one data frame using rbind
mydata <- rbind(first, last)

# Sort the data by the particle column so that the source and sink 
# information for each particle is displayed together
mydata <- mydata[with(mydata, order(Particle)), ]

# Convert the time to a datetime object using the origin time 
# defined earlier
mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)

# Finally, write the .csv file
write.csv(mydata, "./csv file/2000_28_Random_100000_passive_2D_1deg_settled.csv", row.names = FALSE)

# Remove the sink information from the environment ('last'), BUT keep 
# the source information ('first') as this will remain the same regardless of 
# the PLD  
rm(last)

# Repeat the exact same process as above for the remaining PLDs (25, 17 and 
# 14 days), with the only changes being the row indexing of the particle sink 
# data and the name of the output file.

# Create a .csv file for a 25 day PLD -------------------------------------

print("25 day PLD")

# 25 * 24 = 600 rows
lat_last <- lat[600, ]
lon_last <- lon[600, ]
time_last  <- time[600, ]
dist_last <- dist[600, ]

lat_last <- as.data.frame(lat_last)
lon_last <- as.data.frame(lon_last)
time_last <- as.data.frame(time_last)
dist_last <- as.data.frame(dist_last)
last <- cbind(lat_last, lon_last, time_last, dist_last)
last$Particle <- paste(1:nrow(last)) 
last$Connectivity <- "Sink"
colnames(last) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_last, lon_last, time_last, dist_last)

mydata <- rbind(first, last)

mydata <- mydata[with(mydata, order(Particle)), ]

mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)

write.csv(mydata, "./csv file/2000_25_Random_100000_passive_2D_1deg_settled.csv", row.names = FALSE)

rm(last)

# Create a .csv file for a 17 day PLD -------------------------------------

print("17 day PLD")
 
# 17 * 24 = 408
lat_last <- lat[408, ]
lon_last <- lon[408, ]
time_last  <- time[408, ]
dist_last <- dist[408, ]

lat_last <- as.data.frame(lat_last)
lon_last <- as.data.frame(lon_last)
time_last <- as.data.frame(time_last)
dist_last <- as.data.frame(dist_last)
last <- cbind(lat_last, lon_last, time_last, dist_last)
last$Particle <- paste(1:nrow(last)) 
last$Connectivity <- "Sink"
colnames(last) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_last, lon_last, time_last, dist_last)

mydata <- rbind(first, last)

mydata <- mydata[with(mydata, order(Particle)), ]

mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)

write.csv(mydata, "./csv file/2000_17_Random_100000_passive_2D_1deg_settled.csv", row.names = FALSE)

rm(last)

# Create a .csv file for a 14 day PLD -------------------------------------

print("14 day PLD")

# 14 * 24 = 336
lat_last <- lat[336, ]
lon_last <- lon[336, ]
time_last  <- time[336, ]
dist_last <- dist[336, ]

lat_last <- as.data.frame(lat_last)
lon_last <- as.data.frame(lon_last)
time_last <- as.data.frame(time_last)
dist_last <- as.data.frame(dist_last)
last <- cbind(lat_last, lon_last, time_last, dist_last)
last$Particle <- paste(1:nrow(last)) 
last$Connectivity <- "Sink"
colnames(last) <- paste(c("Lat", "Lon", "Time", "Distance", "Particle", "Connectivity"))
rm(lat_last, lon_last, time_last, dist_last)

mydata <- rbind(first, last)

mydata <- mydata[with(mydata, order(Particle)), ]

mydata$Time <- as.POSIXct(mydata$Time, origin=time_origin)

write.csv(mydata, "./csv file/2000_14_Random_100000_passive_2D_1deg_settled.csv", row.names = FALSE)
