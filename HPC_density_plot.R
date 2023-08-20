library(dplyr)
library(plyr)
library(geosphere)
library(sdmpredictors)
library(ggplot2)

# Read in the csv data
mydata_100 <- read.csv("./csv file/Sensitivity_100.csv")
mydata_1000 <- read.csv("./csv file/Sensitivity_1000.csv")
mydata_10000 <- read.csv("./csv file/Sensitivity_10000.csv")
mydata_100000 <- read.csv("./csv file/Sensitivity_100000.csv")

# Load raster layer for CRS projection
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)

# Calculate distance between start and end point
#100
mydata_100_source <- data.frame(mydata_100$Source_lon, mydata_100$Source_lat)
colnames(mydata_100_source) <- c("Lon", "Lat")
mydata_100_sink <- data.frame(mydata_100$Sink_lon, mydata_100$Sink_lat)
colnames(mydata_100_sink) <- c("Lon", "Lat")

mydata_100_source <- SpatialPointsDataFrame(
  coords=mydata_100_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_100_source)

mydata_100_sink <- SpatialPointsDataFrame(
  coords=mydata_100_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_100_sink)

mydata_100 <- mydata_100 %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_100_source)) {
  gDists <- suppressWarnings(distm(mydata_100_source[i,], mydata_100_sink[i,], fun=distHaversine)/1000)
  mydata_100$Dist_total[i] <- gDists}

#1000
mydata_1000_source <- data.frame(mydata_1000$Source_lon, mydata_1000$Source_lat)
colnames(mydata_1000_source) <- c("Lon", "Lat")
mydata_1000_sink <- data.frame(mydata_1000$Sink_lon, mydata_1000$Sink_lat)
colnames(mydata_1000_sink) <- c("Lon", "Lat")

mydata_1000_source <- SpatialPointsDataFrame(
  coords=mydata_1000_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_1000_source)

mydata_1000_sink <- SpatialPointsDataFrame(
  coords=mydata_1000_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_1000_sink)

mydata_1000 <- mydata_1000 %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_1000_source)) {
  gDists <- suppressWarnings(distm(mydata_1000_source[i,], mydata_1000_sink[i,], fun=distHaversine)/1000)
  mydata_1000$Dist_total[i] <- gDists}

#10000
mydata_10000_source <- data.frame(mydata_10000$Source_lon, mydata_10000$Source_lat)
colnames(mydata_10000_source) <- c("Lon", "Lat")
mydata_10000_sink <- data.frame(mydata_10000$Sink_lon, mydata_10000$Sink_lat)
colnames(mydata_10000_sink) <- c("Lon", "Lat")

mydata_10000_source <- SpatialPointsDataFrame(
  coords=mydata_10000_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_10000_source)

mydata_10000_sink <- SpatialPointsDataFrame(
  coords=mydata_10000_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_10000_sink)

mydata_10000 <- mydata_10000 %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_10000_source)) {
  gDists <- suppressWarnings(distm(mydata_10000_source[i,], mydata_10000_sink[i,], fun=distHaversine)/1000)
  mydata_10000$Dist_total[i] <- gDists}

#100000
mydata_100000_source <- data.frame(mydata_100000$Source_lon, mydata_100000$Source_lat)
colnames(mydata_100000_source) <- c("Lon", "Lat")
mydata_100000_sink <- data.frame(mydata_100000$Sink_lon, mydata_100000$Sink_lat)
colnames(mydata_100000_sink) <- c("Lon", "Lat")

mydata_100000_source <- SpatialPointsDataFrame(
  coords=mydata_100000_source[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_100000_source)

mydata_100000_sink <- SpatialPointsDataFrame(
  coords=mydata_100000_sink[,c("Lon", "Lat")], 
  proj4string=CRS(proj4string(bathy)), data=mydata_100000_sink)

mydata_100000 <- mydata_100000 %>% mutate(Dist_total=NA)

for (i in 1:nrow(mydata_100000_source)) {
  gDists <- suppressWarnings(distm(mydata_100000_source[i,], mydata_100000_sink[i,], fun=distHaversine)/1000)
  mydata_100000$Dist_total[i] <- gDists}

# Get just the distances
ks_100 <- as.data.frame(mydata_100$Dist_total)
ks_1000 <- as.data.frame(mydata_1000$Dist_total)
ks_10000 <- as.data.frame(mydata_10000$Dist_total)
ks_100000 <- as.data.frame(mydata_100000$Dist_total)

# Add a particle column
ks_100$Particles <- 100
ks_1000$Particles <- 1000
ks_10000$Particles <- 10000
ks_100000$Particles <- 100000

# Change column names
colnames(ks_100) <- c("Distance", "Particles")
colnames(ks_1000) <- c("Distance", "Particles")
colnames(ks_10000) <- c("Distance", "Particles")
colnames(ks_100000) <- c("Distance", "Particles")

# Merge
total_data <- rbind(ks_100, ks_1000, ks_10000, ks_100000)
total_data$Particles <- as.factor(total_data$Particles)

# ggplot theme
theme_marine <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 20),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 2, linetype="blank"))
}

setwd("./Density plot")

# Density plot
medians <- ddply(total_data, "Particles", summarise, rating.median = median(Distance))

ggplot(total_data, aes(x=Distance, group=Particles, fill=Particles))+
  geom_density(alpha=.4)+
  scale_fill_manual(values=c("dark grey", "tan1", "aquamarine3", "dark green"))+
  geom_vline(data=medians, aes(xintercept = rating.median, colour = Particles), linetype = "longdash", size=1)+
  scale_colour_manual(values=c("dark grey", "tan1", "aquamarine3", "dark green"))+
  xlab("Distance travelled (km)") +
  ylab("Density")+
  theme_marine()

