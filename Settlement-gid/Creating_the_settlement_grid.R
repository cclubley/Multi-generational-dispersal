# -------------------------------------------------------------------------
# ------- Creating a grid as a settlement proxy for Pacific oysters -------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# The following code creates the 'settlement grid' that was used in the 
# manuscript to make the spawning and settlement locations biologically
# relevant. It used raster data for four environmental variables that
# influence Pacific oyster settlement success and combines them, using the 
# resulting raster to crop the ocean grid (code for making which can be found in)
# the 'Initial-spawning-locations' folder) to only those polygons representing 
# 'suitable' habitat.

# Set working directory and load required packages ------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/(3) Methods/Establishment proxy")

{ library(sdmpredictors)
library(rgdal)
library(ggplot2)
library(viridis)
library(ccplot)
library(dplyr)
library(raster)
library(RColorBrewer)
library(rworldxtra)
  library(ggspatial)
}

# Create bathymetry map ---------------------------------------------------

# Load the Bio-ORACLE and MARSPEC datasets 
#datasets <- sdmpredictors::list_datasets(terrestrial = FALSE, marine = TRUE)
#layers <- sdmpredictors::list_layers(datasets)
#View(layers)

# Load the bathymetry raster layer
# 5m resolution
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)
bathy <- bathy$MS_bathy_5m

# Crop raster to the desired boundaries
min_lon <- -13
max_lon <- 13
min_lat <- 43
max_lat <- 65
geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat) # Set the boundaries
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4])) # Create a spatial points object
coordinates(Sites.grid) <- ~ lon_bound + lat_bound # Assign coordinates
bathy <- crop(bathy, extent(Sites.grid)) # Crop the raster to the extent set above

# Plot the raster to check
plot(bathy, col=(rev(brewer.pal(10,"RdBu"))))

# Convert the raster to a spatial points data frame so that the depth values can be added
bathyspdf <- as(bathy, "SpatialPixelsDataFrame")
bathydf <- as.data.frame(bathyspdf)
# Change the column names
colnames(bathydf) <-c("value", "x", "y")
# Assign anything deeper than 40m 'NA' to indicate that it is unsuitable habitat
bathydf$value[bathydf$value < -40] <- NA

# Convert the data frame back to a raster
# Create a spatial points data frame
bathysp <- bathydf
coordinates(bathysp) <- ~ x + y
# Coerce it to a spatial pixels data frame
gridded(bathysp) <- TRUE
# Coerce it to a raster
bathy <- raster(bathysp)

# Plot to check it has worked
plot(bathy, col=(rev(brewer.pal(10,"RdBu"))))
# raster now only includes data for 40m and above

# Create temperature map ----------------------------------------------------

#datasets <- sdmpredictors::list_datasets(terrestrial = FALSE, marine = TRUE)
#layers <- sdmpredictors::list_layers(datasets)
#View(layers)

# Load the sst raster layer for July
sstjul <- load_layers("MS_sst07_5m", equalarea=FALSE)
sstjul <- sstjul$MS_sst07_5m
# August
sstaug <- load_layers("MS_sst08_5m", equalarea=FALSE)
sstaug <- sstaug$MS_sst08_5m
# September
sstsep <- load_layers("MS_sst09_5m", equalarea=FALSE)
sstsep <- sstsep$MS_sst09_5m
# These months used because these are the months in the model in which spawning and settlement occur

# Crop rasters to desired boundaries
min_lon <- -13
max_lon <- 13
min_lat <- 43
max_lat <- 65
geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))
coordinates(Sites.grid) <- ~ lon_bound + lat_bound
sstjul <- crop(sstjul, extent(Sites.grid))
sstaug <- crop(sstaug, extent(Sites.grid))
sstsep <- crop(sstsep, extent(Sites.grid))

# Plot to check
plot(sstsep, col=(rev(brewer.pal(10,"RdBu"))))

# Convert the raster to a spatial points data frame so that the temperature values can be added
sstjulspdf <- as(sstjul, "SpatialPixelsDataFrame")
sstjulspdf <- as.data.frame(sstjulspdf)
# Change the column names
colnames(sstjulspdf) <-c("valuejul", "lon", "lat")

sstaugspdf <- as(sstaug, "SpatialPixelsDataFrame")
sstaugspdf <- as.data.frame(sstaugspdf)
colnames(sstaugspdf) <-c("valueaug", "lon", "lat")

sstsepspdf <- as(sstsep, "SpatialPixelsDataFrame")
sstsepspdf <- as.data.frame(sstsepspdf)
colnames(sstsepspdf) <-c("valuesep", "lon", "lat")

# Merge the three months into one data frame
sstspdf <- full_join(sstjulspdf, sstaugspdf, by=c("lon", "lat")) #full_join will only take two arguments at a time
sstspdf <- full_join(sstspdf, sstsepspdf, by=c("lon", "lat"))
# Average across the three months
sstspdf$value <- rowMeans(sstspdf[ , c(1,4,5)])

# Keep only the coordinates and the new mean value
sstspdf <- sstspdf[ , c(2,3,6)]
colnames(sstspdf) <- c("x", "y", "value")

# Assign anything colder than 3 degrees celsius 'NA' to indicate that it is unsuitable conditions
sstspdf$value[sstspdf$value < 3] <- NA

# Create a risk map where optimal conditions are assigned a value of 2 and suboptimal conditions are assigned a value of 1
# Seperate the conditions based on the temperature thresholds
#sstspdf_sub <- filter(sstspdf, value >= 3 & value <= 16)
#sstspdf_opt <- filter(sstspdf, value > 16 & value <= 35)
# Assign the risk score
#sstspdf_sub$risk <- 1
#sstspdf_opt$risk <- 2
# Merge the two datasets
#sstspdf <- full_join(sstspdf_sub, sstspdf_opt, by=c("x", "y", "value", "risk"))
# Keep only te coordinates and the risk score
#test <- sstspdf[ , c(1,2,4)]
# Convert the data frame back to a raster
# Create a spatial points data frame
#test <- test
#coordinates(test) <- ~ x + y
# Coerce it to a spatial pixels data frame
#gridded(test) <- TRUE
# Coerce it to a raster
#test <- raster(test)
# Plot to check
#plot(test, col=(rev(brewer.pal(2,"RdBu"))))

# Convert the data frame back to a raster
# Create a spatial points data frame
sstsp <- sstspdf
coordinates(sstsp) <- ~ x + y
# Coerce it to a spatial pixels data frame
gridded(sstsp) <- TRUE
# Coerce it to a raster
sst <- raster(sstsp)

# Plot to check
plot(sst, col=(rev(brewer.pal(10,"RdBu"))))
# Now only areas where the average sst over July, August and September is > 3 degrees Celsius are included

# Create salinity map ----------------------------------------------------

#datasets <- sdmpredictors::list_datasets(terrestrial = FALSE, marine = TRUE)
#layers <- sdmpredictors::list_layers(datasets)
#View(layers)

# Load the sss raster layer
sss <- load_layers("BO22_salinitymean_ss", equalarea=FALSE)
sss <- sss$BO22_salinitymean_ss

# Crop rasters to desired boundaries
min_lon <- -13
max_lon <- 13
min_lat <- 43
max_lat <- 65
geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))
coordinates(Sites.grid) <- ~ lon_bound + lat_bound
sss <- crop(sss, extent(Sites.grid))

# Plot to check
plot(sss, col=(rev(brewer.pal(10,"RdBu"))))

# Convert the raster to a spatial points data frame so that the values can be added
sssspdf <- as(sss, "SpatialPixelsDataFrame")
sssspdf <- as.data.frame(sssspdf)
# Change the column names
colnames(sssspdf) <-c("value", "x", "y")

# Assign anything lower than 11 ppt 'NA' to indicate that it is unsuitable conditions
sssspdf$value[sssspdf$value < 11] <- NA

# Convert the data frame back to a raster
# Create a spatial points data frame
ssssp <- sssspdf
coordinates(ssssp) <- ~ x + y
# Coerce it to a spatial pixels data frame
gridded(ssssp) <- TRUE
# Coerce it to a raster
sss <- raster(ssssp)

# Plot to check
plot(sss, col=(rev(brewer.pal(10,"RdBu"))))

# Combine the three maps ----------------------------------------------------

# Convert the raster to a spatial points data frame so that the values can be added
bathyspdf <- as(bathy, "SpatialPixelsDataFrame")
bathydf <- as.data.frame(bathyspdf)
# Change the column names
colnames(bathydf) <-c("bathy", "x", "y")

# Convert the raster to a spatial points data frame so that the values can be added
sstdf <- as(sst, "SpatialPixelsDataFrame")
sstdf <- as.data.frame(sstdf)
# Change the column names
colnames(sstdf) <-c("sst", "x", "y")

# Convert the raster to a spatial points data frame so that the values can be added
sssdf <- as(sss, "SpatialPixelsDataFrame")
sssdf <- as.data.frame(sssdf)
# Change the column names
colnames(sssdf) <-c("sss", "x", "y")

# Combine them into one data frame
df <- full_join(bathydf, sstdf, by=c("x", "y")) #full_join will only take two arguments at a time
df <- full_join(df, sssdf, by=c("x", "y"))

# Remove any data where at least one variable is missing (not suitable habitat)
df <- na.omit(df) # removes 43884 rows

# Create a risk map where optimal conditions are assigned a value of 2 and suboptimal conditions are assigned a value of 1
# This will be based on temperature because I have already eliminated the unsuitable conditions for salinity and bathymetry (no suboptimal conditions, only optimal)

# Seperate the conditions based on the temperature thresholds
df_sub <- filter(df, sst >= 3 & sst <= 16)
df_opt <- filter(df, sst > 16 & sst <= 35)

# Assign the risk score
df_sub$risk <- 1
df_opt$risk <- 2

# Merge the two datasets
df <- full_join(df_sub, df_opt, by=c("bathy", "x", "y", "sst", "sss", "risk"))

# Keep only the coordinates and the risk score
df <- df[ , c(2,3,6)]

# Duplicate for later
df2 <- df

# Convert the data frame back to a raster
# Create a spatial points data frame
coordinates(df) <- ~ x + y
# Coerce it to a spatial pixels data frame
gridded(df) <- TRUE
# Coerce it to a raster
df <- raster(df)
# Plot to check
plot(df, col=(rev(brewer.pal(2,"RdBu"))))

# Add the grid ------------------------------------------------------------

# Load grid
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/(3) Methods/Modelling/Original release file/Shapefiles/Ocean Grid/1 degree")
ocean_grid <- readOGR(dsn=".", layer="ocean_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/(3) Methods/Establishment proxy")

# Plot to check
plot(ocean_grid, add=TRUE)

# Crop the grid to only areas of suitable habitat
df2 <- SpatialPointsDataFrame(coords=df2[,c("x", "y")], proj4string=CRS(proj4string(ocean_grid)), data=df2)
mg_grid <- ocean_grid[!is.na(over(ocean_grid, geometry(df2))),]

# Plot to check
plot(df, col=(rev(brewer.pal(2,"RdBu"))))
plot(mg_grid, add=TRUE)

# Remove the unwanted grid cells (identified on paper)
# Annoyingly you can only on one at a time...
{mg_grid <- mg_grid[!grepl("367", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("368", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("369", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("746", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("791", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("792", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("793", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("794", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("795", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("798", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("799", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("800", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("801", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("802", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("839", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("840", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("841", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("847", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("848", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1067", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1281", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1282", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1329", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1330", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1368", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1369", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1370", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1373", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1374", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1375", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1377", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1378", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1403", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1404", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1416", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1417", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1419", mg_grid$ID),]
mg_grid <- mg_grid[!grepl("1423", mg_grid$ID),]
}

# Plot to check
plot(df, col=(rev(brewer.pal(2,"RdBu"))))
plot(mg_grid, add=TRUE)

# 177 grid polygons, plus 3 more polygons (653, 1410 and 1411) = 180 grid polygons

# Re-write the polygon IDs just so they start from 1
mg_grid@data$ID <- NULL
slot(mg_grid, "data") <- cbind("ID"=1:length(mg_grid),
                            slot(mg_grid, "data"))

# Save the grid ------------------------------------
writeOGR(obj=mg_grid, dsn=".", layer="ocean_grid", driver="ESRI Shapefile")
ocean_grid <- readOGR(dsn=".", layer="ocean_grid")

# Final figure ------------------------------------------------------------

# Convert the settlement raster to a spatial pixels data frame
spdf <- as(df, "SpatialPixelsDataFrame")
df <- as.data.frame(spdf)
df$risk <- 1 # re-set the risk so it will be only one colour

# Read in the spawning locations from Icybox harddrive
mg <- read.csv(file.choose(), header=TRUE)

# Load a map to make the plot neater
data(countriesHigh)
countries <- crop(countriesHigh, extent(-13, 13, 43, 65))
plot(countries)
rm(countriesHigh)

# Convert the settlement grid to spatial lines
grid <- as(mg_grid, 'SpatialLinesDataFrame')
class(grid)
plot(grid)

library(rnaturalearth)

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "large", returnclass = "sf")

ggplot(data=world)+
  geom_tile(df, mapping=aes(x, y, fill=risk))+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+
  cc_theme()+
  geom_path(grid, mapping=aes(x=long, y=lat, group=group))+
  geom_point(mg, mapping=aes(x=lon, y=lat), col="darkorange1", fill="darkorange1", shape=1, size=2)+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  annotation_scale(location = "tl", line_width = 0.5, 
                   bar_cols=c("black", "dark grey"))+
  annotation_north_arrow(location="tl", which_north="true",
                         pad_x=unit(0.4, "in"), pad_y=unit(0.4, "in"))

