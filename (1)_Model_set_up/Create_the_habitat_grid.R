# The following code creates the 'habitat grid' that was used in the 
# manuscript to make the spawning and settlement locations biologically
# relevant. It used raster data for four environmental variables that
# influence Pacific oyster settlement success and combines them, using the 
# resulting raster to crop the ocean grid to only those polygons representing 
# 'suitable' habitat.

# Set working directory and load required packages ------------------------

{ library(sdmpredictors)
  library(ggplot2)
  library(rnaturalearth)
  library(viridis)
  library(ccplot)
  library(dplyr)
  library(raster)
  library(RColorBrewer)
  library(rworldxtra)
  library(ggspatial)
  library(sf)
}

# Create bathymetry map ---------------------------------------------------

# Load the Bio-ORACLE and MARSPEC datasets 
#datasets <- sdmpredictors::list_datasets(terrestrial = FALSE, marine = TRUE)
#layers <- sdmpredictors::list_layers(datasets)
#View(layers)

# Load the bathymetry raster layer
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

# Load the substrate types data -------------------------------------------

# Load the substrate classifications raster from EU Sea Map
substrate <- raster("./EUSeaMap_2023_Substrate_100m_WGS84.tif")

# Crop the substrate raster to the extent of the environmental raster
substrate <- crop(substrate, extent(df))

# substrate raster is at 100 m resolution 
# Change the resolution to match that of the other rasters, using nearest 
# neighbour method to determine the values for each cell
substrate_scaled <- resample(substrate, bathy, method="ngb")

# Plot the raster to check
plot(substrate_scaled)

# Convert the raster to a spatial points data frame so that the depth values can be added
subsspdf <- as(substrate_scaled, "SpatialPixelsDataFrame")
subsdf <- as.data.frame(subsspdf)
# Change the column names
colnames(subsdf) <-c("substrate", "x", "y")

# Join the data frames
df <- full_join(bathydf, sstdf, by=c("x", "y")) #full_join will only take two arguments at a time
df <- full_join(df, sssdf, by=c("x", "y"))
df <- full_join(df, subsdf, by=c("x", "y"))

# Remove habitat unsuitable due to depth, temp and salinity
df <- df[!is.na(c(df$bathy)),]
df <- df[!is.na(c(df$sst)),]
df <- df[!is.na(c(df$sss)),]

# Check remaining substrate values
table(df$substrate)
# 1-9, 11-12, 15, 17-19, 21 & 23 
# correspond to substrate types - NA is where there is no substrate data

# Substrates on which Pacific oysters can grow:
# 1 = rock or other hard substrata
# 2 = sand
# 3 = coarse substrate
# 4 = mixed sediment
# 5 = fine mud
# 6 = sandy mud
# 7 = seabed
# 8 = fine mud/sandy mud/muddy sand
# 9  = muddy sand
# 11 = coarse and mixed sediment
# 15 = sandy mud or muddy sand 
# 17 = worm reefs (biogenic reef)
# 18 = Ostrea edulis beds (biogenic reef)
# 21 = Sabellaria spinulosa reefs (biogenic reef)
# 23 = Modiolus modiolus beds (biogenic reef)
# 19 = Sediment

# Substrates on which Pacific oysters cannot grow:
# 12 = Posidonia oceanica meadows

# Separate the NA values
# Replace NAs with 0
df[is.na(df)] <- 0
subsna <- filter(df, substrate==0)
# Add a value for plotting purposes
subsna$value <- 1
# Keep only the coordinates
subsna <- subsna[ , c(2,3,7)]
# Convert the data frame back to a raster
# Create a spatial points data frame
coordinates(subsna) <- ~ x + y
# Coerce it to a spatial pixels data frame
gridded(subsna) <- TRUE
# Plot to check
plot(subsna, col="tan2")
# Convert the settlement raster to a spatial pixels data frame
subsna <- as(subsna, "SpatialPixelsDataFrame")
subsna <- as.data.frame(subsna)

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "large", returnclass = "sf")
ggplot(data=world)+
  #geom_path(grid, mapping=aes(x=long, y=lat, group=group))+
  geom_tile(subsna, mapping=aes(x, y, fill=value), col="red")+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+
  cc_theme()

# Separate the NON-NA values
subs <- filter(df, substrate>0)
# Add a value for plotting purposes
subs$value <- 1
# Keep only the coordinates
subs <- subs[ , c(2,3,7)]
# Convert the data frame back to a raster
# Create a spatial points data frame
coordinates(subs) <- ~ x + y
# Coerce it to a spatial pixels data frame
gridded(subs) <- TRUE
# Plot to check
plot(subs, col="tan2")
# Convert the habitat raster to a spatial pixels data frame
subs <- as(subs, "SpatialPixelsDataFrame")
subs <- as.data.frame(subs)
ggplot(data=world)+
  geom_tile(subs, mapping=aes(x, y, fill=value), col="lightblue")+
  geom_tile(subsna, mapping=aes(x, y, fill=value), col="red")+
  #geom_path(grid, mapping=aes(x=long, y=lat, group=group))+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+
  cc_theme()
# Looks like all the places where there is unknown substrate (NA/Red) there is 
# also suitable substrate (blue)

# Assign Posidonia oceanica meadows (12) 'NA' to indicate that it is unsuitable 
# habitat
df[is.na(df)] <- 0 # Replace NAs with 0 first
df$substrate[df$substrate == 12] <- NA
# Remove the NAs
df <- na.omit(df) # removes 8 rows

# Plotting ----------------------------------------------------------------

# Add a value for plotting purposes
df$value <- 1

# Keep only the coordinates
df <- df[ , c(2,3,7)]

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
plot(df, col="tan2")

# Add the grid ------------------------------------------------------------

# Load grid
ocean_grid <- read_sf(dsn=".", layer="ocean_grid")
# Convert to spatial polygons
ocean_grid <- as(ocean_grid, "Spatial")

# Plot to check
plot(ocean_grid, add=TRUE)

# Crop the grid to only areas of suitable habitat
df2 <- SpatialPointsDataFrame(coords=df2[,c("x", "y")], proj4string=CRS(proj4string(ocean_grid)), data=df2)
mg_grid <- ocean_grid[!is.na(over(ocean_grid, geometry(df2))),]

# Plot to check
plot(df, col="tan2")
plot(mg_grid, add=TRUE)

# Save the grid ------------------------------------

writeSpatialShape(mg_grid, "ocean_grid.shp")
# Load grid
ocean_grid <- read_sf(dsn=".", layer="ocean_grid")
# Convert to spatial polygons
ocean_grid <- as(ocean_grid, "Spatial")

# Final figure ------------------------------------------------------------

# Convert the settlement raster to a spatial pixels data frame
spdf <- as(df, "SpatialPixelsDataFrame")
df <- as.data.frame(spdf)
df$risk <- 1 # re-set the risk so it will be only one colour

# Read in the spawning locations for the year 2000
mg <- read.csv("Spawning_locations_2000_1_deg.csv")

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

