# -------------------------------------------------------------------------
# ------- Creating a grid as a settlement proxy for Pacific oysters -------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# The following code creates the 'habitat grid' that was used in the 
# manuscript to make the spawning and settlement locations biologically
# relevant. It used raster data for four environmental variables that
# influence Pacific oyster settlement success and combines them, using the 
# resulting raster to crop the ocean grid (code for making which can be found in
# the 'Creating_an_ocean_shapefile' script) to only those polygons representing 
# 'suitable' habitat.

# Set working directory and load required packages ------------------------

#setwd("D:/OneDrive back up (27-02-2024)/Chapters/(3) Dispersal Limitation/(3) Methods/(Fig 2) Establishment proxy")
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
# also suitable substrate (blue), so I'm not worried about the NA values and will
# keep them.

# Assign Posidonia oceanica meadows (12) 'NA' to indicate that it is unsuitable 
# habitat - I don't know of any evidence that oysters can live in seagrass meadows
df[is.na(df)] <- 0 # Replace NAs with 0 first
df$substrate[df$substrate == 12] <- NA
# Remove the NAs
df <- na.omit(df) # removes 8 rows

# Plotting ----------------------------------------------------------------

## Create a risk map where optimal conditions are assigned a value of 2 and suboptimal conditions are assigned a value of 1
## This will be based on temperature because I have already eliminated the unsuitable conditions for salinity and bathymetry (no suboptimal conditions, only optimal)
## Seperate the conditions based on the temperature thresholds
#df_sub <- filter(df, sst >= 3 & sst <= 16)
#df_opt <- filter(df, sst > 16 & sst <= 35)
## Assign the risk score
#df_sub$risk <- 1
#df_opt$risk <- 2
## Merge the two datasets
#df <- full_join(df_sub, df_opt, by=c("bathy", "x", "y", "sst", "sss", "risk"))

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

#writeRaster(df, "oysters.grd")

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

# Remove the unwanted grid cells (identified on paper)
# Annoyingly you can only on one at a time...
{ mg_grid <- mg_grid[!grepl("367", mg_grid$ID),]
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
plot(df, col="tan2")
plot(mg_grid, add=TRUE)

# 177 grid polygons, plus 3 more polygons (653, 1410 and 1411) = 180 grid polygons

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

