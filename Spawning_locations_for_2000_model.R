# The following script generates spawning locations for the first year of the 
# biophysical models (2000) using Pacific oyster distribution data downloaded 
# from four online repositories: GBIF, OBIS, NBN Atlas and Marine Recorder. The 
# data were used in a previous publication (Clubley et al. 2023, 
# https://doi.org/10.1016/j.scitotenv.2023.162754) and details on the 
# compilation and cleaning process are described therein.

# Load the required libraries ---------------------------------------------
library(dplyr)
library(sp)
library(rgdal)

# Load the data and subset ------------------------------------------------

# Load the Magallana gigas occurrence data and save as 'mg'
mg <- read.csv("Data_2021_for_release_file.csv")

# Subset the data to include only those records up until the year 2000 (first
# year of the biophysical models)
mg <- filter(mg, Year <= 1999)

# Select only the latitude and longitude
mg <- mg %>% dplyr::select(Longitude, Latitude)

# Subset the data to include only points within the domain of the NEMO 
# hydrodynamic data
mg <- filter(mg, Longitude <= 13)
mg <- filter(mg, Longitude >= -13)
mg <- filter(mg, Latitude <= 65)
mg <- filter(mg, Latitude >= 45)

# Import BIO-ORACLE data --------------------------------------------------
bathy <-sdmpredictors::load_layers("MS_bathy_5m", equalarea = FALSE)

# Convert the projection of the data to that of the raster ----------------
mg <- sp::SpatialPointsDataFrame(coords=mg[,c("Longitude", "Latitude")], 
                                 proj4string=CRS(proj4string(bathy)), data=mg)

# Check the points plot on the raster -------------------------------------

plot(bathy, xlim=c(-20,30), ylim=c(30,70))
points(mg, col='blue')

# Load in ocean grid shapefile --------------------------------------------

# Load in a high resolution ocean grid (Code in other R script)
# This grid has a resolution of 1 degree and already has polygon IDs
ocean_grid <- readOGR(dsn=".", layer="ocean_grid")

# Plot to check
plot(bathy, xlim=c(-13, 13), ylim=c(45, 65))
# Add the cropped grid 
plot(ocean_grid, add=TRUE)
# Add the mg points
plot(mg, add=TRUE, col='blue')

# Use the species spatial points to select polygons with presence data ----
mg_grid <- ocean_grid[!is.na(sp::over(ocean_grid, sp::geometry(mg))),]

# Extract centroids of polygons with presence data ------------------------

# Extract unique polygon IDs from mg_grid
polygonID <- mg_grid$ID 

# Get centroids for each polygon and save as spatial points data
centroids <- getSpPPolygonsLabptSlots(mg_grid) 

# Convert to data.frame
centroids <- as.data.frame(centroids); colnames(centroids) <- c("lon","lat") 

# Join the separate data frames
grid_centres <- cbind(centroids, polygonID)
head(grid_centres)

# Plot the spawning locations
plot(bathy, xlim=c(-13, 13), ylim=c(45, 65))
points(grid_centres[,1:2], cex=0.5, pch=19, col='red')
