# -------------------------------------------------------------------------
# ----------------- Generating original spawning locations ----------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# The following script generates spawning locations for the first year of the 
# biophysical models (2000) using Pacific oyster distribution data downloaded 
# from four online repositories: GBIF, OBIS, NBN Atlas and Marine Recorder. The 
# data were used in a previous publication (Clubley et al. 2023, 
# https://doi.org/10.1016/j.scitotenv.2023.162754) and details on the 
# compilation and cleaning process are described therein.

# Load packages --------------------------------------------
library(dplyr)
library(sp)
#library(rgdal)

# Load the data and subset ------------------------------------------------

# Load the Magallana gigas occurrence data collated in my first PhD chapter 
# (and the STOTEN paper) and save as 'mg'
#setwd("D:/OneDrive back up (27-02-2024)/Chapters/(2) Rate of spread/Data/Data - 2022/Final spreadsheet")
mg <- read.csv("Total_data_Atlantic_April_2022_Final.csv")
#setwd("D:/OneDrive back up (27-02-2024)/Chapters/(3) Dispersal Limitation/(3) Methods/Modelling/Original release file/Final scripts and data")

# Subset the data to include only those records up until the year 2000 (first
# year of the biophysical models) - n = 714
mg <- filter(mg, Year <= 1999)

# Select only the latitude and longitude
mg <- mg %>% dplyr::select(Longitude, Latitude)

# Subset the data to include only points within the domain of the NEMO 
# hydrodynamic data - n = 622
mg <- filter(mg, Longitude <= 13)
mg <- filter(mg, Longitude >= -13)
mg <- filter(mg, Latitude <= 65)
mg <- filter(mg, Latitude >= 45)

# Import BIO-ORACLE data --------------------------------------------------

# Load a raster of the ocean depth of the study area
bathy <-sdmpredictors::load_layers("MS_bathy_5m", equalarea = FALSE)

# Convert the projection of the data to that of the raster
mg <- sp::SpatialPointsDataFrame(coords=mg[,c("Longitude", "Latitude")], 
                                 proj4string=CRS(proj4string(bathy)), data=mg)

# Plot to check
plot(bathy, xlim=c(-20,30), ylim=c(30,70))
points(mg, col='blue')

# Load in settlement grid shapefile ---------------------------------------

# Set the working directory to the location of the settlement grid shapefile
# - the settlement grid is the grid created in the 'Creating-the-settlement-grid'
# script that only includes areas of suitable habitat for M. gigas
#setwd("D:/OneDrive back up (27-02-2024)/Chapters/(3) Dispersal Limitation/(3) Methods/(Fig 2) Establishment proxy/Establishment-based grid")

# This grid has a resolution of 1 degree and already has polygon IDs
#ocean_grid <- readOGR(dsn=".", layer="ocean_grid")
ocean_grid <- sf::st_read(dsn = ".", layer = "ocean_grid")

# Plot to check
plot(bathy, xlim=c(-13, 13), ylim=c(45, 65))
# Add the cropped grid 
plot(ocean_grid, add=TRUE)
# Add the mg points
plot(mg, add=TRUE, col='blue')

# Set the working directory back to the main one
#setwd("D:/OneDrive back up (27-02-2024)/Chapters/(3) Dispersal Limitation/(3) Methods/Modelling/Original release file/Final scripts and data")

# Use the species spatial points to select polygons with presence data ----

mg_sf <- sf::st_as_sf(mg)
mg_grid <- sf::st_intersection(ocean_grid, mg_sf)
#mg_grid <- ocean_grid[!is.na(sp::over(ocean_grid, sp::geometry(mg))),]

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

# Write the spawning location file ----------------------------------------
write.csv(grid_centres, "Spawning_locations_2000.csv", row.names = FALSE)
