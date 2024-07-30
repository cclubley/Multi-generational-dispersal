# The following script creates a 1 degree resolution grid over the ocean areas of 
# the study domain (north-west Europe). This grid is used in the creation of the
# habitat grid, which in turn is used to generate spawning locations. High 
# resolution country shapefiles are used to create the grid to try to preserve 
# complex coastlines like fjord and estuary areas.

# Load the required libraries ---------------------------------------------
library(raster)
library(sp)
library(rgdal)

# Download/load high-resolution country shapefiles ------------------------

# Download from GADM using the country's ISO code
{ #Albania
ALB <- getData('GADM', country='ALB', level='0')
#Algeria
DZA <- getData('GADM', country='DZA', level='0')
#Andorra
AND <- getData('GADM', country='AND', level='0')
#Austria
AUT <- getData('GADM', country='AUT', level='0')
#Belarus
BLR <- getData('GADM', country='BLR', level='0')
#Belgium
BEL <- getData('GADM', country='BEL', level='0')
#Bosnia and Herzegowina 
BIH <- getData('GADM', country='BIH', level='0')
#Bulgaria
BGR <- getData('GADM', country='BGR', level='0')
#Croatia
HRV <- getData('GADM', country='HRV', level='0')
#Czech Republic
CZE <- getData('GADM', country='CZE', level='0')
#Denmark
DNK <- getData('GADM', country='DNK', level='0')
#Egypt
EGY <- getData('GADM', country='EGY', level='0')
#Estonia
EST <- getData('GADM', country='EST', level='0')
#Finland
FIN <- getData('GADM', country='FIN', level='0')
#France
FRA <- getData('GADM', country='FRA', level='0')
#Germany
DEU <- getData('GADM', country='DEU', level='0')
#Great Britain
GBR <- getData('GADM', country='GBR', level='0')
#Greece
GRC <- getData('GADM', country='GRC', level='0')
#Hungary
HUN <- getData('GADM', country='HUN', level='0')
#Iceland
ISL <- getData('GADM', country='ISL', level='0')
#Ireland
IRL <- getData('GADM', country='IRL', level='0')
#Italy
ITA <- getData('GADM', country='ITA', level='0')
#Kosovo
KSV <- getData('GADM', country='XK', level='0')
#Latvia
LVA <- getData('GADM', country='LVA', level='0')
#Libya
LBY <- getData('GADM', country='LBY', level='0')
#Liechtenstein
LIE <- getData('GADM', country='LIE', level='0')
#Lithuania
LTU <- getData('GADM', country='LTU', level='0')
#Luxembourg
LUX <- getData('GADM', country='LUX', level='0')
#Macedonia
MKD <- getData('GADM', country='MKD', level='0')
#Malta
MLT <- getData('GADM', country='MLT', level='0')
#Moldova
MDA <- getData('GADM', country='MDA', level='0')
#Monaco
MCO <- getData('GADM', country='MCO', level='0')
#Montenegro
MNE <- getData('GADM', country='MNE', level='0')
#Morocco
MAR <- getData('GADM', country='MAR', level='0')
#Netherlands
NLD <- getData('GADM', country='NLD', level='0')
#Norway
NOR <- getData('GADM', country='NOR', level='0')
#Poland
POL <- getData('GADM', country='POL', level='0')
#Portugal
PRT <- getData('GADM', country='PRT', level='0')
#Romania
ROU <- getData('GADM', country='ROU', level='0')
#Russia (Kaliningrad)
RUS <- getData('GADM', country='RUS', level='0')
#San Marino
SMR <- getData('GADM', country='SMR', level='0')
#Serbia
SRB <- getData('GADM', country='SRB', level='0')
#Slovakia
SVK <- getData('GADM', country='SVK', level='0')
#Slovenia
SVN <- getData('GADM', country='SVN', level='0')
#Spain
ESP <- getData('GADM', country='ESP', level='0')
#Sweden
SWE <- getData('GADM', country='SWE', level='0')
#Switzerland
CHE <- getData('GADM', country='CHE', level='0')
#Tunisia
TUN <- getData('GADM', country='TUN', level='0')
#Turkey
TUR <- getData('GADM', country='TUR', level='0')
#Ukraine
UKR <- getData('GADM', country='UKR', level='0')
#Western Sahara
ESH <- getData('GADM', country='ESH', level='0')
}

# Combine the individual high resolution shapefiles into one shapefile
EUR <- rbind(ALB, DZA, AND, AUT, BLR, BEL, BIH, BGR, HRV, CZE, DNK, EGY, EST, FIN, 
             FRA, DEU, GBR, GRC, HUN, ISL, IRL, ITA, KSV, LVA, LBY, LIE, LTU, LUX, 
             MKD, MLT, MNE, MDA, MCO, MAR, NLD, NOR, POL, PRT, ROU, RUS, SRB, SMR, 
             SVK, SVN, ESP, SWE, CHE, TUN, TUR, UKR, ESH)

# Create a blank polygon --------------------------------------------------

# Define boundaries of a rectangle using the extent of the NEMO model (plus a slight buffer) 
x1 = -20
x2 = 20
y1 = 35
y2 = 75

# Create a polygon using these boundaries
myPolygon = Polygon(cbind(c(x1,x1,x2,x2,x1),c(y1,y2,y2,y1,y1)))
myPolygons = Polygons(list(myPolygon), ID = "A")

# Create a polygon list and give each polygon an ID
SpPolygon = SpatialPolygons(list(myPolygons))
plot(SpPolygon)

# Define and apply projection to the polygon
proj = "+proj=longlat +datum=WGS84 +no_defs"
proj4string(SpPolygon) = proj

# Crop the country shapefile from the blank polygon -----------------------

# Create ocean shapefile using the country shapefile by cropping countries from the rectangle
spTransform(EUR, CRSobj="+proj=longlat +datum=WGS84 +no_defs")
ocean_shp <- rgeos::gDifference(SpPolygon, EUR)
plot(ocean_shp)

# Save oceans shapefile to working directory ------------------------------

oceans <- as(ocean_shp, "SpatialPolygonsDataFrame")
#Save the newly created shapefile to the working directory
writeOGR(obj=oceans, dsn=".", layer="oceans", driver="ESRI Shapefile")

# Load the oyster distribution data  --------------------------------------

mg <- read.csv("Data_2021_for_release_file.csv")

# Convert the data to a data frame
mgdf <- as.data.frame(mg)
# Convert it back to spatial points using the CRS of the sstannual data
mg.shp <- SpatialPointsDataFrame(coords = mgdf[,c("Longitude", "Latitude")], data=mgdf)

# Extract the extent of the distribution data -----------------------------

# Export the spatial extent of distribution data
extent <- extent(mg.shp) 
# Specify a buffer
border <- 10

# Create a grid using the spatial extent information ----------------------

# Specify the grid resolution in degrees
# This creates a grid with each cell representing 1 degree of latitiude and longitude
res <- 0.5
# Create a square/rectangular grid using the spatial extent of mg.shp and give each grid cell a unique ID
grid <- rasterToPolygons(raster(xmn = extent@xmin-border, xmx = extent@xmax+border, ymn = extent@ymin-border, ymx = extent@ymax+border, resolution=res))

# Give the polygons unique IDs
slot(grid, "data") <- cbind("ID"=1:length(grid),
                            slot(grid, "data"))

# The grid and species polygon should have the same projection
projection(mg.shp) <- projection(grid)

# Crop the grid to only the ocean region ----------------------------------

ocean_grid <- raster::crop(grid, oceans)
#plot(ocean_grid)
