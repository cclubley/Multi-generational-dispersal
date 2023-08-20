# -------------------------------------------------------------------------
# --------------------- Edge weight and seaway distance -------------------
# -------------------------------------------------------------------------
# Set the working directory ---------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/Data/Data/igraph environments")

# Load the required packages --------------------------------------------------
{ library(reshape2)
  library(igraph)
  library(rworldxtra)
  library(raster)
  library(maps)
  library(ggplot2)
  library(sdmpredictors)
  library(rSDM)
  library(gdistance)
  library(SDraw)
  library(ccplot)
  library(tidyverse)
  library(broom)
  #install.packages("remotes")
  #remotes::install_github("OnofriAndreaPG/aomisc")
  library(aomisc)
  library(wesanderson)
  library(ggExtra)
  library(dplyr)
}

# Load the connectivity environments --------------------------------------

load("./Combined/Combined_2000.RData")
# Re-calculate the number of connections between source and sink polygons for only 
# those where there *were* connections
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
# Save as that year
c_2000 <- myFreqs

# Load the other years in the same way
{ load("./Combined/Combined_2001.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n())
c_2001 <- myFreqs

load("./Combined/Combined_2002.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n())
c_2002 <- myFreqs

load("./Combined/Combined_2003.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2003 <- myFreqs

load("./Combined/Combined_2004.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2004 <- myFreqs

load("./Combined/Combined_2005.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2005 <- myFreqs

load("./Combined/Combined_2006.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2006 <- myFreqs

load("./Combined/Combined_2007.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2007 <- myFreqs

load("./Combined/Combined_2008.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2008 <- myFreqs

load("./Combined/Combined_2009.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2009 <- myFreqs

load("./Combined/Combined_2010.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2010 <- myFreqs

load("./Combined/Combined_2011.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2011 <- myFreqs

load("./Combined/Combined_2012.RData")
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2012 <- myFreqs
}

# Merge the data and calculate the weighted average -----------------------

# Merge the data together
{ connections <- merge(c_2000, c_2001, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, c_2002, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, c_2003, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, c_2004, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, c_2005, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, c_2006, by=c("Release_poly", "Settlement_poly"), all=TRUE)     
connections <- merge(connections, c_2007, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, c_2008, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, c_2009, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, c_2010, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, c_2011, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, c_2012, by=c("Release_poly", "Settlement_poly"), all=TRUE)
}

# Rename the columns
colnames(connections) <- c("Release_poly", "Settlement_poly", "c_2000", "c_2001", "c_2002", "c_2003",
                           "c_2004", "c_2005", "c_2006", "c_2007", "c_2008", "c_2009", "c_2010", 
                           "c_2011", "c_2012")

# Calculate how many years each link appears in 
connections$freq <- rowSums(is.na(connections))
connections$freq <- 13-connections$freq

# Multiply the values by the number of years they appear in to get the weighted value
connections[3:15] <- connections[3:15]*connections$freq

# Now calculate the weighted average across years
connections$avg <- rowMeans(connections[3:15], na.rm=TRUE)

# Create the new myFreqs data frame 
myFreqs <- as.data.frame(connections[, c(1, 2, 17)])
colnames(myFreqs) <- c("Release_poly", "Settlement_poly", "Count")

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the 2012 data
poly_id1 <- as.factor(connectivity$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(connectivity$Settlement_poly)
poly_id2 <- levels(poly_id2)
poly_id2 <- as.data.frame(poly_id2)
colnames(poly_id2) <- "Polygon"

# Combine the release and settlement data
poly_id <- rbind(poly_id1, poly_id2)
# Remove duplicates
poly_id <- poly_id[!duplicated(poly_id), ]

# Create a data frame of all possible polygon combinations
all_pairs <- expand.grid(poly_id, poly_id)
# Re-name the columns
colnames(all_pairs) <- c("Settlement_poly", "Release_poly")
# re-arrange the columns
all_pairs <- all_pairs[,c("Release_poly", "Settlement_poly")]
# Create a function that converts a variable to a numeric factor
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
# Convert both variables to numeric factors
all_pairs$Release_poly <- as.numeric.factor(all_pairs$Release_poly)
all_pairs$Settlement_poly <- as.numeric.factor(all_pairs$Settlement_poly)

rm(poly_id1, poly_id2, poly_id)

# Merge the observed connectivity matrix with all possible combinations -------

# Convert the actual connectivity matrix to a dataframe
myFreqs <- as.data.frame(myFreqs)
# Combine with the dataframe for all possible combinations
myFreqs <- merge(all_pairs, myFreqs, all.x = TRUE)
# remove NA values
myFreqs[is.na(myFreqs)] <- 0

rm(all_pairs)

# Create a square matrix of combinations --------------------------------------

# Create a matrix of the count for each combination of release and settlement polygons
s.dat <- acast(myFreqs, Release_poly~Settlement_poly, value.var = "Count")
# Check the dimensions of the matrix - they must be the same
# Use the polygon IDs here to update the spawning locations file for the year
dim(s.dat) 

# Create a transition layer -------------------------------------------------

# Load a bathymetry raster layer
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)
bathy <- bathy$MS_bathy_5m

# Crop raster to desired boundaries
min_lon <- -20
max_lon <- 20
min_lat <- 35
max_lat <- 65
geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))
coordinates(Sites.grid) <- ~ lon_bound + lat_bound
bathy <- crop(bathy, extent(Sites.grid))

# Clean the environment
rm(Sites.grid, geo_bounds, max_lat, max_lon, min_lat, min_lon)

# Convert projection of raster to UTM (which measures in metres instead of
# decimal degree)
bathy2 <- projectRaster(bathy, crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs" )

# Make land impassable
# Make all ocean cells equal to -999
bathy2[!is.na(bathy2)] <- -999
# Turn all land cells to NA
bathy2[bathy2>-999] <- NA
# Assign all ocean cells a value of 1
bathy2[bathy2==-999] <- 1

# Checks whether cells are not NA and then creates a matrix listing all feasible connections between cells
trb <- transition(bathy2, mean, directions = 16) 
# Corrects for fact that diagonal movement is longer than straight lines
trb <- geoCorrection(trb, "c") 

rm(bathy2)

# Calculate the seaway distance between nodes -----------------------------

# Set up empty data frame to enter distances into
Distances <- myFreqs
Distances$Count <- NULL
Distances$Distance <- NA

# Give the centroids an ID column
centroids$ID <- row.names(centroids)

# Isolate the settlement polygons
Settle <- as.data.frame(myFreqs$Settlement_poly)
colnames(Settle) <- "ID"
# Add the corrdinate information
Settle <- merge(Settle, centroids, by="ID", all=TRUE)
# Convert to a spatial points data frame
Settlesp <- SpatialPointsDataFrame(coords=Settle[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=Settle)

# Move points off of land
Settlesp <- points2nearestcell(Settlesp, bathy)
# Convert projection of points to UTM
Settlesp <- spTransform(Settlesp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))

# Give a distance column
Settlesp$Distance <- NA
# Remove duplicate polygons
Settlesp <- Settlesp[!duplicated(Settlesp$ID), ]

# Create an empty distances matrix
dist <- matrix(nrow=length(Settlesp@data$ID), ncol=length(Settlesp@data$ID))
colnames(dist) <- Settlesp@data$ID
rownames(dist) <- Settlesp@data$ID

# Duplicate the spdf
x <- Settlesp
y <- x

# Calculate the seaway distance between nodes
# I couldn't figure out a way to automate this so you have to repeat manually 
# until all nodes have been calculated (a LONG process)
AtoB <- (shortestPath(trb, y, x, output="SpatialLines")) # Calculate distances  
dist[130, ] <- lineLength(AtoB, byid=TRUE)/1000 # Work out length and fill in matrix
y <- y[-1,] # Delete top row
# Repeat untildata frame y is empty

# Edit and save the data frame --------------------------------------------

# Convert to long format
distances <- as.data.frame(melt(dist))
colnames(distances) <- c("Release_poly", "Settlement_poly", "Distance")

# Merge the distances with the edge weight
distances <- merge(distances, myFreqs, by=c("Release_poly", "Settlement_poly"), all=TRUE)

# Save the data 
#setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/(4) Results/(Part 1) Ecology/Distance and weight")
#write.csv(distances, "./Distances.csv", row.names=FALSE)

# Plot with edge weight = 0 removed
distances2 <- filter(distances, Count > 0 & Distance > 0)
#write.csv(distances2, "./Distances_no_zero.csv", row.names=FALSE)

# Read the data -----------------------------------------------------------

setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/(4) Results/(1) Graph-level analysis/(Fig 4) Link weight and distance")
distances2 <- read.csv("./Distances_no_zero.csv")

# Summarise and plot ------------------------------------------------------

# Plot distances with count > 0 and no self-recruitment
ggplot(data=distances2, aes(x=Distance, y=Count))+
  geom_point()

# Summary, all edges with weight > 0 and no self-recruitment
summary(distances2$Distance)
plotrix::std.error(distances2$Distance)
# The greatest distance between two nodes with a connection was 923.1514 km
# The minimum distance (not including self-recruitment) was 11.92 km
# The average distance was 206.35 km 

dist <- filter(distances2, Distance <=260)
(nrow(dist)/nrow(distances2))*100
# 75% of links are 260 km or less apart

dist <- filter(distances2, Distance <=175)
(nrow(dist)/nrow(distances2))*100
# 50% of links are 175 km or less apart

dist <- filter(distances2, Distance <=115)
(nrow(dist)/nrow(distances2))*100
# 25% of links are 115 km or less apart

# Fit an exponential decay curve ------------------------------------------

# https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
# https://tjmurphy.github.io/jabstb/nonlinearreplicates.html 

# Nonlinear regression is a statistical method to fit nonlinear models to the 
# kinds of data sets that have nonlinear relationships between independent and 
# dependent variables.

# In nonlinear regression we fit a model formula to pairs of X,Y data from an 
# experiment. The best fitting model parameters responsible for giving nonlinear 
# shape to the relationship between X and Y are then determined by the regression
# method.

# Best fit of the regression curve is determined by a minimization of the sum of 
# the squared residuals between the model values for the reponse and 
# experimentally-derived values.

# Exponential decay using self-starting functions

# y(t) ~ yf + (y0 - yf)e-at

# The measured y value starts at y0 and decays towards yf at a rate of a

ggplot(data=distances2, aes(x=Distance, y=Count))+
  geom_point()

# 'nls' is the standard R base function to fit non-linear equations.
# However trying to fit exponential decay with nls doesn't work if you pick a 
# bad initial guess for the rate constant (a).

# A solution is to use a self-starting function - a special function for curve 
# fitting that guesses its own starting parameters. 
# The asymptotic regression function 'SSasymp' is equivalet to exponential decay.
# y and x in the formula are replaced by Count and Distance.
fit <- nls(Count~SSasymp(Distance, yf, y0, log_alpha), data=distances2)
fit
# Instead of fitting the rate constant directly, it searched for the logarithm 
# of a:
# y(t) ~ yf + (y0 - yf) * e ^ (-exp(log a)) * t

# Extract the parameters
paras <- summary(fit)$coeff[,1]
paras

fit
# The fact the regression converges to a solution suggests a good fit, but 
# that's not always the case. Nonlinear regression resolves parameter values by
# an iterative algorithm that converges onto the solution (a stable minimization 
# of residual error). Here, this took 5 cycles.

# Non-linear models do not have an intercept term, so most authors advocate 
# against the use of R-squared in nls analysis. 
# We can calculate pseudo-R-squared, but it cannot be interpreted as the 
# proportion of variance explained by the model.
R2nls(fit)$PseudoR2
# 0.17

# Graphically check the fitted curve
ggplot(data=augment(fit), aes(x=Distance, y=Count))+
  geom_point()+
  geom_line(aes(y=.fitted), col="red")

## Trying an alternative method

## Use 'NLS.expoDecay' from the 'aomisc' package instead
#model <- nls(Count ~ NLS.expoDecay(Distance, a, k),
             #data = distances2)
#summary(model)

#ggplot(data=augment(model), aes(x=Distance, y=Count))+
  #geom_point()+
  #geom_line(aes(y=.fitted), col="red")

## Compare the two methods
#anova(fit, model, test="F")
## No significant difference - the two models are essentially the same
## Let's go with the 'fit' model as it uses a self-start function

plot <- ggplot(data=augment(fit), aes(x=Distance, y=Count))+
  geom_point()+
  geom_line(aes(y=.fitted), col="red", size=1)+
  cc_theme()

#Add a marginal density plot
plot2 <- ggMarginal(plot, type="density")
plot2


