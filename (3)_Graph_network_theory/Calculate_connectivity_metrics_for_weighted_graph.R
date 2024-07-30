# Set wd ------------------------------------------------------------------

{library(reshape2)
library(dplyr)
library(rnaturalearth)
library(viridis)
library(ggplot2)
library(ccplot)
library(aomisc)
library(broom)
library(igraph)
library(ggpubr)
library(sf)
library(raster)
library(rworldxtra)
library(maps)
library(tidyr)
}

# Load the data -----------------------------------------------------------

load("./Combined.RData")

# -------------------------------------------------------------------------
# ----------------------------- Figure 3 ----------------------------------
# -------------------------------------------------------------------------
# Plotting the edges on a map ---------------------------------------------

# Convert the coordinates of the nodes into a matrix
coords$ID <- NULL
coords <- as.matrix(coords)

coords.df <- as.data.frame(coords)  ## convert the layout to a data.frame
coords.df$ID <- rownames(coords)  ## add in the polygon IDs

# remove local retention/self-recruitment
myFreqs_unique <- myFreqs[myFreqs$Release_poly != myFreqs$Settlement_poly, ] 
myFreqs_unique <- filter(myFreqs_unique, Count > 0) 

# Create a separate data frame for only local retention/self-recruitment
myFreqs_sr <- myFreqs[myFreqs$Release_poly == myFreqs$Settlement_poly, ]
myFreqs_sr <- filter(myFreqs_sr, Count > 0) # Removes 9 rows

# Convert the data frame to something 'plotable'
head(myFreqs_unique)
myFreqs_unique$from.x <- coords.df$lon[match(myFreqs_unique$Release_poly, coords.df$ID)]  #  match the from locations from the node data.frame we previously connected
myFreqs_unique$from.y <- coords.df$lat[match(myFreqs_unique$Release_poly, coords.df$ID)]
myFreqs_unique$to.x <- coords.df$lon[match(myFreqs_unique$Settlement_poly, coords.df$ID)]  #  match the to locations from the node data.frame we previously connected
myFreqs_unique$to.y <- coords.df$lat[match(myFreqs_unique$Settlement_poly, coords.df$ID)]
head(myFreqs_unique)

# Normalise link weight between 0 and 1 to aid interpretation
myFreqs_unique$norm_count <- (myFreqs_unique$Count-min(myFreqs_unique$Count))/(max(myFreqs_unique$Count)-min(myFreqs_unique$Count))

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "medium", returnclass = "sf")

# Re-order the data frame so strongest links appear on top
myFreqs_unique <- myFreqs_unique[order(myFreqs_unique$norm_count, decreasing=FALSE), ]

# Plot with perceptually uniform colours ---------------------------------------

# Perceptually uniform colour palette
cols <- magma(200)
cols <- cols[200:100]

# Plot all links together
ggplot(data=world) +
  geom_sf(colour="black", fill="lightgrey")+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+ #Crop map to desired area
  geom_segment(data=myFreqs_unique, aes(x=from.x, xend=to.x, y=from.y, yend=to.y, 
                                        col=norm_count)) +
  scale_color_viridis(option="A", direction=-1, begin=0, end=.9)+
  geom_point(data=coords.df, aes(x=lon, y=lat), size=1.2, colour="black") +
  xlab("Longitude")+
  ylab("Latitude")+
  cc_theme()+
  theme(legend.position="right")

# Split the scale to values < 0.1 and >= 0.1  
summary(myFreqs_unique$Count)
plotrix::std.error(myFreqs_unique$Count)
myFreqs_lower <- filter(myFreqs_unique, norm_count < 0.1)
max(myFreqs_lower$norm_count)
myFreqs_lower <- myFreqs_lower[order(myFreqs_lower$norm_count, decreasing=FALSE), ]

myFreqs_upper <- filter(myFreqs_unique, norm_count >= 0.1)
min(myFreqs_upper$norm_count)
myFreqs_upper <- myFreqs_upper[order(myFreqs_upper$norm_count, decreasing=FALSE), ]

# Plot links < 0.1
ggplot(data=world) +
  geom_sf(colour="black", fill="lightgrey")+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+ #Crop map to desired area
  geom_segment(data=myFreqs_lower, aes(x=from.x, xend=to.x, y=from.y, yend=to.y, col=norm_count)) +
  scale_color_viridis(limits=c(0, 0.1), option="A", direction=-1, begin=0, end=.9)+
  geom_point(data=coords.df, aes(x=lon, y=lat), size=1.2, colour="black") +
  xlab("Longitude")+
  ylab("Latitude")+
  cc_theme()+
  theme(legend.position="right")

# Plot links >= 0.1
ggplot(data=world) +
  geom_sf(colour="black", fill="lightgrey")+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+ #Crop map to desired area
  geom_segment(data=myFreqs_upper, aes(x=from.x, xend=to.x, y=from.y, yend=to.y, col=norm_count)) +
  scale_color_viridis(limits=c(0.1, 1), option="A", direction=-1, begin=0, end=.9)+
  geom_point(data=coords.df, aes(x=lon, y=lat), size=1.2, colour="black") +
  xlab("Longitude")+
  ylab("Latitude")+
  cc_theme()+
  theme(legend.position="right")

# Calculate local retention metrics ---------------------------------------

# Calculate percentage of links that were self-recruitment
(nrow(myFreqs_sr)/nrow(myFreqs))*100
# self-recruitment accounted for 0.5509735% of connections - same as before!

# Calculate percentage contribution of self recruitment to total edge weight
(sum(myFreqs_sr$Count)/sum(myFreqs$Count))*100
# Self-recruitment accounted for 31.26563% of edge weight 

# Density of edges --------------------------------------------------------

# Plot a histogram of normalised link weight
ggplot(myFreqs_unique, aes(x=norm_count))+
  geom_histogram()+
  geom_vline(aes(xintercept=median(norm_count)),
             color="blue", linetype="dashed", size=1)+
  #ylim(c(0, 1500))+
  xlab("Normalised Link weight")+
  ylab("Count")+
  cc_theme()

median(myFreqs_unique$norm_count)
# 0.002539013

# -------------------------------------------------------------------------
# ----------------------------- Figure 4 ----------------------------------
# -------------------------------------------------------------------------
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

# Plot with edge weight = 0 removed
distances2 <- filter(distances, Count > 0 & Distance > 0)
distances2$Count <- NULL

new_data <- merge(distances2, myFreqs, by=(c("Release_poly", "Settlement_poly")))
colnames(new_data) <- c("Release_poly", "Settlement_poly", "Distance", "avg")

summary(new_data$avg)
plotrix::std.error(new_data$avg)

# Summarise and plot ------------------------------------------------------

# Plot distances with count > 0 and no self-recruitment
ggplot(data=new_data, aes(x=Distance, y=avg))+
  geom_point()

# Summary, all edges with weight > 0 and no self-recruitment
summary(new_data$Distance)
plotrix::std.error(new_data$Distance)
# The greatest distance between two nodes with a connection was 923.15 km
# The minimum distance (not including self-recruitment) was 11.92 km
# The median distance was 176.73 km
# The average distance was 206.35 km +- 2.861289 

# All those values are the same as before!

dist <- filter(new_data, Distance <=260)
(nrow(dist)/nrow(new_data))*100
# 75% of links are 260 km or less apart

dist <- filter(new_data, Distance <=175)
(nrow(dist)/nrow(new_data))*100
# 50% of links are 175 km or less apart

dist <- filter(new_data, Distance <=115)
(nrow(dist)/nrow(new_data))*100
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
# the squared residuals between the model values for the response and 
# experimentally-derived values.

# Exponential decay using self-starting functions

# y(t) ~ yf + (y0 - yf)e-at

# The measured y value starts at y0 and decays towards yf at a rate of a

ggplot(data=new_data, aes(x=Distance, y=avg))+
  geom_point()

# 'nls' is the standard R base function to fit non-linear equations.
# However trying to fit exponential decay with nls doesn't work if you pick a 
# bad initial guess for the rate constant (a).

# A solution is to use a self-starting function - a special function for curve 
# fitting that guesses its own starting parameters. 
# The asymptotic regression function 'SSasymp' is equivalet to exponential decay.
# y and x in the formula are replaced by Count and Distance.
fit <- nls(avg~SSasymp(Distance, yf, y0, alpha), data=new_data)
fit
# Instead of fitting the rate constant directly, it searched for the logarithm 
# of a:
# y(t) ~ yf + (y0 - yf) * e ^ (-exp(a)) * t

# Extract the parameters
paras <- summary(fit)$coeff[,1]
paras

# alpha = exp(alpha)
exp(-4.28177)

# y(t) ~ 13.96 + (127539.20 - 13.96) * e ^ -0.014*t

fit
# The fact the regression converges to a solution suggests a good fit, but 
# that's not always the case. Nonlinear regression resolves parameter values by
# an iterative algorithm that converges onto the solution (a stable minimization 
# of residual error). Here, this took 6 cycles.

# Non-linear models do not have an intercept term, so most authors advocate 
# against the use of R-squared in nls analysis. 
# We can calculate pseudo-R-squared, but it cannot be interpreted as the 
# proportion of variance explained by the model.
aomisc::R2nls(fit)$PseudoR2
# 0.21 

# Graphically check the fitted curve
ggplot(data=augment(fit), aes(x=Distance, y=avg))+
  geom_point()+
  geom_line(aes(y=.fitted), col="red", size=1)+
  cc_theme()

# -------------------------------------------------------------------------
# ----------------------------- Figure 5 ----------------------------------
# -------------------------------------------------------------------------
# Create new matrix ---------------------------------

s.dat <- acast(myFreqs, Release_poly~Settlement_poly, value.var = "Count")
# Check the dimensions of the matrix - they must be the same
# Use the polygon IDs here to update the spawning locations file for the year
dim(s.dat)

# Create graph object -----------------------------------------------------

# Create a directed igraph from the adjacency matrix
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=TRUE)
# Give each node the correct lat and lon value
coords <- centroids[match(V(g)$name, rownames(centroids)), ]
V(g)$Latitude  <- coords[,"lat"] 
V(g)$Longitude <- coords[,"lon"] 

# Calculate in degree and out degree --------------------------------------

# calculate node out degree
# out degree = number of outgoing edges from the node (i.e. importance as a source)
deg_out <- degree(g, v=V(g), mode="out")
# Convert to data frame
out_degree <- as.data.frame(deg_out)
out_degree$Node <- rownames(out_degree)

# calculate node in degree
# in degree = number of incoming edges to a node (i.e. importance as a sink)
deg_in <- degree(g, v=V(g), mode="in")
# Convert to data frame to add to spreadsheet
in_degree <- as.data.frame(deg_in)
in_degree$Node <- rownames(in_degree)

# Calculate total degree (out degree + in degree)
deg <- degree(g, v=V(g), mode="all")
deg <- as.data.frame(deg)
deg$Node <- rownames(deg)

# Merge together
metrics <- merge(out_degree, in_degree, by=c("Node"), all=TRUE)
metrics <- merge(metrics, deg, by=c("Node"), all=TRUE)
metrics[is.na(metrics)] <- 0

# Analyse and plot --------------------------------------------------------

# Quick visualisation
ggplot(data=metrics, aes(x=deg_out, y=deg_in))+
  geom_point()+
  geom_smooth()
# Looks like it could be linear

# Try a linear model
mod <- lm(deg_in~deg_out, data=metrics)
mod
# Regression equation: y = 6.84 + 0.50*x     OR     In degree = 6.84 + 0.50 * Out degree
par(mfrow = c(2,2))
plot(mod)
# Looks okay to me!

# Check the residuals for normality
shapiro.test(residuals(mod))
# P-value = 0.64
# Residuals are fine

# Summarise the model
summary(mod)
# RSE = 4.42 on 170 DoF
# Multiple R-squared = 0.3213
# Adjusted R-squared = 0.3173
# F-stat = 80.48
# P-value = 5.254e-16 
# In degree increases by 0.4997 +- 0.0557 for every 1 unit increase in out degree

# Carry out correlation analysis to determine the strength of the linear relationship
corr <- cor.test(x=metrics$deg_out, y=metrics$deg_in, method='pearson')
corr
# r = 0.5668259

# Proper plot
ggplot(data=metrics, aes(x=deg_out, y=deg_in))+
  geom_point(size=2.5)+
  geom_smooth(method="lm")+
  stat_regline_equation(label.x=0, label.y=30, size=5)+
  stat_cor(aes(label=..rr.label..), label.x=0, label.y=29, size=5)+
  #xlim(c(0,35))+
  ylim(c(0, 30))+
  xlab("Out degree")+
  ylab("In degree")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))

# Plot as a map -----------------------------------------------------------

# Load grid
ocean_grid <- st_read(dsn=".", layer="ocean_grid")

# calculate node out degree
# out degree = number of outgoing edges from the node (i.e. importance as a source)
deg_out <- degree(g, v=V(g), mode="out")
# Convert to data frame
out_degree <- as.data.frame(deg_out)
out_degree$ID <- rownames(out_degree)

# calculate node in degree
# in degree = number of incoming edges to a node (i.e. importance as a sink)
deg_in <- degree(g, v=V(g), mode="in")
# Convert to data frame to add to spreadsheet
in_degree <- as.data.frame(deg_in)
in_degree$ID <- rownames(in_degree)

# merge the grid and the particle frequencies
grid <- merge(ocean_grid, out_degree, by.x="ID", by.y="ID")
grid_out <- st_as_sf(grid)

# merge the grid and the particle frequencies
grid <- merge(ocean_grid, in_degree, by.x="ID", by.y="ID")
grid_in <- st_as_sf(grid)

# Load country shapefile
data(countriesHigh)
countries <- crop(countriesHigh, extent(-13, 13, 43, 65))
#plot(countries)
rm(countriesHigh)

# Plot out degree
ggplot()+ 
  scale_fill_viridis(option="A", direction=-1, begin=0, end=.9)+
  cc_theme()+
  theme(legend.position="right", 
        strip.text.x=element_text(size=5, margin=margin(0, 0, 0, 0, "cm")))+
  geom_sf(data=grid_out, aes(fill=deg_out), colour=NA)+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+
  geom_polygon(countries, col="black", fill="light grey", mapping=aes(x=long, y=lat, group=group))

# Plot in degree
ggplot()+ 
  scale_fill_viridis(limits=c(0, 30), option="A", direction=-1, begin=0, end=.9)+
  cc_theme()+
  theme(legend.position="right", 
        strip.text.x=element_text(size=5, margin=margin(0, 0, 0, 0, "cm")))+
  geom_sf(data=grid_in, aes(fill=deg_in), colour=NA)+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+
  geom_polygon(countries, col="black", fill="light grey", mapping=aes(x=long, y=lat, group=group))

# -------------------------------------------------------------------------
# ----------------------------- Figure 7 ----------------------------------
# -------------------------------------------------------------------------
# Community detection -----------------------------------------------------

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)

# How many communities are there?
length(ceb)
# 13

# Plot in different styles --------------------------------------

# Load country shapefile
data(countriesHigh)
# Crop to area of interest
map_crop <- crop(countriesHigh, extent(-13, 13, 43, 65))

#Convert the coordinates of the nodes into a matrix
coords <- as.matrix(coords)

# Plot on map without edges
plot(map_crop, col="light grey")
# Set edges of graph all to same weight/width - otherwise becomes too messy
E(g)$width <- 1
# Add the igraph with communities
plot(ceb, g, add=TRUE, rescale=FALSE,layout=coords, axes = FALSE, vertex.size=5,
     vertex.label=NA, arrow.size=.5, edge.arrow.size=0.1, edge.size=.5, edge.curved=0.2,
     edge.color=NA)
map.axes(cex.axis=1) # map border and axis

# Identify bridges --------------------------------------------------------

# Identify bridges between clusters
bridge <- as.data.frame(igraph::crossing(ceb, g))

# Keep only bridges
bridge2 <- filter(bridge, igraph::crossing(ceb,g)=="TRUE")
# Isolate non-bridges
nonbridge <- anti_join(bridge, bridge2)
# FOrmate data frame
bridge2$nodes <- rownames(bridge2)
bridge2 <- bridge2 %>% tidyr::separate(nodes, c("Release_poly", "Settlement_poly"))
bridge2$`igraph::crossing(ceb, g)` <- NULL

nonbridge$nodes <- rownames(nonbridge)
nonbridge <- nonbridge %>% separate(nodes, c("Release_poly", "Settlement_poly"))
nonbridge$`igraph::crossing(ceb, g)` <- NULL

bridge2 <- merge(bridge2, myFreqs, by=c("Release_poly", "Settlement_poly"), all=FALSE)
(sum(bridge2$Count)/sum(myFreqs$Count))*100
# Bridges between communities account for 6.27% of all particle movement
# How much particle exchange occurs within clusters?
100-6.272169 
# what % of links occurred between clusters?
(nrow(bridge2)/sum(nrow(bridge)))*100

# The final ~6% is accounted for by self-recruitment

modularity(ceb)
# Modularity is 0.8383671
