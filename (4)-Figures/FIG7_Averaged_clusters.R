# Load packages and set wd ------------------------------------------------
{ library(dplyr)
  library(reshape2)
  library(igraph)
  library(raster)
  library(ggplot2)
  library(ccplot)
  library(rnaturalearth)
  library(wesanderson)
  library(maps)
  library(tidyr)
}

setwd("C:/Users/charl/Documents/PhD/Chapters/(3) Dispersal Limitation/Data/Data/igraph environments")
load("./Combined/Combined_2012.RData")
setwd("C:/Users/charl/Documents/PhD/Chapters/(3) Dispersal Limitation/(4) Results/(1) Graph-level analysis/(Fig 3) Averaged link weight/New averaged link weights")

# Load the new weighted averages
connectivity <- read.csv("Weighted_average_graph_new.csv")

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons
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

# Combine with the dataframe for all possible combinations
myFreqs <- merge(all_pairs, connectivity, all.x = TRUE)
# remove NA values
myFreqs[is.na(myFreqs)] <- 0

rm(all_pairs)

# Create a square matrix of combinations --------------------------------------

# Create a matrix of the count for each combination of release and settlement polygons
s.dat <- acast(myFreqs, Release_poly~Settlement_poly, value.var = "Count")
# Check the dimensions of the matrix - they must be the same
# Use the polygon IDs here to update the spawning locations file for the year
dim(s.dat) 

# Create a basic igraph ------------------------------------------------------------

# Create a directed igraph from the adjacency matrix
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# directed means the directionality of movement is taken into account
# weighted means the number of particles travelling along each edge is taken into account
# diag=FALSE means that we are not involving self-recruitment - this is for aesthetics with the map

# Give each node the correct lat and lon value
coords <- centroids[match(V(g)$name, rownames(centroids)), ]
V(g)$Latitude  <- coords[,"lat"] 
V(g)$Longitude <- coords[,"lon"] 

# Community detection -----------------------------------------------------

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)

# How many communities are there?
length(ceb)
# 10

# Plot in different styles --------------------------------------

library(rworldxtra)
library(maps)

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
# Bridges between communities account for 5.81% of all particle movement
# How much particle exchange occurs within clusters?
100-5.807264
# what % of links occurred between clusters?
(nrow(bridge2)/sum(nrow(bridge)))*100

nonbridge <- merge(nonbridge, myFreqs, by=c("Release_poly", "Settlement_poly"), all=FALSE)
(sum(nonbridge$Count)/sum(myFreqs$Count))*100
# Particles exchanged within communities account for 69.380% of all particle movement

# The final ~25% is accounted for by self-recruitment

modularity(ceb)
# Modularity is 0.7818394