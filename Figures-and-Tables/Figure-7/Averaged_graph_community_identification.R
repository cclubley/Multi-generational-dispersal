# -------------------------------------------------------------------------
# ------------------ Averaged graph cluster identification ----------------
# -------------------------------------------------------------------------
# Set the working directory ---------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/Data/Data/igraph environments")

# Load the required packages --------------------------------------------------

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
{connections <- merge(c_2000, c_2001, by=c("Release_poly", "Settlement_poly"), all=TRUE)
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

# Plot in different styles --------------------------------------

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
bridge2 <- bridge2 %>% separate(nodes, c("Release_poly", "Settlement_poly"))
bridge2$`igraph::crossing(ceb, g)` <- NULL

nonbridge$nodes <- rownames(nonbridge)
nonbridge <- nonbridge %>% separate(nodes, c("Release_poly", "Settlement_poly"))
nonbridge$`igraph::crossing(ceb, g)` <- NULL

bridge2 <- merge(bridge2, myFreqs, by=c("Release_poly", "Settlement_poly"), all=FALSE)
(sum(bridge2$Count)/sum(myFreqs$Count))*100
# Bridges between communities account for 6.36% of all particle movement

nonbridge <- merge(nonbridge, myFreqs, by=c("Release_poly", "Settlement_poly"), all=FALSE)
(sum(nonbridge$Count)/sum(myFreqs$Count))*100
# Particles exchanged within communities account for 74.30% of all particle movement

# The final ~25% is accounted for by self-recruitment
