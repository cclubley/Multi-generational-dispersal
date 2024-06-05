# Set the working directory ---------------------------------------------------
#setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/(4) Results/igraph creation and data extraction/Data")

# Load the required packages --------------------------------------------------
{ library(dplyr)
  library(reshape2)
  library(igraph)
  library(rworldxtra)
  library(raster)
  library(maps)
}

# -------------------------------------------------------------------------
# -------------- Load the 14 day PLD connectivity environment -------------

load("./igraph environments/14 PLD/PLD14_2012.RData")

# -------------------------------------------------------------------------
# Remove the unwanted polygons --------------------------------------------

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Summarise the polygon connectivity matrix --------------------------

# Calculate the number of connections between source and sink polygons
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  summarise(Count = n()) 

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

# Save the R Environment for future use ---------------------

rm(as.numeric.factor)
# Save the environment
save.image(file='./igraph environments/14 PLD/PLD14_2012.RData')

# Create a basic igraph ------------------------------------------------------------

# Create a directed igraph from the adjacency matrix
g_14 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# directed means the directionality of movement is taken into account
# weighted means the number of particles travelling along each edge is taken into account
# diag=FALSE means that we are not involving self-recruitment - this is for aesthetics with the map

# Give each node the correct lat and lon value
coords <- centroids[match(V(g_14)$name, rownames(centroids)), ]
V(g_14)$Latitude  <- coords[,"lat"] 
V(g_14)$Longitude <- coords[,"lon"] 
coords_14 <- coords

# -------------------------------------------------------------------------
# -------------- Load the 17 day PLD connectivity environment -------------

load("./igraph environments/17 PLD/PLD17_2012.RData")

# -------------------------------------------------------------------------
# Remove the unwanted polygons --------------------------------------------

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Summarise the polygon connectivity matrix --------------------------

# Calculate the number of connections between source and sink polygons
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  summarise(Count = n()) 

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

# Save the R Environment for future use ---------------------

rm(as.numeric.factor)
# Save the environment
save.image(file='./igraph environments/17 PLD/PLD17_2012.RData')

# Create a basic igraph ------------------------------------------------------------

# Create a directed igraph from the adjacency matrix
g_17 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# directed means the directionality of movement is taken into account
# weighted means the number of particles travelling along each edge is taken into account
# diag=FALSE means that we are not involving self-recruitment - this is for aesthetics with the map

# Give each node the correct lat and lon value
coords <- centroids[match(V(g_17)$name, rownames(centroids)), ]
V(g_17)$Latitude  <- coords[,"lat"] 
V(g_17)$Longitude <- coords[,"lon"] 
coords_17 <- coords

# -------------------------------------------------------------------------
# -------------- Load the 25 day PLD connectivity environment -------------

load("./igraph environments/25 PLD/PLD25_2012.RData")

# -------------------------------------------------------------------------
# Remove the unwanted polygons --------------------------------------------

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Summarise the polygon connectivity matrix --------------------------

# Calculate the number of connections between source and sink polygons
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  summarise(Count = n()) 

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

# Save the R Environment for future use ---------------------

rm(as.numeric.factor)
# Save the environment
save.image(file='./igraph environments/25 PLD/PLD25_2012.RData')

# Create a basic igraph ------------------------------------------------------------

# Create a directed igraph from the adjacency matrix
g_25 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# directed means the directionality of movement is taken into account
# weighted means the number of particles travelling along each edge is taken into account
# diag=FALSE means that we are not involving self-recruitment - this is for aesthetics with the map

# Give each node the correct lat and lon value
coords <- centroids[match(V(g_25)$name, rownames(centroids)), ]
V(g_25)$Latitude  <- coords[,"lat"] 
V(g_25)$Longitude <- coords[,"lon"] 
coords_25 <- coords

# -------------------------------------------------------------------------
# -------------- Load the 28 day PLD connectivity environment -------------

load("./igraph environments/28 PLD/PLD28_2012.RData")

# -------------------------------------------------------------------------
# Remove the unwanted polygons --------------------------------------------

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Summarise the polygon connectivity matrix --------------------------

# Calculate the number of connections between source and sink polygons
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  summarise(Count = n()) 

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

# Save the R Environment for future use ---------------------

rm(as.numeric.factor)
# Save the environment
save.image(file='./igraph environments/28 PLD/PLD28_2012.RData')

# Create a basic igraph ------------------------------------------------------------

# Create a directed igraph from the adjacency matrix
g_28 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# directed means the directionality of movement is taken into account
# weighted means the number of particles travelling along each edge is taken into account
# diag=FALSE means that we are not involving self-recruitment - this is for aesthetics with the map

# Give each node the correct lat and lon value
coords <- centroids[match(V(g_28)$name, rownames(centroids)), ]
V(g_28)$Latitude  <- coords[,"lat"] 
V(g_28)$Longitude <- coords[,"lon"] 
coords_28 <- coords

# -------------------------------------------------------------------------
# ------------------ igraph creation and data extraction ------------------
# -------------------------------------------------------------------------
# Combine into one graph object -------------------------------------------

# Combine the different graph attributes and only keep unique vertices
attrs <- rbind(as_data_frame(g_14, "vertices"), as_data_frame(g_17, "vertices"), 
               as_data_frame(g_25, "vertices"), as_data_frame(g_28, "vertices")) %>% unique()
attrs <- attrs[!duplicated(attrs[,c('name')]),]
# Combine the different edge lists
el <- rbind(as_data_frame(g_14), as_data_frame(g_17), as_data_frame(g_25), as_data_frame(g_28))
# Add the edges together
el_d <- data.table::data.table(el) # Convert to data table
el_d <- el_d[, lapply(.SD, sum), by=list(from, to)]
el_d <- as.data.frame(el_d)
g <- graph_from_data_frame(el_d, directed=TRUE, vertices=attrs)

coords_14$ID <- row.names(coords_14)
coords_17$ID <- row.names(coords_17)
coords_25$ID <- row.names(coords_25)
coords_28$ID <- row.names(coords_28)
coords <- rbind(coords_14, coords_17, coords_25, coords_28) %>% unique()
coords <- coords[!duplicated(coords[3]),]

save.image(file='./igraph environments/Combined/Combined_2012.RData')

# Community detection -----------------------------------------------------

# The louvain algorithm for detecting communities only works on undirected graphs.
# The usual way of implementing louvain is to consider directed graphs as undirected
# as suggested by the author of python-louvain.
g2 <- as.undirected(g)

# Community detection is based on modularity
ceb <- cluster_fast_greedy(g2, weights=NULL)

# How many communities are there?
length(ceb)

# How modular is the graph partitioning? - measures how separated the different communities are from each other
modularity(ceb)
# High modularity (/1) reflects dense connections within communities and sparse 
# connections across communities

# List the community membership for each node
membership(ceb)

# Which edges act as bridges between communities? - i.e. a path that connects two communities
bridge <- as.data.frame(igraph::crossing(ceb, g))
# Interested in only those that ARE bridges
bridge <- filter(bridge, igraph::crossing(ceb,g)=="TRUE")

colnames(bridge) <- "2012"
write.csv(bridge, "./Bridge/2012.csv")

## Plotting the iGraph and communities on a map --------------------------------

# Load country shapefile
data(countriesHigh)
# Crop to area of interest
map_crop <- crop(countriesHigh, extent(-13, 13, 43, 65))

#Convert the coordinates of the nodes into a matrix
coords$ID <- NULL
coords <- as.matrix(coords)

# Plot the map
plot(map_crop, col="light grey")
# Set edges of graph all to same weight/width - otherwise becomes too messy
E(g)$width <- 1
# Add the igraph with communities
plot(ceb, g, add=TRUE, rescale=FALSE,layout=coords, axes = FALSE, vertex.size=5,
     vertex.label=NA, arrow.size=.5, edge.arrow.size=0.1, edge.size=.5, edge.curved=0.2,
     edge.color='blue')
map.axes(cex.axis=1) # map border and axis

# Calculate node degree, edge weight and betweenness ----------------------------

# Create a new igraph including self-recruitment (diag=TRUE)
g2 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=TRUE)
# Give each node the correct lat and lon value
coords <- centroids[match(V(g2)$name, rownames(centroids)), ]
V(g2)$Latitude  <- coords[,"lat"] 
V(g2)$Longitude <- coords[,"lon"] 

# calculate node out degree
# out degree = number of outgoing edges from the node (i.e. importance as a source)
deg_out <- degree(g2, v=V(g2), mode="out")
# Convert to data frame
out_degree <- as.data.frame(deg_out)
# Select only data with out degree greater than 0 because these are the original release locations
out_degree <- filter(out_degree, deg_out > 0) 

colnames(out_degree) <- "2012"
write.csv(out_degree, "./Out degree/2012.csv")

# calculate node in degree
# in degree = number of incoming edges to a node (i.e. importance as a sink)
deg_in <- degree(g2, v=V(g2), mode="in")
# Convert to data frame to add to spreadsheet
in_degree <- as.data.frame(deg_in)

colnames(in_degree) <- "2012"
write.csv(in_degree, "./In degree/2012.csv")

# Convert the edges with weights to a data frame
edges <- filter(myFreqs, Count > 0)

colnames(edges) <- c("Release_poly", "Settlement_poly", "2012")
# weight/count is the number of particles that passed from one node to another 
write.csv(edges, "./Edges/2012.csv", row.names = FALSE)

# Calculate node betweenness
# gives an indication of the fraction of shortest paths between pairs of nodes that run through the node
# high betweenness = a large number of particles pass through that node on the way to their sink location
v_between <- as.data.frame(betweenness(g2, directed=T))
colnames(v_between) <- "betweenness"
v_between <- filter(v_between, betweenness>0)

colnames(v_between) <- "2012"
write.csv(v_between, "./Node betweenness/2012.csv")

# Calculate closeness
close <- as.data.frame(closeness(g, mode="all", weights=NA))
colnames(close) <- "2000"
write.csv(close, "./Node closeness/2000.csv")

# Calculate edge betweenness
# gives an indication of the number of shortest paths between pairs of nodes that run along the specified edge
# high betweenness means a large number of particles pass along that edge on the way to their sink location
e_between <- as.data.frame(edge_betweenness(g2, directed=T))
colnames(e_between) <- "2012"
# merge with the edges and their weights
e_between <- cbind(edges[, c(1:2)], e_between) 
# Write to file to speed up process
write.csv(e_between, "./Edge betweenness/2012.csv", row.names = FALSE)

# Calculate network assortativity
# quantifies whether high-degree nodes tend to attach to other high-degree nodes (assortative mixing) or
# whether high-degree nodes rather attach to low-degree ones (disassortative mixing).
assortativity_degree(g2, directed=T)



