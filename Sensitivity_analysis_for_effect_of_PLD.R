# The following code conducts a sensitivity analysis for the effects of 
# planktonic larval duration (PLD) on connectivity metrics

{ library(dplyr)
  library(reshape2)
  library(igraph)
  library(rworldxtra)
  library(raster)
  library(maps)
  library(sf)
}

# Year 2000, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2000.RData")

# List the unwanted polygons - outside of study area
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2000 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2000) <- "dist_14_2000"

# Create graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2000, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2000.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2000 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2000) <- "dist_17_2000"

# Create graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2000, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2000.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2000 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2000) <- "dist_25_2000"

# Create graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2000, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2000.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2000 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2000) <- "dist_28_2000"

# Create graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2001, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2001.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2001 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2001) <- "dist_14_2001"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2001, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2001.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2001 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2001) <- "dist_17_2001"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2001, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2001.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402" "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2001 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2001) <- "dist_25_2001"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2001, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2001.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2001 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2001) <- "dist_28_2001"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2002, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2002.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2002 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2002) <- "dist_14_2002"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2002, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2002.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2002 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2002) <- "dist_17_2002"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2002, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2002.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2002 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2002) <- "dist_25_2002"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2002, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2002.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2002 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2002) <- "dist_28_2002"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2003, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2003.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2003 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2003) <- "dist_14_2003"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2003, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2003.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2003 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2003) <- "dist_17_2003"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2003, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2003.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2003 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2003) <- "dist_25_2003"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2003, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2003.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2003 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2003) <- "dist_28_2003"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2004, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2004.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2004 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2004) <- "dist_14_2004"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2004, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2004.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2004 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2004) <- "dist_17_2004"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2004, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2004.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2004 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2004) <- "dist_25_2004"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2004, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2004.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2004 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2004) <- "dist_28_2004"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2005, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2005.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2005 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2005) <- "dist_14_2005"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2005, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2005.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2005 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2005) <- "dist_17_2005"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2005, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2005.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2005 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2005) <- "dist_25_2005"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2005, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2005.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2005 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2005) <- "dist_28_2005"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2006, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2006.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2006 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2006) <- "dist_14_2006"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2006, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2006.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2006 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2006) <- "dist_17_2006"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2006, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2006.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2006 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2006) <- "dist_25_2006"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2006, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2006.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2006 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2006) <- "dist_28_2006"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2007, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2007.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2007 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2007) <- "dist_14_2007"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2007, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2007.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2007 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2007) <- "dist_17_2007"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2007, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2007.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2007 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2007) <- "dist_25_2007"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2007, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2007.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2007 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2007) <- "dist_28_2007"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2008, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2008.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2008 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2008) <- "dist_14_2008"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2008, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2008.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2008 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2008) <- "dist_17_2008"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2008, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2008.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2008 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2008) <- "dist_25_2008"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2008, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2008.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2008 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2008) <- "dist_28_2008"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2009, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2009.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2009 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2009) <- "dist_14_2009"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2009, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2009.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2009 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2009) <- "dist_17_2009"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2009, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2009.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2009 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2009) <- "dist_25_2009"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2009, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2009.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2009 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2009) <- "dist_28_2009"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2010, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2010.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2010 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2010) <- "dist_14_2010"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2010, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2010.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2010 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2010) <- "dist_17_2010"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2010, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2010.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2010 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2010) <- "dist_25_2010"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2010, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2010.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2010 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2010) <- "dist_28_2010"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2011, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2011.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2011 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2011) <- "dist_14_2011"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, g, g2, ceb)

# Year 2011, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2011.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2011 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2011) <- "dist_17_2011"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2011, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2011.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2011 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2011) <- "dist_25_2011"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, g, g2, ceb)

# Year 2011, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2011.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2011 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2011) <- "dist_28_2011"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, g, g2)

# Year 2012, PLD 14 -------------------------------------------------------

load("./14 PLD/PLD14_2012.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_14_2012 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_14_2012) <- "dist_14_2012"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, ceb, g, g2)

# Year 2012, PLD 17 -------------------------------------------------------

load("./17 PLD/PLD17_2012.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_17_2012 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_17_2012) <- "dist_17_2012"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, coords_14, ceb, g, g2)

# Year 2012, PLD 25 -------------------------------------------------------

load("./25 PLD/PLD25_2012.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_25_2012 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_25_2012) <- "dist_25_2012"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords, ceb, coords_14, coords_17, g, g2)

# Year 2012, PLD 28 -------------------------------------------------------

load("./28 PLD/PLD28_2012.RData")

# List the unwanted polygons
polygons_to_remove <- c("344", "364", "365", "366", "367", "368", "369", "392", "398", "410",
                        "411", "444", "445", "447", "457", "492", "495", "499", "500", "501",
                        "504", "505", "541", "542", "543", "546", "549", "552", "590", "591", 
                        "592", "593", "597", "601", "635", "636", "637", "643", "644", "649",
                        "683", "684", "692", "697", "698", "731", "732", "740", "779", "780", 
                        "788", "789", "826", "827", "921", "922", "969", "1018", "1019", "1020",
                        "1021", "1022", "1069", "1118", "1119", "1125", "1215", "1216", "1308", 
                        "1309", "1310", "1311", "1312", "1313", "1314", "1354", "1355", "1402", "1403", "1404")

# Remove from spawning polygons
connectivity <- connectivity[!(connectivity$Release_poly %in% polygons_to_remove), ]

# Remove from settlement polygons
connectivity <- connectivity[!(connectivity$Settlement_poly %in% polygons_to_remove), ]

# Remove from centroids
centroids$ID <- row.names(centroids)
centroids <- centroids[!(centroids$ID %in% polygons_to_remove), ]
centroids$ID <- NULL
centroids <- na.omit(centroids)

# Calculate particle distances
sf_source <- st_as_sf(connectivity, coords = c("Source_lon","Source_lat"), crs=4326)
sf_sink <- st_as_sf(connectivity, coords = c("Sink_lon","Sink_lat"), crs=4326)
distances_28_2012 <- as.data.frame(st_distance(sf_source, sf_sink, by_element = TRUE))
colnames(distances_28_2012) <- "dist_28_2012"

g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
length(V(g))
# Retrieve the network density
ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)
# How many communities are there?
length(ceb)

rm(centroids, connectivity, g_14, g_17, g_25, myFreqs, s.dat, sf_sink, sf_source, polygons_to_remove, coords)

# KS tests ----------------------------------------------------------------

# Carry out the Kolmogorov-Smirnov tests
distances_14_2000$dist_14_2000 <- as.numeric(distances_14_2000$dist_14_2000)
distances_17_2000$dist_17_2000 <- as.numeric(distances_17_2000$dist_17_2000)
distances_25_2000$dist_25_2000 <- as.numeric(distances_25_2000$dist_25_2000)
distances_28_2000$dist_28_2000 <- as.numeric(distances_28_2000$dist_28_2000)
distances_combo_2000$dist_combo_2000 <- as.numeric(distances_combo_2000$dist_combo_2000)

print("14 vs 17")
ks.test(distances_14_2000$dist_14_2000, distances_17_2000$dist_17_2000)
# D = 0.078984
# p < 2.2e-16

print("14 vs 25")
ks.test(distances_14_2000$dist_14_2000, distances_25_2000$dist_25_2000)
# D = 0.20974
# p < 2.2e-16

print("14 vs 28")
ks.test(distances_14_2000$dist_14_2000, distances_28_2000$dist_28_2000)
# D = 0.23649
# p < 2.2e-16

print("17 vs 25")
ks.test(distances_17_2000$dist_17_2000, distances_25_2000$dist_25_2000)
# D = 0.13328
# p < 2.2e-16

print("17 vs 28")
ks.test(distances_17_2000$dist_17_2000, distances_28_2000$dist_28_2000)
# D = 0.16061
# p < 2.2e-16

print("25 vs 28")
ks.test(distances_25_2000$dist_25_2000, distances_28_2000$dist_28_2000)
# D = 0.027803
# p < 2.2e-16

# Plot --------------------------------------------------------------------

pld <- read.csv("./PLD_sensitivity.csv")

library(ggplot2)
library(ccplot)

pld$PLD <- as.character(pld$PLD)
pld$Year <- as.character(pld$Year)

ggplot()+
  geom_line(pld, mapping=aes(x=Year, y=Nodes, group=PLD, col=PLD), size=1)+
  ylab("Number of nodes")+
  scale_y_continuous(limits=c(20, 180), breaks = seq(20, 180, by = 40))+
  cc_theme()

ggplot()+
  geom_line(pld, mapping=aes(x=Year, y=Density, group=PLD, col=PLD), size=1)+
  ylab("Network density")+
  ylim(c(0, 0.06))+
  cc_theme()
