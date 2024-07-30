# The following code was used to create the arbitrary network in table 1, which 
# defines the different graph network metrics used in the manuscript.

# Load the required libraries
library(igraph)
library(dplyr)

# Construct a hypothetical graph
g1 <- graph(edges=c(1,2, 2,3, 3,2, 4,2, 4,3, 4,5, 4,6, 5,4, 5,6, 6,5, 6,4), n=6, directed=T) 
E(g1)$weight <- c(10, 2, 2, 1, 5, 7, 12, 4, 5, 7, 9)
E(g1)$width <- E(g1)$weight/2
  
# Plot to check
plot(g1, edge.arrow.size=1, vertex.color="grey", vertex.size=15, 
     vertex.frame.color="black", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=2, edge.color="black", edge.curved=0.3)

# Number of nodes
V(g1)
# 6

# Network density
ecount(g1)/(vcount(g1)*(vcount(g1)-1)) # Directed network calculation 
# 0.3666667

# Out-degree and In-degree
deg <- degree(g1, mode="out")
deg
# node 4 = 4
deg <- degree(g1, mode="in")
deg
# node 4 = 2

# Number of clusters
g2 <- as.undirected(g1)
ceb <- cluster_fast_greedy(g2, weights=NULL)
plot(ceb, g2, vertex.color="grey", vertex.size=15, 
     vertex.frame.color="black", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=2, edge.color="black", edge.curved=0.3)
length(ceb)
# 2

# Modularity
modularity(ceb)
# 0.30

# Cluster permeability
# Node 4 is the only bridge
# The two links it has with the other community have a value of 1 and 5
# Total number of particles exchanged is equal to 64
sum(E(g1)$weight)
# So to calculate permeability
(6/64)*100
# Bridges between communities account for 9.4% of all particle movement

