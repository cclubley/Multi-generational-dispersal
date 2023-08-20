# -------------------------------------------------------------------------
# ------------------------- Network growth metrics ------------------------
# -------------------------------------------------------------------------
# Set the working directory ---------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/Data/Data/igraph environments")

# Load the required packages --------------------------------------------------
{ library(dplyr)
  library(reshape2)
  library(igraph)
  library(rworldxtra)
  library(raster)
  library(maps)
  library(ggplot2)
  library(ccplot)
}

# Set up empty data frames -----------------------------------------------

# Node accumulation
node <- as.data.frame(2000:2012)
colnames(node) <- "Year"
node$Node <- NA

# Graph density
dens <- as.data.frame(2000:2012)
colnames(dens) <- "Year"
dens$Density <- NA

# Load the igraph environments and save the number of nodes ---------------

# Load the data
load("./Combined/Combined_2000.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
node[1,2] <- length(V(g))
# Retrieve the network density
dens[1,2] <- ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation 

load("./Combined/Combined_2001.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[2,2] <- length(V(g))
dens[2,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2002.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[3,2] <- length(V(g))
dens[3,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2003.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[4,2] <- length(V(g))
dens[4,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2004.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[5,2] <- length(V(g))
dens[5,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2005.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[6,2] <- length(V(g))
dens[6,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2006.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[7,2] <- length(V(g))
dens[7,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2007.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[8,2] <- length(V(g))
dens[8,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2008.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[9,2] <- length(V(g))
dens[9,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2009.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[10,2] <- length(V(g))
dens[10,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2010.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[11,2] <- length(V(g))
dens[11,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2011.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[12,2] <- length(V(g))
dens[12,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2012.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
node[13,2] <- length(V(g))
dens[13,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

# Plot --------------------------------------------------------------------

node$Year <- as.character(node$Year)

ggplot()+
  geom_line(node, mapping=aes(x=Year, y=Node, group=1), colour="black", size=1)+
  geom_hline(yintercept=180, linetype="dashed", colour="red", size=1)+
  ylab("Number of nodes")+
  ylim(c(100, 180))+
  cc_theme()

dens$Year <- as.character(dens$Year)

ggplot()+
  geom_line(dens, mapping=aes(x=Year, y=Density, group=1), colour="black", size=1)+
  ylab("Network density")+
  ylim(c(0, 0.06))+
  cc_theme()
