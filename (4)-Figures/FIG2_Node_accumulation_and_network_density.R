# -------------------------------------------------------------------------
# ------------------------- Network growth metrics ------------------------
# -------------------------------------------------------------------------
# Set the working directory ---------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Chapters/(3) Dispersal Limitation/Data/Data/igraph environments")

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

# Reviewer's comments - Node accumulation ---------------------------------

# Particles were originally released from 32 nodes, so why does this line graph 
# start with 104? If 72 sites were colonised in the first year, this means the 
# range expansion was much greater than implied by this figure (which makes it 
# look like only 68 sites were colonised in total).

load("./Combined/Combined_2000.RData")
n_2000 <- connectivity$Release_poly
n_2000 <- n_2000[!duplicated(n_2000)]
node[1,2] <- length(n_2000)

load("./Combined/Combined_2001.RData")
n_2001 <- connectivity$Release_poly
n_2001 <- n_2001[!duplicated(n_2001)]
node[2,2] <- length(n_2001)

load("./Combined/Combined_2002.RData")
n_2002 <- connectivity$Release_poly
n_2002 <- n_2002[!duplicated(n_2002)]
node[3,2] <- length(n_2002)

load("./Combined/Combined_2003.RData")
n_2003 <- connectivity$Release_poly
n_2003 <- n_2003[!duplicated(n_2003)]
node[4,2] <- length(n_2003)

load("./Combined/Combined_2004.RData")
n_2004 <- connectivity$Release_poly
n_2004 <- n_2004[!duplicated(n_2004)]
node[5,2] <- length(n_2004)

load("./Combined/Combined_2005.RData")
n_2005 <- connectivity$Release_poly
n_2005 <- n_2005[!duplicated(n_2005)]
node[6,2] <- length(n_2005)

load("./Combined/Combined_2006.RData")
n_2006 <- connectivity$Release_poly
n_2006 <- n_2006[!duplicated(n_2006)]
node[7,2] <- length(n_2006)

load("./Combined/Combined_2007.RData")
n_2007 <- connectivity$Release_poly
n_2007 <- n_2007[!duplicated(n_2007)]
node[8,2] <- length(n_2007)

load("./Combined/Combined_2008.RData")
n_2008 <- connectivity$Release_poly
n_2008 <- n_2008[!duplicated(n_2008)]
node[9,2] <- length(n_2008)

load("./Combined/Combined_2009.RData")
n_2009 <- connectivity$Release_poly
n_2009 <- n_2009[!duplicated(n_2009)]
node[10,2] <- length(n_2009)

load("./Combined/Combined_2010.RData")
n_2010 <- connectivity$Release_poly
n_2010 <- n_2010[!duplicated(n_2010)]
node[11,2] <- length(n_2010)

load("./Combined/Combined_2011.RData")
n_2011 <- connectivity$Release_poly
n_2011 <- n_2011[!duplicated(n_2011)]
node[12,2] <- length(n_2011)

load("./Combined/Combined_2012.RData")
n_2012 <- connectivity$Release_poly
n_2012 <- n_2012[!duplicated(n_2012)]
node[13,2] <- length(n_2012)

# Plot --------------------------------------------------------------------

node$Year <- as.character(node$Year)

ggplot()+
  geom_line(node, mapping=aes(x=Year, y=Node, group=1), colour="black", size=1)+
  geom_hline(yintercept=180, linetype="dashed", colour="red", size=1)+
  ylab("Number of nodes")+
  scale_y_continuous(limits=c(20, 180), breaks = seq(20, 180, by = 40))+
  cc_theme()

dens$Year <- as.character(dens$Year)

ggplot()+
  geom_line(dens, mapping=aes(x=Year, y=Density, group=1), colour="black", size=1)+
  ylab("Network density")+
  ylim(c(0, 0.06))+
  cc_theme()
