{ library(reshape2)
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
  library(corrplot)
  library(sp)
  library(dplyr)
  library(dendextend)
}
# -------------------------------------------------------------------------
# ------------------------------ Figure 2 ---------------------------------
# -------------------------------------------------------------------------
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
load("./Combined/Combined_2000_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Retrieve the number of nodes
n_2000 <- filter(myFreqs, !Count==0)
n_2000 <- n_2000$Release_poly
n_2000 <- n_2000[!duplicated(n_2000)]
node[1,2] <- length(n_2000)
# Retrieve the network density
dens[1,2] <- ecount(g)/(vcount(g)*(vcount(g)-1)) # Directed network calculation 

load("./Combined/Combined_2001_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
n_2001 <- filter(myFreqs, !Count==0)
n_2001 <- n_2001$Release_poly
n_2001 <- n_2001[!duplicated(n_2001)]
node[2,2] <- length(n_2001)
dens[2,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2002_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
n_2002 <- filter(myFreqs, !Count==0)
n_2002 <- n_2002$Release_poly
n_2002 <- n_2002[!duplicated(n_2002)]
node[3,2] <- length(n_2002)
dens[3,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2003_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
n_2003 <- filter(myFreqs, !Count==0)
n_2003 <- n_2003$Release_poly
n_2003 <- n_2003[!duplicated(n_2003)]
node[4,2] <- length(n_2003)
dens[4,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2004_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
n_2004 <- filter(myFreqs, !Count==0)
n_2004 <- n_2004$Release_poly
n_2004 <- n_2004[!duplicated(n_2004)]
node[5,2] <- length(n_2004)
dens[5,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2005_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
n_2005 <- filter(myFreqs, !Count==0)
n_2005 <- n_2005$Release_poly
n_2005 <- n_2005[!duplicated(n_2005)]
node[6,2] <- length(n_2005)
dens[6,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2006_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
n_2006 <- filter(myFreqs, !Count==0)
n_2006 <- n_2006$Release_poly
n_2006 <- n_2006[!duplicated(n_2006)]
node[7,2] <- length(n_2006)
dens[7,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2007_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
n_2007 <- filter(myFreqs, !Count==0)
n_2007 <- n_2007$Release_poly
n_2007 <- n_2007[!duplicated(n_2007)]
node[8,2] <- length(n_2007)
dens[8,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2008_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
n_2008 <- filter(myFreqs, !Count==0)
n_2008 <- n_2008$Release_poly
n_2008 <- n_2008[!duplicated(n_2008)]
node[9,2] <- length(n_2008)
dens[9,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2009_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
n_2009 <- filter(myFreqs, !Count==0)
n_2009 <- n_2009$Release_poly
n_2009 <- n_2009[!duplicated(n_2009)]
node[10,2] <- length(n_2009)
dens[10,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2010_new.RData")
s.dat <- s.dat[-c(171), -c(171)]
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
myFreqs <- filter(myFreqs, !Settlement_poly=="1402")
myFreqs <- filter(myFreqs, !Release_poly=="1402")
n_2010 <- filter(myFreqs, !Count==0)
n_2010 <- n_2010$Release_poly
n_2010 <- n_2010[!duplicated(n_2010)]
node[11,2] <- length(n_2010)
dens[11,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2011_new.RData")
s.dat <- s.dat[-c(171), -c(171)]
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
myFreqs <- filter(myFreqs, !Settlement_poly=="1402")
myFreqs <- filter(myFreqs, !Release_poly=="1402")
n_2011 <- filter(myFreqs, !Count==0)
n_2011 <- n_2011$Release_poly
n_2011 <- n_2011[!duplicated(n_2011)]
node[12,2] <- length(n_2011)
dens[12,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

load("./Combined/Combined_2012_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
n_2012 <- filter(myFreqs, !Count==0)
n_2012 <- n_2012$Release_poly
n_2012 <- n_2012[!duplicated(n_2012)]
node[13,2] <- length(n_2012)
dens[13,2] <-  ecount(g)/(vcount(g)*(vcount(g)-1))

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

# -------------------------------------------------------------------------
# ------------------------------ Figure 6 ---------------------------------
# -------------------------------------------------------------------------
# Calculate in and out degree ---------------------------------------------

# Load the data
load("./Combined/Combined_2000_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2000 <- as.data.frame(deg_out)
out_deg_2000 <- filter(out_deg_2000, deg_out > 0) 
colnames(out_deg_2000) <- "out_2000"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2000 <- as.data.frame(deg_in)
colnames(in_deg_2000) <- "in_2000"

# Load the data
load("./Combined/Combined_2001_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2001 <- as.data.frame(deg_out)
out_deg_2001 <- filter(out_deg_2001, deg_out > 0) 
colnames(out_deg_2001) <- "out_2001"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2001 <- as.data.frame(deg_in)
colnames(in_deg_2001) <- "in_2001"

# Load the data
load("./Combined/Combined_2002_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2002 <- as.data.frame(deg_out)
out_deg_2002 <- filter(out_deg_2002, deg_out > 0) 
colnames(out_deg_2002) <- "out_2002"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2002 <- as.data.frame(deg_in)
colnames(in_deg_2002) <- "in_2002"

# Load the data
load("./Combined/Combined_2003_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2003 <- as.data.frame(deg_out)
out_deg_2003 <- filter(out_deg_2003, deg_out > 0) 
colnames(out_deg_2003) <- "out_2003"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2003 <- as.data.frame(deg_in)
colnames(in_deg_2003) <- "in_2003"

# Load the data
load("./Combined/Combined_2004_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2004 <- as.data.frame(deg_out)
out_deg_2004 <- filter(out_deg_2004, deg_out > 0) 
colnames(out_deg_2004) <- "out_2004"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2004 <- as.data.frame(deg_in)
colnames(in_deg_2004) <- "in_2004"

# Load the data
load("./Combined/Combined_2005_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2005 <- as.data.frame(deg_out)
out_deg_2005 <- filter(out_deg_2005, deg_out > 0) 
colnames(out_deg_2005) <- "out_2005"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2005 <- as.data.frame(deg_in)
colnames(in_deg_2005) <- "in_2005"

# Load the data
load("./Combined/Combined_2006_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2006 <- as.data.frame(deg_out)
out_deg_2006 <- filter(out_deg_2006, deg_out > 0) 
colnames(out_deg_2006) <- "out_2006"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2006 <- as.data.frame(deg_in)
colnames(in_deg_2006) <- "in_2006"

# Load the data
load("./Combined/Combined_2007_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2007 <- as.data.frame(deg_out)
out_deg_2007 <- filter(out_deg_2007, deg_out > 0) 
colnames(out_deg_2007) <- "out_2007"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2007 <- as.data.frame(deg_in)
colnames(in_deg_2007) <- "in_2007"

# Load the data
load("./Combined/Combined_2008_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2008 <- as.data.frame(deg_out)
out_deg_2008 <- filter(out_deg_2008, deg_out > 0) 
colnames(out_deg_2008) <- "out_2008"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2008 <- as.data.frame(deg_in)
colnames(in_deg_2008) <- "in_2008"

# Load the data
load("./Combined/Combined_2009_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2009 <- as.data.frame(deg_out)
out_deg_2009 <- filter(out_deg_2009, deg_out > 0) 
colnames(out_deg_2009) <- "out_2009"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2009 <- as.data.frame(deg_in)
colnames(in_deg_2009) <- "in_2009"

# Load the data
load("./Combined/Combined_2010_new.RData")
# Create a graph
s.dat <- s.dat[-c(171), -c(171)]
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2010 <- as.data.frame(deg_out)
out_deg_2010 <- filter(out_deg_2010, deg_out > 0) 
colnames(out_deg_2010) <- "out_2010"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2010 <- as.data.frame(deg_in)
colnames(in_deg_2010) <- "in_2010"

# Load the data
load("./Combined/Combined_2011_new.RData")
# Create a graph
s.dat <- s.dat[-c(171), -c(171)]
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2011 <- as.data.frame(deg_out)
out_deg_2011 <- filter(out_deg_2011, deg_out > 0) 
colnames(out_deg_2011) <- "out_2011"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2011 <- as.data.frame(deg_in)
colnames(in_deg_2011) <- "in_2011"

# Load the data
load("./Combined/Combined_2012_new.RData")
# Create a graph
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
# Out degree
deg_out <- degree(g, v=V(g), mode="out")
out_deg_2012 <- as.data.frame(deg_out)
out_deg_2012 <- filter(out_deg_2012, deg_out > 0) 
colnames(out_deg_2012) <- "out_2012"
# In degree
deg_in <- degree(g, v=V(g), mode="in")
in_deg_2012 <- as.data.frame(deg_in)
colnames(in_deg_2012) <- "in_2012"

# Merge together ----------------------------------------------------------

{out_deg_2000$Node <- rownames(out_deg_2000)
out_deg_2001$Node <- rownames(out_deg_2001)
out_deg_2002$Node <- rownames(out_deg_2002)
out_deg_2003$Node <- rownames(out_deg_2003)
out_deg_2004$Node <- rownames(out_deg_2004)
out_deg_2005$Node <- rownames(out_deg_2005)
out_deg_2006$Node <- rownames(out_deg_2006)
out_deg_2007$Node <- rownames(out_deg_2007)
out_deg_2008$Node <- rownames(out_deg_2008)
out_deg_2009$Node <- rownames(out_deg_2009)
out_deg_2010$Node <- rownames(out_deg_2010)
out_deg_2011$Node <- rownames(out_deg_2011)
out_deg_2012$Node <- rownames(out_deg_2012)
}
{out_deg <- merge(out_deg_2000, out_deg_2001, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2002, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2003, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2004, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2005, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2006, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2007, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2008, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2009, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2010, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2011, by="Node", all=TRUE)
  out_deg <- merge(out_deg, out_deg_2012, by="Node", all=TRUE)
}

{in_deg_2000$Node <- rownames(in_deg_2000)
  in_deg_2001$Node <- rownames(in_deg_2001)
  in_deg_2002$Node <- rownames(in_deg_2002)
  in_deg_2003$Node <- rownames(in_deg_2003)
  in_deg_2004$Node <- rownames(in_deg_2004)
  in_deg_2005$Node <- rownames(in_deg_2005)
  in_deg_2006$Node <- rownames(in_deg_2006)
  in_deg_2007$Node <- rownames(in_deg_2007)
  in_deg_2008$Node <- rownames(in_deg_2008)
  in_deg_2009$Node <- rownames(in_deg_2009)
  in_deg_2010$Node <- rownames(in_deg_2010)
  in_deg_2011$Node <- rownames(in_deg_2011)
  in_deg_2012$Node <- rownames(in_deg_2012)
}
{in_deg <- merge(in_deg_2000, in_deg_2001, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2002, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2003, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2004, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2005, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2006, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2007, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2008, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2009, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2010, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2011, by="Node", all=TRUE)
  in_deg <- merge(in_deg, in_deg_2012, by="Node", all=TRUE)
}

# Merge together
deg <- merge(out_deg, in_deg, by=c("Node"), all=TRUE)
# Remove any rows where both columns are NA
deg <- deg[rowSums(is.na(deg)) != ncol(deg), ]
# Replace any remaining NA values with 0
deg[is.na(deg)] <- 0

# Create a correlation matrix ---------------------------------------------

M <- cor(deg[, 2:27])
M[M < 0] <- 0

# Carry out a significance test for correlations
testRes <- cor.mtest(deg[, 2:27], conf.level=0.95)

# Plot with perceptually uniform colour palette
cols <- magma(200)
cols <- cols[200:100]

corrplot(M, p.mat=testRes$p, method="color", type="lower", addCoef.col="black",
         number.cex=0.6, diag=FALSE, tl.col="black", insig="blank", cl.ratio=0.2, tl.srt=45, sig.level=0.05,
         col.lim=c(0, 1), col=cols, is.corr=FALSE)

# -------------------------------------------------------------------------
# ------------------------------ Figure 7 ---------------------------------
# -------------------------------------------------------------------------
# Load the data -----------------------------------------------------------

# Load the data
{load("./Combined/Combined_2000_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2000 <- as.undirected(g)

load("./Combined/Combined_2001_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2001 <- as.undirected(g)

load("./Combined/Combined_2002_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2002 <- as.undirected(g)

load("./Combined/Combined_2003_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2003 <- as.undirected(g)

load("./Combined/Combined_2004_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2004 <- as.undirected(g)

load("./Combined/Combined_2005_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2005 <- as.undirected(g)

load("./Combined/Combined_2006_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2006 <- as.undirected(g)

load("./Combined/Combined_2007_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2007 <- as.undirected(g)

load("./Combined/Combined_2008_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2008 <- as.undirected(g)

load("./Combined/Combined_2009_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2009 <- as.undirected(g)

load("./Combined/Combined_2010_new.RData")
s.dat <- s.dat[-c(171), -c(171)]
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2010 <- as.undirected(g)

load("./Combined/Combined_2011_new.RData")
s.dat <- s.dat[-c(171), -c(171)]
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2011 <- as.undirected(g)

load("./Combined/Combined_2012_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2012 <- as.undirected(g)
}

# Edit graphs to all have the same number of nodes ------------------------

# 2000 - 2003 have fewer nodes than all other graphs
# Separate the nodes from each graph for 2000 - 2004
# Using the graph from 2004 to identify the missing nodes
{v2000 <- cbind(igraph::as_data_frame(g2000, "vertices"))
v2000$Y_2000 <- "2000"
v2001 <- cbind(igraph::as_data_frame(g2001, "vertices"))
v2001$Y_2001 <- "2001"
v2002 <- cbind(igraph::as_data_frame(g2002, "vertices"))
v2002$Y_2002 <- "2002"
v2003 <- cbind(igraph::as_data_frame(g2003, "vertices"))
v2003$Y_2003 <- "2003"
v2004 <- cbind(igraph::as_data_frame(g2004, "vertices"))
v2004$Y_2004 <- "2004"
}

# Merge them into one data frame, where if a node is not present in one year it 
# is filled with NA
{nodes <- merge(v2000, v2001, by="name", all=TRUE)
nodes <- merge(nodes, v2002, by="name", all=TRUE)
nodes <- merge(nodes, v2003, by="name", all=TRUE)
nodes <- merge(nodes, v2004, by="name", all=TRUE)
}

# Add the missing nodes to each year
v2000 <- as.data.frame(nodes[, c(1,2)]) # Get the data for that year
colnames(v2000) <- c("name", "year") # Re-name the columns
v2000na <- v2000[rowSums(is.na(v2000))>0,] # Separate rows = NA
v2000na$year <- NULL # Remove the lat column
rownames(v2000na) <- v2000na$name # Assign the row names
g2000 <- add_vertices(g2000, 67, name=v2000na$name) # Add as vertices to the igraph for that year

# Do the same for 2001-2003
{v2001 <- as.data.frame(nodes[, c(1,3)])
  colnames(v2001) <- c("name", "year")
  v2001na <- v2001[rowSums(is.na(v2001))>0,]
  v2001na$year <- NULL
  rownames(v2001na) <- v2001na$name
  g2001 <- add_vertices(g2001, 38, name=v2001na$name)
  
  v2002 <- as.data.frame(nodes[, c(1,4)])
  colnames(v2002) <- c("name", "year")
  v2002na <- v2002[rowSums(is.na(v2002))>0,]
  v2002na$year <- NULL
  rownames(v2002na) <- v2002na$name
  g2002 <- add_vertices(g2002, 14, name=v2002na$name)
  
  v2003 <- as.data.frame(nodes[, c(1,5)])
  colnames(v2003) <- c("name", "year")
  v2003na <- v2003[rowSums(is.na(v2003))>0,]
  v2003na$year <- NULL
  rownames(v2003na) <- v2003na$name
  g2003 <- add_vertices(g2003, 1, name=v2003na$name)
}

# Community identification ------------------------------------------------

# Identify communities
{ ceb2000 <- cluster_fast_greedy(g2000)
ceb2001 <- cluster_fast_greedy(g2001)
ceb2002 <- cluster_fast_greedy(g2002)
ceb2003 <- cluster_fast_greedy(g2003)
ceb2004 <- cluster_fast_greedy(g2004)
ceb2005 <- cluster_fast_greedy(g2005)
ceb2006 <- cluster_fast_greedy(g2006)
ceb2007 <- cluster_fast_greedy(g2007)
ceb2008 <- cluster_fast_greedy(g2008)
ceb2009 <- cluster_fast_greedy(g2009)
ceb2010 <- cluster_fast_greedy(g2010)
ceb2011 <- cluster_fast_greedy(g2011)
ceb2012 <- cluster_fast_greedy(g2012)
}

# Convert to dendrogram ---------------------------------------------------

# Create dendrogram
{ceb2000h <- as.dendrogram(ceb2000)
ceb2000h <- as_hclust_fixed(ceb2000h)
ceb2001h <- as.dendrogram(ceb2001)
ceb2001h <- as_hclust_fixed(ceb2001h)
ceb2002h <- as.dendrogram(ceb2002)
ceb2002h <- as_hclust_fixed(ceb2002h)
ceb2003h <- as.dendrogram(ceb2003)
ceb2003h <- as_hclust_fixed(ceb2003h)
ceb2004h <- as.dendrogram(ceb2004)
ceb2004h <- as_hclust_fixed(ceb2004h)
ceb2005h <- as.dendrogram(ceb2005)
ceb2005h <- as_hclust_fixed(ceb2005h)
ceb2006h <- as.dendrogram(ceb2006)
ceb2006h <- as_hclust_fixed(ceb2006h)
ceb2007h <- as.dendrogram(ceb2007)
ceb2007h <- as_hclust_fixed(ceb2007h)
ceb2008h <- as.dendrogram(ceb2008)
ceb2008h <- as_hclust_fixed(ceb2008h)
ceb2009h <- as.dendrogram(ceb2009)
ceb2009h <- as_hclust_fixed(ceb2009h)
ceb2010h <- as.dendrogram(ceb2010)
ceb2010h <- as_hclust_fixed(ceb2010h)
ceb2011h <- as.dendrogram(ceb2011)
ceb2011h <- as_hclust_fixed(ceb2011h)
ceb2012h <- as.dendrogram(ceb2012)
ceb2012h <- as_hclust_fixed(ceb2012h)
}

# Add community membership details
{ member <- membership(ceb2000)
  ceb2000h$membership <- as.character(member)
  member <- membership(ceb2001)
  ceb2001h$membership <- as.character(member)
  member <- membership(ceb2002)
  ceb2002h$membership <- as.character(member)
  member <- membership(ceb2003)
  ceb2003h$membership <- as.character(member)
  member <- membership(ceb2004)
  ceb2004h$membership <- as.character(member)
  member <- membership(ceb2005)
  ceb2005h$membership <- as.character(member)
  member <- membership(ceb2006)
  ceb2006h$membership <- as.character(member)
  member <- membership(ceb2007)
  ceb2007h$membership <- as.character(member)
  member <- membership(ceb2008)
  ceb2008h$membership <- as.character(member)
  member <- membership(ceb2009)
  ceb2009h$membership <- as.character(member)
  member <- membership(ceb2010)
  ceb2010h$membership <- as.character(member)
  member <- membership(ceb2011)
  ceb2011h$membership <- as.character(member)
  member <- membership(ceb2012)
  ceb2012h$membership <- as.character(member)
}

# Return to dendrogram object
{dend0 <- as.dendrogram(ceb2000h)
  dend1 <- as.dendrogram(ceb2001h)
  dend2 <- as.dendrogram(ceb2002h)
  dend3 <- as.dendrogram(ceb2003h)
  dend4 <- as.dendrogram(ceb2004h)
  dend5 <- as.dendrogram(ceb2005h)
  dend6 <- as.dendrogram(ceb2006h)
  dend7 <- as.dendrogram(ceb2007h)
  dend8 <- as.dendrogram(ceb2008h)
  dend9 <- as.dendrogram(ceb2009h)
  dend10 <- as.dendrogram(ceb2010h)
  dend11 <- as.dendrogram(ceb2011h)
  dend12 <- as.dendrogram(ceb2012h)
}

# Set branch colours to communities permanently
{ dend0 <- dend0 %>%
    set("branches_k_color", k=length(ceb2000)) 
  dend1 <- dend1 %>%
    set("branches_k_color", k=length(ceb2001))
  dend2 <- dend2 %>%
    set("branches_k_color", k=length(ceb2002)) 
  dend3 <- dend3 %>%
    set("branches_k_color", k=length(ceb2003))
  dend4 <- dend4 %>%
    set("branches_k_color", k=length(ceb2004)) 
  dend5 <- dend5 %>%
    set("branches_k_color", k=length(ceb2005))
  dend6 <- dend6 %>%
    set("branches_k_color", k=length(ceb2006)) 
  dend7 <- dend7 %>%
    set("branches_k_color", k=length(ceb2007))
  dend8 <- dend8 %>%
    set("branches_k_color", k=length(ceb2008)) 
  dend9 <- dend9 %>%
    set("branches_k_color", k=length(ceb2009))
  dend10 <- dend10 %>%
    set("branches_k_color", k=length(ceb2010)) 
  dend11 <- dend11 %>%
    set("branches_k_color", k=length(ceb2011))
  dend12 <- dend12 %>%
    set("branches_k_color", k=length(ceb2012))
}

# Create a dendrogram list ------------------------------------------------

# Add attributes to the trees
{ dend0 <- dend0 %>%  set("labels_colors") %>%  # Changes node labels to same colour as branch
  set("labels_cex", c(.5)) # Changes font size of node labels
dend1 <- dend1 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend2 <- dend2 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend3 <- dend3 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend4 <- dend4 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend5 <- dend5 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend6 <- dend6 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend7 <- dend7 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend8 <- dend8 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend9 <- dend9 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend10 <- dend10 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend11 <- dend11 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
dend12 <- dend12 %>%  set("labels_colors") %>%  
  set("labels_cex", c(.5))
}

# Create a dendrogram list
dl <- dendlist(dend0, dend1, dend2, dend3, dend4, dend5, dend6, dend7, dend8, dend9,
               dend10, dend11, dend12)

# Baker's gamma index -----------------------------------------------------

# Baker's gamma index is a measure of similarity between two dendrograms. It is
# defined as the rank correlation between the stages at which pairs of objects 
# combine in each of the two trees.

# It is calculated by taking two nodes and finding the highest possible level of 
# k (number of cluster groups created when cutting the tree) for which the two nodes
# still belong to the same subtree. That k is returned, and the same is done for the
# same two nodes in the second tree. This is repeated for all pairs of nodes, and
# then the two sets of numbers (one for each tree) are paired according to the 
# pairs of nodes and a Spearman correlation is calculated. 

# The value can range between -1 and 1, with near 0 values meaning that the two trees 
# are not statistically similar. For the exact p value, a permutation test would
# need to be used.

# The measure is not affected by the height of a branch, but rather by its relative 
# position compared with other branches.

# Cophenetic correlation matrix
cors <- cor.dendlist(dl, method="baker")
cors
min(cors)

# Least correlated = 2000 and 2008
tanglegram(dend0, dend8, sort=TRUE, common_subtrees_color_lines=TRUE, 
           highlight_distinct_edges=TRUE, highlight_branches_lwd=FALSE, lwd=2)
# Most correlated = 2009 and 2010
tanglegram(dend9, dend10, sort=TRUE, common_subtrees_color_lines=TRUE, 
           highlight_distinct_edges=TRUE, highlight_branches_lwd=FALSE, lwd=2)

# You can then visualise using corrplot
corrplot(cors, method="color", type="lower", addCoef.col="black",
         number.cex=0.6, diag=FALSE, tl.col="black", cl.ratio=0.2, tl.srt=45,
         col.lim=c(0, 1), col=colorRampPalette(c("white", "#54b3e7ff", "white", "#f3202cff"))(200))

# Perceptually uniform colour palette
cols <- magma(200)
cols <- cols[200:100]

corrplot(cors, method="color", type="lower", addCoef.col="black",
         number.cex=0.6, diag=FALSE, tl.col="black", cl.ratio=0.2, tl.srt=45,
         col.lim=c(0, 1), col=cols, is.corr=FALSE)


# Load country shapefile
data(countriesHigh)
# Crop to area of interest
map_crop <- crop(countriesHigh, extent(-13, 13, 43, 65))

# 2009
load("./Combined/Combined_2009_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2009 <- as.undirected(g)
ceb2009 <- cluster_fast_greedy(g2009)
# Convert the coordinates of the nodes into a matrix
coords <- centroids[match(V(g)$name, rownames(centroids)), ]
coords$ID <- NULL
coords <- as.matrix(coords)
# Plot on map without edges
plot(map_crop, col="light grey")
# Set edges of graph all to same weight/width - otherwise becomes too messy
E(g)$width <- 1
# Add the igraph with communities
plot(ceb2009, g2009, add=TRUE, rescale=FALSE,layout=coords, axes = FALSE, vertex.size=5,
     vertex.label=NA, arrow.size=.5, edge.arrow.size=0.1, edge.size=.5, edge.curved=0.2,
     edge.color=NA)
map.axes(cex.axis=1) # map border and axis

# 2010
load("./Combined/Combined_2010_new.RData")
s.dat <- s.dat[-c(171), -c(171)]
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2010 <- as.undirected(g)
ceb2010 <- cluster_fast_greedy(g2010)
# Convert the coordinates of the nodes into a matrix
coords <- centroids[match(V(g)$name, rownames(centroids)), ]
coords$ID <- NULL
coords <- as.matrix(coords)
# Plot on map without edges
plot(map_crop, col="light grey")
# Set edges of graph all to same weight/width - otherwise becomes too messy
E(g)$width <- 1
# Add the igraph with communities
plot(ceb2010, g2010, add=TRUE, rescale=FALSE,layout=coords, axes = FALSE, vertex.size=5,
     vertex.label=NA, arrow.size=.5, edge.arrow.size=0.1, edge.size=.5, edge.curved=0.2,
     edge.color=NA)
map.axes(cex.axis=1) # map border and axis

# 2000
load("./Combined/Combined_2000_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2000 <- as.undirected(g)
ceb2000 <- cluster_fast_greedy(g2000)
# Convert the coordinates of the nodes into a matrix
coords <- centroids[match(V(g)$name, rownames(centroids)), ]
coords$ID <- NULL
coords <- as.matrix(coords)
# Plot on map without edges
plot(map_crop, col="light grey")
# Set edges of graph all to same weight/width - otherwise becomes too messy
E(g)$width <- 1
# Add the igraph with communities
plot(ceb2000, g2000, add=TRUE, rescale=FALSE,layout=coords, axes = FALSE, vertex.size=5,
     vertex.label=NA, arrow.size=.5, edge.arrow.size=0.1, edge.size=.5, edge.curved=0.2,
     edge.color=NA)
map.axes(cex.axis=1) # map border and axis

# 2008
load("./Combined/Combined_2008_new.RData")
g <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
g2008 <- as.undirected(g)
ceb2008 <- cluster_fast_greedy(g2008)
# Convert the coordinates of the nodes into a matrix
coords <- centroids[match(V(g)$name, rownames(centroids)), ]
coords$ID <- NULL
coords <- as.matrix(coords)
# Plot on map without edges
plot(map_crop, col="light grey")
# Set edges of graph all to same weight/width - otherwise becomes too messy
E(g)$width <- 1
# Add the igraph with communities
plot(ceb2008, g2008, add=TRUE, rescale=FALSE,layout=coords, axes = FALSE, vertex.size=5,
     vertex.label=NA, arrow.size=.5, edge.arrow.size=0.1, edge.size=.5, edge.curved=0.2,
     edge.color=NA)
map.axes(cex.axis=1) # map border and axis
