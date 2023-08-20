# -------------------------------------------------------------------------
# ------------- BGI and Tanglegrams for cluster identification  -----------
# -------------------------------------------------------------------------
# Set the working directory -----------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/Data/Data/igraph environments")

# Load the required packages ----------------------------------------------
{ library(sp)
  library(dplyr)
  library(igraph)
  library(dendextend)
  library(corrplot)
  library(ggplot2)
  library(ccplot)
}

# Load the data -----------------------------------------------------------

# Load the igraph environments
{ load("./Combined/Combined_2000.RData")
  g2000 <- as.undirected(g)
  
  load("./Combined/Combined_2001.RData")
  g2001 <- as.undirected(g)
  
  load("./Combined/Combined_2002.RData")
  g2002 <- as.undirected(g)
  
  load("./Combined/Combined_2003.RData")
  g2003 <- as.undirected(g)
  
  load("./Combined/Combined_2004.RData")
  g2004 <- as.undirected(g)
  
  load("./Combined/Combined_2005.RData")
  g2005 <- as.undirected(g)
  
  load("./Combined/Combined_2006.RData")
  g2006 <- as.undirected(g)
  
  load("./Combined/Combined_2007.RData")
  g2007 <- as.undirected(g)
  
  load("./Combined/Combined_2008.RData")
  g2008 <- as.undirected(g)
  
  load("./Combined/Combined_2009.RData")
  g2009 <- as.undirected(g)
  
  load("./Combined/Combined_2010.RData")
  g2010 <- as.undirected(g)
  
  load("./Combined/Combined_2011.RData")
  g2011 <- as.undirected(g)
  
  load("./Combined/Combined_2012.RData")
  g2012 <- as.undirected(g)
}

# Edit graphs to all have the same number of nodes ------------------------

# 2000 - 2003 have fewer nodes than all other graphs
# Separate the nodes from each graph for 2000 - 2004
# Using the graph from 2004 to identify the missing nodes
{ v2000 <- cbind(igraph::as_data_frame(g2000, "vertices"))
v2000 <- v2000[, c(1, 2)]
colnames(v2000) <- c("name", "2000")
v2001 <- cbind(igraph::as_data_frame(g2001, "vertices"))
v2001 <- v2001[, c(1, 2)]
colnames(v2001) <- c("name", "2001")
v2002 <- cbind(igraph::as_data_frame(g2002, "vertices"))
v2002 <- v2002[, c(1, 2)]
colnames(v2002) <- c("name", "2002")
v2003 <- cbind(igraph::as_data_frame(g2003, "vertices"))
v2003 <- v2003[, c(1, 2)]
colnames(v2003) <- c("name", "2003")
v2004 <- cbind(igraph::as_data_frame(g2004, "vertices"))
v2004 <- v2004[, c(1, 2)]
colnames(v2004) <- c("name", "2004")
}

# Merge them into one data frame, where if a node is not present in one year it 
# is filled with NA
nodes <- merge(v2000, v2001, by="name", all=TRUE)
nodes <- merge(nodes, v2002, by="name", all=TRUE)
nodes <- merge(nodes, v2003, by="name", all=TRUE)
nodes <- merge(nodes, v2004, by="name", all=TRUE)

# Add the missing nodes to each year
v2000 <- as.data.frame(nodes[, c(1,2)]) # Get the data for that year
colnames(v2000) <- c("name", "lat") # Re-name the columns
v2000na <- v2000[rowSums(is.na(v2000))>0,] # Separate rows = NA
v2000na$lat <- NULL # Remove the lat column
rownames(v2000na) <- v2000na$name # Assign the row names
g2000 <- add_vertices(g2000, 67, name=v2000na$name) # Add as vertices to the igraph for that year
# Do the same for 2001-2003
{v2001 <- as.data.frame(nodes[, c(1,3)])
colnames(v2001) <- c("name", "lat")
v2001na <- v2001[rowSums(is.na(v2001))>0,]
v2001na$lat <- NULL
rownames(v2001na) <- v2001na$name
g2001 <- add_vertices(g2001, 38, name=v2001na$name)

v2002 <- as.data.frame(nodes[, c(1,4)])
colnames(v2002) <- c("name", "lat")
v2002na <- v2002[rowSums(is.na(v2002))>0,]
v2002na$lat <- NULL
rownames(v2002na) <- v2002na$name
g2002 <- add_vertices(g2002, 14, name=v2002na$name)

v2003 <- as.data.frame(nodes[, c(1,5)])
colnames(v2003) <- c("name", "lat")
v2003na <- v2003[rowSums(is.na(v2003))>0,]
v2003na$lat <- NULL
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

# Plot to check
{ #dendPlot(ceb2000, mode="hclust")
#dendPlot(ceb2001, mode="hclust")
#dendPlot(ceb2002, mode="hclust")
#dendPlot(ceb2003, mode="hclust")
#dendPlot(ceb2004, mode="hclust")
#dendPlot(ceb2005, mode="hclust")
#dendPlot(ceb2006, mode="hclust")
#dendPlot(ceb2007, mode="hclust")
#dendPlot(ceb2008, mode="hclust")
#dendPlot(ceb2009, mode="hclust")
#dendPlot(ceb2010, mode="hclust")
#dendPlot(ceb2011, mode="hclust")
#dendPlot(ceb2012, mode="hclust")
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

# Plot to check
{ #dend0 %>% set("branches_k_color", k=length(ceb2000)) %>% plot()
#dend1 %>% set("branches_k_color", k=length(ceb2001)) %>%  plot()
#dend2 %>% set("branches_k_color", k=length(ceb2002)) %>%  plot()
#dend3 %>% set("branches_k_color", k=length(ceb2003)) %>%  plot()
#dend4 %>% set("branches_k_color", k=length(ceb2004)) %>%  plot()
#dend5 %>% set("branches_k_color", k=length(ceb2005)) %>%  plot()
#dend6 %>% set("branches_k_color", k=length(ceb2006)) %>%  plot()
#dend7 %>% set("branches_k_color", k=length(ceb2007)) %>%  plot()
#dend8 %>% set("branches_k_color", k=length(ceb2008)) %>%  plot()
#dend9 %>% set("branches_k_color", k=length(ceb2009)) %>%  plot()
#dend10 %>% set("branches_k_color", k=length(ceb2010)) %>%  plot()
#dend11 %>% set("branches_k_color", k=length(ceb2011)) %>%  plot()
#dend12 %>% set("branches_k_color", k=length(ceb2012)) %>%  plot()
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
#      2000  2001  2002  2003  2004  2005  2006  2007  2008  2009  2010  2011  2012
#2000  1.00  
#2001  0.59  1.00  
#2002  0.29  0.39  1.00 
#2003  0.27  0.37  0.42  1.00 
#2004  0.32  0.29  0.49  0.66  1.00 
#2005  0.36  0.26  0.47  0.53  0.76  1.00 
#2006  0.34  0.25  0.45  0.54  0.74  0.98  1.00 
#2007  0.32  0.35  0.41  0.74  0.73  0.71  0.68  1.00 
#2008  0.20  0.34  0.24  0.55  0.51  0.32  0.32  0.44  1.00
#2009  0.34  0.39  0.49  0.83  0.79  0.66  0.64  0.91  0.54  1.00                                      
#2010  0.35  0.38  0.49  0.83  0.80  0.68  0.66  0.91  0.54  0.99  1.00                                             
#2011  0.23  0.32  0.30  0.73  0.55  0.40  0.41  0.60  0.60  0.70  0.70  1.00                                                   
#2012  0.32  0.34  0.40  0.72  0.73  0.69  0.67  0.97  0.44  0.89  0.89  0.58  1.00      

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

# Permutation tests for p values ------------------------------------------

## To get exact p values we look at the distribution of BGI under the null hypothesis
## The null hypothesis is that two trees are not significantly more similar than the
## same tree compared to itself once the leaves have been shuffled.

#set.seed(23235)

## Compare tree to itself
#the_cor <- cor_bakers_gamma(dend11, dend11)
#the_cor # BGI should always = 1
## Save dendrogram labels to speed up for loop
#labels <- labels(dend11)

#R <- 1000 # Number of times to repeat for loop
#cor_bakers_gamma_results <- numeric(R) # Output file
#dend_mixed <- dend11 # Duplicate dendrogram

#for(i in 1:R) {
  #dend_mixed <- sample.dendrogram(dend_mixed, replace=FALSE, dend_labels=labels) # Shuffle the existing leaves of the dendrogram to make it random
  #cor_bakers_gamma_results[i] <- cor_bakers_gamma(dend11, dend_mixed) # Calculate BGI for dendrogram vs shuffled dendrogram
#}

## Density plot of BGI from the permutation test
#plot(density(cor_bakers_gamma_results),
     #main="Baker's gamma distribution under H0",
     #xlim=c(-1,1))
## Add line for dendrogram vs itself
#abline(v=the_cor, lty=2, col=2) #always 1
## Calculate the BGI of the dendrogram vs another dendrogram
#the_cor2 <- cor_bakers_gamma(dend11, dend12)
#abline(v=the_cor2, lty=2, col=4) # Add line for dendrogram vs other dendrogram
#legend("topleft", legend=c("cor", "cor2"), fill=c(2,4))
## Calculate p value
## p value = number of times the BGI of dendrogram vs other dendrogram was smaller than 
## dendrogram vs shuffled dendrogram
#the_cor2 <- cor_bakers_gamma(dend11, dend12)
#round(sum(the_cor2 < cor_bakers_gamma_results)/R)

#       2000  2001  2002  2003  2004  2005  2006  2007  2008  2009  2010  2011
# 2000     
# 2001     0     
# 2002     0     0       
# 2003     0     0     0                
# 2004     0     0     0     0
# 2005     0     0     0     0     0
# 2006     0     0     0     0     0     0  
# 2007     0     0     0     0     0     0     0
# 2008     0     0     0     0     0     0     0     0
# 2009     0     0     0     0     0     0     0     0     0
# 2010     0     0     0     0     0     0     0     0     0     0
# 2011     0     0     0     0     0     0     0     0     0     0     0
# 2012     0     0     0     0     0     0     0     0     0     0     0     0

# Identify common subtrees ------------------------------------------------

# Louise suggested making pie charts to show the number of common sub trees 
# between pairs of years

# This code will return an array showing the subtrees that are shared between the two dendrograms
#clusters1 <- common_subtrees_clusters(dend9, dend10)
#clusters1

# There's probably a quicker/easier way of doing this, but oh well

years <- 2000:2012
comp <- as.data.frame(matrix(nrow=13, ncol=13))
colnames(comp) <- years
rownames(comp) <- years
comp <- as.matrix(comp)
comp <- reshape2::melt(comp)
comp <- comp[,c(2, 1, 3)]
colnames(comp) <- c("Year_1", "Year_2", "Shared")
comp$Unique <- NA
comp <- comp[!comp$Year_1==comp$Year_2,]

# 2000 vs all else
{ clusters <- common_subtrees_clusters(dend0, dend1)
comp[1,3] <- sum(clusters > 0)
comp[1,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend2)
comp[2,3] <- sum(clusters > 0)
comp[2,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend3)
comp[3,3] <- sum(clusters > 0)
comp[3,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend4)
comp[4,3] <- sum(clusters > 0)
comp[4,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend5)
comp[5,3] <- sum(clusters > 0)
comp[5,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend6)
comp[6,3] <- sum(clusters > 0)
comp[6,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend7)
comp[7,3] <- sum(clusters > 0)
comp[7,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend8)
comp[8,3] <- sum(clusters > 0)
comp[8,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend9)
comp[9,3] <- sum(clusters > 0)
comp[9,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend10)
comp[10,3] <- sum(clusters > 0)
comp[10,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend11)
comp[11,3] <- sum(clusters > 0)
comp[11,4] <- sum(clusters == 0)

clusters <- common_subtrees_clusters(dend0, dend12)
comp[12,3] <- sum(clusters > 0)
comp[12,4] <- sum(clusters == 0)
}

# Remove repeated rows
comp <- comp[-c(13), ]
# 2001 vs all else
{ clusters <- common_subtrees_clusters(dend1, dend2)
  comp[13,3] <- sum(clusters > 0)
  comp[13,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend1, dend3)
  comp[14,3] <- sum(clusters > 0)
  comp[14,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend1, dend4)
  comp[15,3] <- sum(clusters > 0)
  comp[15,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend1, dend5)
  comp[16,3] <- sum(clusters > 0)
  comp[16,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend1, dend6)
  comp[17,3] <- sum(clusters > 0)
  comp[17,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend1, dend7)
  comp[18,3] <- sum(clusters > 0)
  comp[18,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend1, dend8)
  comp[19,3] <- sum(clusters > 0)
  comp[19,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend1, dend9)
  comp[20,3] <- sum(clusters > 0)
  comp[20,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend1, dend10)
  comp[21,3] <- sum(clusters > 0)
  comp[21,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend1, dend11)
  comp[22,3] <- sum(clusters > 0)
  comp[22,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend1, dend12)
  comp[23,3] <- sum(clusters > 0)
  comp[23,4] <- sum(clusters == 0)
}

comp <- comp[-c(24, 25), ]
# 2002 vs all else
{ clusters <- common_subtrees_clusters(dend2, dend3)
  comp[24,3] <- sum(clusters > 0)
  comp[24,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend2, dend4)
  comp[25,3] <- sum(clusters > 0)
  comp[25,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend2, dend5)
  comp[26,3] <- sum(clusters > 0)
  comp[26,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend2, dend6)
  comp[27,3] <- sum(clusters > 0)
  comp[27,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend2, dend7)
  comp[28,3] <- sum(clusters > 0)
  comp[28,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend2, dend8)
  comp[29,3] <- sum(clusters > 0)
  comp[29,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend2, dend9)
  comp[30,3] <- sum(clusters > 0)
  comp[30,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend2, dend10)
  comp[31,3] <- sum(clusters > 0)
  comp[31,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend2, dend11)
  comp[32,3] <- sum(clusters > 0)
  comp[32,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend2, dend12)
  comp[33,3] <- sum(clusters > 0)
  comp[33,4] <- sum(clusters == 0)
}

comp <- comp[-c(34, 35, 36), ]
# 2003 vs all else
{ clusters <- common_subtrees_clusters(dend3, dend4)
  comp[34,3] <- sum(clusters > 0)
  comp[34,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend3, dend5)
  comp[35,3] <- sum(clusters > 0)
  comp[35,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend3, dend6)
  comp[36,3] <- sum(clusters > 0)
  comp[36,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend3, dend7)
  comp[37,3] <- sum(clusters > 0)
  comp[37,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend3, dend8)
  comp[38,3] <- sum(clusters > 0)
  comp[38,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend3, dend9)
  comp[39,3] <- sum(clusters > 0)
  comp[39,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend3, dend10)
  comp[40,3] <- sum(clusters > 0)
  comp[40,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend3, dend11)
  comp[41,3] <- sum(clusters > 0)
  comp[41,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend3, dend12)
  comp[42,3] <- sum(clusters > 0)
  comp[42,4] <- sum(clusters == 0)
}

comp <- comp[-c(43, 44, 45, 46), ]
# 2004 vs all else
{ clusters <- common_subtrees_clusters(dend4, dend5)
  comp[43,3] <- sum(clusters > 0)
  comp[43,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend4, dend6)
  comp[44,3] <- sum(clusters > 0)
  comp[44,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend4, dend7)
  comp[45,3] <- sum(clusters > 0)
  comp[45,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend4, dend8)
  comp[46,3] <- sum(clusters > 0)
  comp[46,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend4, dend9)
  comp[47,3] <- sum(clusters > 0)
  comp[47,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend4, dend10)
  comp[48,3] <- sum(clusters > 0)
  comp[48,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend4, dend11)
  comp[49,3] <- sum(clusters > 0)
  comp[49,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend4, dend12)
  comp[50,3] <- sum(clusters > 0)
  comp[50,4] <- sum(clusters == 0)
}

comp <- comp[-c(51, 52, 53, 54, 55), ]
# 2005 vs all else
{ clusters <- common_subtrees_clusters(dend5, dend6)
  comp[51,3] <- sum(clusters > 0)
  comp[51,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend5, dend7)
  comp[52,3] <- sum(clusters > 0)
  comp[52,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend5, dend8)
  comp[53,3] <- sum(clusters > 0)
  comp[53,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend5, dend9)
  comp[54,3] <- sum(clusters > 0)
  comp[54,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend5, dend10)
  comp[55,3] <- sum(clusters > 0)
  comp[55,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend5, dend11)
  comp[56,3] <- sum(clusters > 0)
  comp[56,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend5, dend12)
  comp[57,3] <- sum(clusters > 0)
  comp[57,4] <- sum(clusters == 0)
}

comp <- comp[-c(58, 59, 60, 61, 62, 63), ]
# 2006 vs all else
{ clusters <- common_subtrees_clusters(dend6, dend7)
  comp[58,3] <- sum(clusters > 0)
  comp[58,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend6, dend8)
  comp[59,3] <- sum(clusters > 0)
  comp[59,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend6, dend9)
  comp[60,3] <- sum(clusters > 0)
  comp[60,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend6, dend10)
  comp[61,3] <- sum(clusters > 0)
  comp[61,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend6, dend11)
  comp[62,3] <- sum(clusters > 0)
  comp[62,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend6, dend12)
  comp[63,3] <- sum(clusters > 0)
  comp[63,4] <- sum(clusters == 0)
}

comp <- comp[-c(64, 65, 66, 67, 68, 69, 70), ]
# 2007 vs all else
{ clusters <- common_subtrees_clusters(dend7, dend8)
  comp[64,3] <- sum(clusters > 0)
  comp[64,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend7, dend9)
  comp[65,3] <- sum(clusters > 0)
  comp[65,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend7, dend10)
  comp[66,3] <- sum(clusters > 0)
  comp[66,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend7, dend11)
  comp[67,3] <- sum(clusters > 0)
  comp[67,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend7, dend12)
  comp[68,3] <- sum(clusters > 0)
  comp[68,4] <- sum(clusters == 0)
}

comp <- comp[-c(69, 70, 71, 72, 73, 74, 75, 76), ]
# 2008 vs all else
{ clusters <- common_subtrees_clusters(dend8, dend9)
  comp[69,3] <- sum(clusters > 0)
  comp[69,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend8, dend10)
  comp[70,3] <- sum(clusters > 0)
  comp[70,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend8, dend11)
  comp[71,3] <- sum(clusters > 0)
  comp[71,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend8, dend12)
  comp[72,3] <- sum(clusters > 0)
  comp[72,4] <- sum(clusters == 0)
}

comp <- comp[-c(73, 74, 75, 76, 77, 78, 79, 80, 81), ]
# 2009 vs all else
{ clusters <- common_subtrees_clusters(dend9, dend10)
  comp[73,3] <- sum(clusters > 0)
  comp[73,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend9, dend11)
  comp[74,3] <- sum(clusters > 0)
  comp[74,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend9, dend12)
  comp[75,3] <- sum(clusters > 0)
  comp[75,4] <- sum(clusters == 0)
}

comp <- comp[-c(76, 77, 78, 79, 80, 81, 82, 83, 84, 85), ]
# 2010 vs all else
{ clusters <- common_subtrees_clusters(dend10, dend11)
  comp[76,3] <- sum(clusters > 0)
  comp[76,4] <- sum(clusters == 0)
  
  clusters <- common_subtrees_clusters(dend10, dend12)
  comp[77,3] <- sum(clusters > 0)
  comp[77,4] <- sum(clusters == 0)
}

comp <- comp[-c(78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88), ]
# 2011 vs all else
{ clusters <- common_subtrees_clusters(dend11, dend12)
  comp[78,3] <- sum(clusters > 0)
  comp[78,4] <- sum(clusters == 0)
}

comp <- comp[-c(79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90), ]
rownames(comp) <- 1:78

# Draw pie charts for just the least and most correlated years

# 2000 vs 2008 - least correlated
pie(c(comp$Shared[8], comp$Unique[8]), labels=c("Common subtrees", "Unique subtrees"),
    border="white")

# 2009 vs 2010
pie(c(comp$Shared[73], comp$Unique[73]), labels=c("Common subtrees", "Unique subtrees"),
    border="white")

# Shared subgraphs for subsequent years -----------------------------------

comp2 <- comp[c(1, 13, 24, 34, 43, 51, 58, 64, 69, 73, 76, 78),]

comp2$prop <- NA
# Calculate proportion of subgraphs that are shared
{ comp2[1,5] <- comp2[1,3]/(comp2[1,3]+comp2[1,4])
comp2[2,5] <- comp2[2,3]/(comp2[2,3]+comp2[2,4])
comp2[3,5] <- comp2[3,3]/(comp2[3,3]+comp2[3,4])
comp2[4,5] <- comp2[4,3]/(comp2[4,3]+comp2[4,4])
comp2[5,5] <- comp2[5,3]/(comp2[5,3]+comp2[5,4])
comp2[6,5] <- comp2[6,3]/(comp2[6,3]+comp2[6,4])
comp2[7,5] <- comp2[7,3]/(comp2[7,3]+comp2[7,4])
comp2[8,5] <- comp2[8,3]/(comp2[8,3]+comp2[8,4])
comp2[9,5] <- comp2[9,3]/(comp2[9,3]+comp2[9,4])
comp2[10,5] <- comp2[10,3]/(comp2[10,3]+comp2[10,4])
comp2[11,5] <- comp2[11,3]/(comp2[11,3]+comp2[11,4])
comp2[12,5] <- comp2[12,3]/(comp2[12,3]+comp2[12,4])
}

comp$Year_1 <- as.caharcter(comp$Year_1)

ggplot()+
  geom_line(comp2, mapping=aes(x=Year_1, y=prop, group=1), size=1)+
  cc_theme()+
  ylim(c(0, 1))
