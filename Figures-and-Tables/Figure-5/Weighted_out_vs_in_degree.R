# -------------------------------------------------------------------------
# ----------------------- Weighted out- and in-degree ---------------------
# -------------------------------------------------------------------------
# Set the working directory ---------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/Data/Data/igraph environments")

# Load the required packages --------------------------------------------------

{library(dplyr)
library(igraph)
library(ggplot2)
library(ggpubr)
library(ccplot)
library(reshape2)
library(rworldxtra)
library(rgdal)
library(sf)
library(raster)
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

# Isolate the release polygons - make sure you are using the 2012 data
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

# Degree distribution (for supplementary material)
ggdensity(metrics$deg)+
  xlab("Total node degree")+
  ylab("Density")+
  cc_theme()

# Quick visualisation
ggplot(data=metrics, aes(x=deg_out, y=deg_in))+
  geom_point()+
  geom_smooth()
# Looks like it could be linear

# Try a linear model
mod <- lm(deg_in~deg_out, data=metrics)
mod
# Regression equation: y = 6.84 + 0.49*x     OR     In degree = 6.84 + 0.49 * Out degree
par(mfrow = c(2,2))
plot(mod)
# Looks okay to me!

# Check the residuals for normality
shapiro.test(residuals(mod))
# P-value = 0.81
# Residuals are fine

# Summarise the model
summary(mod)
# RSE = 4.426 on 170 DoF
# Multiple R-squared = 0.3131
# Adjusted R-squared = 0.3091
# F-stat = 77.5
# P-value = 1.47e-15 
# In degree increases by 0.49443 +- 0.05616 for every 1 unit increase in out degree

# Carry out correlation analysis to determine the strength of the linear relationship
corr <- cor.test(x=metrics$deg_out, y=metrics$deg_in, method='pearson')
corr
# r = 0.5595773

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

par(mfrow = c(1,1))
ggdensity(metrics$deg)

# Plot as a map -----------------------------------------------------------

# Load grid
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/(3) Methods/Modelling/Original release file/Shapefiles/Ocean Grid/1 degree")
ocean_grid <- readOGR(dsn=".", layer="ocean_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/Data/Data")

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



