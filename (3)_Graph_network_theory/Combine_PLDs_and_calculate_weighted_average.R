# This script combines the igraph environments for the four PLD scenarios
# for each year to one R environment, and also creates an R environment 
# for the weighted average connectivity across the 13 years modeled. 
# PLD scenarios were combined to account for variation in dispersal patterns as 
# a result of the unknown PLD of Pacific oysters. The resulting environments are 
# the ones used in all network metric calculations to create the figures in the 
# manuscript.

# Load the required packages --------------------------------------------------
{ library(dplyr)
  library(reshape2)
  library(igraph)
  library(rworldxtra)
  library(raster)
  library(maps)
}

# 2000 --------------------------------------------------------------------

# Load the 14 day PLD data
load("./14 PLD/PLD14_2000.RData")
connectivity14 <- connectivity

# Load the 17 day PLD data
load("./17 PLD/PLD17_2000.RData")
connectivity17 <- connectivity

# Load the 25 day PLD data
load("./25 PLD/PLD25_2000.RData")
connectivity25 <- connectivity

# Load the 28 day PLD data
load("./28 PLD/PLD28_2000.RData")
connectivity28 <- connectivity

# Combine
connectivity2000 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

# Create new myFreqs data frame
myFreqs <- connectivity2000 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
# Save as that year
c_2000 <- myFreqs

# Clean environment
rm(centroids, connectivity, connectivity14, connectivity17, connectivity25, connectivity28, coords, g_14,
   g_17, g_25, g_28, myFreqs, s.dat)

# 2001 --------------------------------------------------------------------

load("./14 PLD/PLD14_2001.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2001.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2001.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2001.RData")
connectivity28 <- connectivity

connectivity2001 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2001 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2001 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2002 --------------------------------------------------------------------

load("./14 PLD/PLD14_2002.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2002.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2002.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2002.RData")
connectivity28 <- connectivity

connectivity2002 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2002 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2002 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2003 --------------------------------------------------------------------

load("./14 PLD/PLD14_2003.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2003.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2003.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2003.RData")
connectivity28 <- connectivity

connectivity2003 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2003 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2003 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2004 --------------------------------------------------------------------

load("./14 PLD/PLD14_2004.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2004.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2004.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2004.RData")
connectivity28 <- connectivity

connectivity2004 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2004 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2004 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2005 --------------------------------------------------------------------

load("./14 PLD/PLD14_2005.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2005.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2005.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2005.RData")
connectivity28 <- connectivity

connectivity2005 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2005 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2005 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2006 --------------------------------------------------------------------

load("./14 PLD/PLD14_2006.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2006.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2006.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2006.RData")
connectivity28 <- connectivity

connectivity2006 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2006 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2006 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2007 --------------------------------------------------------------------

load("./14 PLD/PLD14_2007.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2007.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2007.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2007.RData")
connectivity28 <- connectivity

connectivity2007 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2007 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2007 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2008 --------------------------------------------------------------------

load("./14 PLD/PLD14_2008.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2008.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2008.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2008.RData")
connectivity28 <- connectivity

connectivity2008 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2008 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2008 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2009 --------------------------------------------------------------------

load("./14 PLD/PLD14_2009.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2009.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2009.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2009.RData")
connectivity28 <- connectivity

connectivity2009 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2009 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2009 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2010 --------------------------------------------------------------------

load("./14 PLD/PLD14_2010_2.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2010.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2010.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2010.RData")
connectivity28 <- connectivity

connectivity2010 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2010 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2010 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2011 --------------------------------------------------------------------

load("./14 PLD/PLD14_2011_2.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2011.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2011.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2011.RData")
connectivity28 <- connectivity

connectivity2011 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2011 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2011 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# 2012 --------------------------------------------------------------------

load("./14 PLD/PLD14_2012.RData")
connectivity14 <- connectivity

load("./17 PLD/PLD17_2012.RData")
connectivity17 <- connectivity

load("./25 PLD/PLD25_2012.RData")
connectivity25 <- connectivity

load("./28 PLD/PLD28_2012.RData")
connectivity28 <- connectivity

connectivity2012 <- rbind(connectivity14, connectivity17, connectivity25, connectivity28)

myFreqs <- connectivity2012 %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 
c_2012 <- myFreqs

rm(attrs, bridge, ceb, centroids, connectivity, connectivity14, connectivity17, connectivity25, 
   connectivity28, coords, coords_14, coords_17, coords_25, coords_28, countriesHigh, 
   e_between, edges, el, el_d, g, g_14, g_17, g_25, g_28, g2, in_degree, map_crop, myFreqs, 
   out_degree, s.dat, v_between, deg_in, deg_out)

# Calculate the weighted average of each edge -----------------------------

# Merge the data together
connections <- merge(c_2000, c_2001, by=c("Release_poly", "Settlement_poly"), all=TRUE)
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

# Rename the columns
colnames(connections) <- c("Release_poly", "Settlement_poly", "c_2000", "c_2001", "c_2002", "c_2003",
                           "c_2004", "c_2005", "c_2006", "c_2007", "c_2008", "c_2009", "c_2010", 
                           "c_2011", "c_2012")

# Calculate how many years each link appears in 
connections$freq <- rowSums(is.na(connections))
connections$freq <- 13-connections$freq

# Multiply the link weight by the number of years the link appears in to get the 
# weighted value
connections[3:15] <- connections[3:15]*connections$freq

# Calculate the weighting factor by multiplying the weight by itself
connections$weight <- connections$freq*connections$freq

# Now calculate the weighted average across years
connections$avg <- (rowSums(connections[3:15], na.rm=TRUE))/connections$weight

# Create the new myFreqs data frame 
myFreqs <- as.data.frame(connections[, c(1, 2, 18)])
colnames(myFreqs) <- c("Release_poly", "Settlement_poly", "Count")

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(connections$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(connections$Settlement_poly)
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

# Create a matrix of the count for each combination of release and settlement olygons
s.dat <- acast(myFreqs, Release_poly~Settlement_poly, value.var = "Count")
# Check the dimensions of the matrix - they must be the same
# Use the polygon IDs here to update the spawning locations file for the year
dim(s.dat)

# Save the R environment for future use
# Save the environment
save.image(file='./Combined.RData')
