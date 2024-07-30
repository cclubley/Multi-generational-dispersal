# Set wd ------------------------------------------------------------------

{library(reshape2)
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
  library(dplyr)
  library(corrplot)
  library(dendextend)
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# --------------------------------- 2000 ----------------------------------
# Load the combined PLD data ------------------------------------------

load("./Combined/Combined_2000_new.RData")

# Implement mortality -----------------------------------------------------

# Duplicate and re-name for purposes of code
myFreqs <- c_2000

# Use the max mortality rate, which is 70% (28 day PLD)

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123) # set seed for reproducibility
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
# Create variable to keep track of particle removal
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the count of the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index (either the number of particles or the remaining reduction, whichever is smaller)
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1) # select a random amount of particles to be removed, between 1 and the max reduction
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 50 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 50

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

# Create a new graph object
g_2000 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2000) # 96

# Rename myFreqs for purpose of saving
myFreqs_2000 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "myFreqs_2000")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2001 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2001_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("733", "782", "830", "844", "887", "888", "889", "1362")

# Remove from spawning polygons
c_2001 <- c_2001[!(c_2001$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2001

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 115 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 115

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2001 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2001) # 129

myFreqs_2001 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "myFreqs_2000","myFreqs_2001")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2002 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2002_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("786", "837", "884", "1124", "1359")

# Remove from spawning polygons
c_2002 <- c_2002[!(c_2002$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2002

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 163 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 163

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2002 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2002) # 155

myFreqs_2002 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "myFreqs_2000","myFreqs_2001", "myFreqs_2002")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2003 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2003_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("412", "413", "414", "415", "460", "461", "493", "494", "506", "1356")

# Remove from spawning polygons
c_2003 <- c_2003[!(c_2003$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2003

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 264 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 264

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2003 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2003) # 168

myFreqs_2003 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "g_2003", "myFreqs_2000","myFreqs_2001", "myFreqs_2002", "myFreqs_2003")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2004 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2004_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("446", "493", "494", "1402")

# Remove from spawning polygons
c_2004 <- c_2004[!(c_2004$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2004

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 248 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 248

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2004 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2004) # 169

myFreqs_2004 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "g_2003", "g_2004", "myFreqs_2000","myFreqs_2001", "myFreqs_2002", "myFreqs_2003", "myFreqs_2004")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2005 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2005_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("446", "493", "494", "1402")

# Remove from spawning polygons
c_2005 <- c_2005[!(c_2005$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2005

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 266 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 266

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2005 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2005) # 169

myFreqs_2005 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "g_2003", "g_2004", "g_2005", "myFreqs_2000","myFreqs_2001", "myFreqs_2002", "myFreqs_2003", "myFreqs_2004", "myFreqs_2005")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2006 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2006_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("446", "493", "494", "1402")

# Remove from spawning polygons
c_2006 <- c_2006[!(c_2006$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2006

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 240 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 240

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2006 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2006) # 169

myFreqs_2006 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "g_2003", "g_2004", "g_2005", "g_2006", "myFreqs_2000","myFreqs_2001", "myFreqs_2002", "myFreqs_2003", "myFreqs_2004", "myFreqs_2005", "myFreqs_2006")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2007 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2007_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("446", "493", "494", "1402")

# Remove from spawning polygons
c_2007 <- c_2007[!(c_2007$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2007

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 240 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 259

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2007 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2007) # 168

myFreqs_2007 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "g_2003", "g_2004", "g_2005", "g_2006", "g_2007", "myFreqs_2000","myFreqs_2001", "myFreqs_2002", "myFreqs_2003", "myFreqs_2004", "myFreqs_2005", "myFreqs_2006", "myFreqs_2007")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2008 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2008_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("446", "493", "494", "1402")

# Remove from spawning polygons
c_2008 <- c_2008[!(c_2008$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2008

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 309 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 309

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2008 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2008) # 169

myFreqs_2008 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "g_2003", "g_2004", "g_2005", "g_2006", "g_2007", "g_2008", "myFreqs_2000","myFreqs_2001", "myFreqs_2002", "myFreqs_2003", "myFreqs_2004", "myFreqs_2005", "myFreqs_2006", "myFreqs_2007", "myFreqs_2008")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2009 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2009_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("446", "493", "494", "1402")

# Remove from spawning polygons
c_2009 <- c_2009[!(c_2009$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2009

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 289 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 289

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2009 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2009) # 169

myFreqs_2009 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "g_2003", "g_2004", "g_2005", "g_2006", "g_2007", "g_2008", "g_2009", "myFreqs_2000","myFreqs_2001", "myFreqs_2002", "myFreqs_2003", "myFreqs_2004", "myFreqs_2005", "myFreqs_2006", "myFreqs_2007", "myFreqs_2008", "myFreqs_2009")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2010 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2010_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("446", "493", "494", "1402")

# Remove from spawning polygons
c_2010 <- c_2010[!(c_2010$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2010

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 276 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 276

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2010 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2010) # 169

myFreqs_2010 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "g_2003", "g_2004", "g_2005", "g_2006", "g_2007", "g_2008", "g_2009", "g_2010", "myFreqs_2000","myFreqs_2001", "myFreqs_2002", "myFreqs_2003", "myFreqs_2004", "myFreqs_2005", "myFreqs_2006", "myFreqs_2007", "myFreqs_2008", "myFreqs_2009", "myFreqs_2010")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2011 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2011_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("446", "493", "494")

# Remove from spawning polygons
c_2011 <- c_2011[!(c_2011$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2011

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 249 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 249

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2011 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2011) # 170

myFreqs_2011 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "g_2003", "g_2004", "g_2005", "g_2006", "g_2007", "g_2008", "g_2009", "g_2010", "g_2011", "myFreqs_2000","myFreqs_2001", "myFreqs_2002", "myFreqs_2003", "myFreqs_2004", "myFreqs_2005", "myFreqs_2006", "myFreqs_2007", "myFreqs_2008", "myFreqs_2009", "myFreqs_2010", "myFreqs_2011")
rm(list=setdiff(ls(), keep_object))

# --------------------------------- 2012 -----------------------------------
# Load the combined PLD data ----------------------------------------

load("./Combined/Combined_2012_new.RData")

# List the unwanted polygons based on the nodes that are no longer colonised during the previous year
polygons_to_remove <- c("446", "493", "494")

# Remove from spawning polygons
c_2012 <- c_2012[!(c_2012$Release_poly %in% polygons_to_remove), ]

# Implement mortality -----------------------------------------------------

myFreqs <- c_2012

# Use the max mortality rate, which is 70%

# Calculate total number of particles
total_sum <- sum(myFreqs$Count)
# Determine amount to remove (70%)
reduction_amount <- 0.7*total_sum

set.seed(123)
# Copy data
myFreqs_m <- as.data.frame(myFreqs$Count)
remaining_reduction <- reduction_amount

# Implement mortality
while (remaining_reduction > 0){ # Keep reducing particles until target reached
  index <- sample(1:nrow(myFreqs_m), 1) # Randomly select an index to reduce
  # Check if the selected index is 0 - if so then skip
  if (myFreqs_m[index, 1]==0){ 
    next
  }
  max_reduction <- min(myFreqs_m[index, 1], remaining_reduction) # Determine maximum possible reduction for this index
  if (max_reduction > 0){
    reduction <- sample(1:max_reduction, 1)
    myFreqs_m[index, 1] <- myFreqs_m[index, 1]-reduction # reduce the value at the specified index
    remaining_reduction <- remaining_reduction-reduction # Update remaining reduction amount
  }
}

# Check 70% has been removed
(sum(myFreqs_m$`myFreqs$Count`)/sum(myFreqs$Count))*100
# 30% remaining 

# 269 connections are no longer present after mortality
sum(myFreqs$Count==0) # 0
sum(myFreqs_m==0) # 269

# Update the link weights 
myFreqs$Count <- myFreqs_m$`myFreqs$Count`
# And remove links that now have a weight of zero
myFreqs <- myFreqs %>% 
  filter(Count != 0)

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
poly_id1 <- as.factor(myFreqs$Release_poly)
# Get the attributes of the variable
poly_id1 <- levels(poly_id1)
# Convert to a dataframe
poly_id1 <- as.data.frame(poly_id1)
# Re-name the column
colnames(poly_id1) <- "Polygon"

# Do the same for the settlement polygons
poly_id2 <- as.factor(myFreqs$Settlement_poly)
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

# Number of nodes when mortality included ---------------------------------

g_2012 <- graph_from_adjacency_matrix(s.dat, mode="directed", weighted=TRUE, diag=FALSE)
V(g_2012) # 169

myFreqs_2012 <- myFreqs

# Clean environment except from new graph
keep_object <- c("g_2000", "g_2001", "g_2002", "g_2003", "g_2004", "g_2005", "g_2006", "g_2007", "g_2008", "g_2009", "g_2010", "g_2011", "g_2012", "myFreqs_2000","myFreqs_2001", "myFreqs_2002", "myFreqs_2003", "myFreqs_2004", "myFreqs_2005", "myFreqs_2006", "myFreqs_2007", "myFreqs_2008", "myFreqs_2009", "myFreqs_2010", "myFreqs_2011", "myFreqs_2012")
rm(list=setdiff(ls(), keep_object))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Weighted averages with mortality ----------------------------------------
# Calculate the weighted average of each edge -----------------------------

# Merge the data together
connections <- merge(myFreqs_2000, myFreqs_2001, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2002, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2003, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2004, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2005, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2006, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2007, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2008, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2009, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2010, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2011, by=c("Release_poly", "Settlement_poly"), all=TRUE)
connections <- merge(connections, myFreqs_2012, by=c("Release_poly", "Settlement_poly"), all=TRUE)

# Rename the columns
colnames(connections) <- c("Release_poly", "Settlement_poly", "c_2000", "c_2001", "c_2002", "c_2003",
"c_2004", "c_2005", "c_2006", "c_2007", "c_2008", "c_2009", "c_2010", 
"c_2011", "c_2012")

connections <- filter(connections, !Release_poly=="1402")
connections <- filter(connections, !Settlement_poly=="1402")

# Calculate how many years each link appears in 
connections$freq <- rowSums(is.na(connections))
connections$freq <- 13-connections$freq

# Multiply the link weight by the number of years the link appears in to get the 
# weighted value
connections[3:15] <- connections[3:15]*connections$freq

# Calculae the weighting factor by multiplying the weight by itself
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

# Create a matrix of the count for each combination of release and settlement polygons
s.dat <- acast(myFreqs, Release_poly~Settlement_poly, value.var = "Count")
# Check the dimensions of the matrix - they must be the same
# Use the polygon IDs here to update the spawning locations file for the year
dim(s.dat)
# 169

myFreqs_com <- myFreqs

# Save the R environment for future use
# Save the environment
save.image(file='./New_combined_w_mortality.RData')

# Load new data -----------------------------------------------------------

load("./Newcombined_edited.RData")

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Plotting the edges on a map ---------------------------------------------

# Convert the coordinates of the nodes into a matrix
coords$ID <- NULL
coords <- as.matrix(coords)

coords.df <- as.data.frame(coords)  ## convert the layout to a data.frame
coords.df$ID <- rownames(coords)  ## add in the polygon IDs

myFreqs <- myFreqs_com
rm(myFreqs_com)

# remove local retention/self-recruitment
myFreqs_unique <- myFreqs[myFreqs$Release_poly != myFreqs$Settlement_poly, ]
myFreqs_unique <- filter(myFreqs_unique, Count > 0)

# Create a separate data frame for only local retention/self-recruitment
myFreqs_sr <- myFreqs[myFreqs$Release_poly == myFreqs$Settlement_poly, ]
myFreqs_sr <- filter(myFreqs_sr, Count > 0)

# Convert the data frame to something 'plotable'
head(myFreqs_unique)
myFreqs_unique$from.x <- coords.df$lon[match(myFreqs_unique$Release_poly, coords.df$ID)]  #  match the from locations from the node data.frame we previously connected
myFreqs_unique$from.y <- coords.df$lat[match(myFreqs_unique$Release_poly, coords.df$ID)]
myFreqs_unique$to.x <- coords.df$lon[match(myFreqs_unique$Settlement_poly, coords.df$ID)]  #  match the to locations from the node data.frame we previously connected
myFreqs_unique$to.y <- coords.df$lat[match(myFreqs_unique$Settlement_poly, coords.df$ID)]
head(myFreqs_unique)

# Normalise link weight between 0 and 1 to aid interpretation
myFreqs_unique$norm_count <- (myFreqs_unique$Count-min(myFreqs_unique$Count))/(max(myFreqs_unique$Count)-min(myFreqs_unique$Count))

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "medium", returnclass = "sf")

# Re-order the data frame so strongest links appear on top
myFreqs_unique <- myFreqs_unique[order(myFreqs_unique$norm_count, decreasing=FALSE), ]

# With perceptually uniform colours ---------------------------------------

# Perceptually uniform colour palette
cols <- magma(200)
cols <- cols[200:100]

# PLot all links together
ggplot(data=world) +
  geom_sf(colour="black", fill="lightgrey")+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+ #Crop map to desired area
  geom_segment(data=myFreqs_unique, aes(x=from.x, xend=to.x, y=from.y, yend=to.y, 
                                        col=norm_count)) +
  scale_color_viridis(option="A", direction=-1, begin=0, end=.9)+
  geom_point(data=coords.df, aes(x=lon, y=lat), size=1.2, colour="black") +
  xlab("Longitude")+
  ylab("Latitude")+
  cc_theme()+
  theme(legend.position="right")

# Split the scale to values < 0.1 and >= 0.1  
summary(myFreqs_unique$Count)
plotrix::std.error(myFreqs_unique$Count)
myFreqs_lower <- filter(myFreqs_unique, norm_count < 0.1)
max(myFreqs_lower$norm_count)
myFreqs_lower <- myFreqs_lower[order(myFreqs_lower$norm_count, decreasing=FALSE), ]

myFreqs_upper <- filter(myFreqs_unique, norm_count >= 0.1)
min(myFreqs_upper$norm_count)
myFreqs_upper <- myFreqs_upper[order(myFreqs_upper$norm_count, decreasing=FALSE), ]

# PLot links < 0.1
ggplot(data=world) +
  geom_sf(colour="black", fill="lightgrey")+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+ #Crop map to desired area
  geom_segment(data=myFreqs_lower, aes(x=from.x, xend=to.x, y=from.y, yend=to.y, col=norm_count)) +
  scale_color_viridis(limits=c(0, 0.1), option="A", direction=-1, begin=0, end=.9)+
  geom_point(data=coords.df, aes(x=lon, y=lat), size=1.2, colour="black") +
  xlab("Longitude")+
  ylab("Latitude")+
  cc_theme()+
  theme(legend.position="right")

# PLot links >= 0.1
ggplot(data=world) +
  geom_sf(colour="black", fill="lightgrey")+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+ #Crop map to desired area
  geom_segment(data=myFreqs_upper, aes(x=from.x, xend=to.x, y=from.y, yend=to.y, col=norm_count)) +
  scale_color_viridis(limits=c(0.1, 1), option="A", direction=-1, begin=0, end=.9)+
  geom_point(data=coords.df, aes(x=lon, y=lat), size=1.2, colour="black") +
  xlab("Longitude")+
  ylab("Latitude")+
  cc_theme()+
  theme(legend.position="right")

# Calculate local retention metrics ---------------------------------------

# Calculate percentage of links that were self-recruitment
(nrow(myFreqs_sr)/nrow(myFreqs))*100
# self-recruitment accounted for 0.5602045% of connections 

# Calculate percentage contribution of self recruitment to total edge weight
(sum(myFreqs_sr$Count)/sum(myFreqs$Count))*100
# Self-recruitment accounted for 31.30843% of edge weight 

# Density of edges --------------------------------------------------------

# PLot a histogram of normalised link weight
ggplot(myFreqs_unique, aes(x=norm_count))+
  geom_histogram()+
  geom_vline(aes(xintercept=median(norm_count)),
             color="blue", linetype="dashed", size=1)+
  #ylim(c(0, 1500))+
  xlab("Normalised Link weight")+
  ylab("Count")+
  cc_theme()

median(myFreqs_unique$norm_count)
# 0.002912655

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Read the distance data --------------------------------------------------

distances2 <- read.csv("./Distances_no_zero.csv")

new_data <- merge(distances2, myFreqs, by=(c("Release_poly", "Settlement_poly")))
colnames(new_data) <- c("Release_poly", "Settlement_poly", "Distance", "Count", "avg")
new_data$Count <- NULL

# Summarise and plot ------------------------------------------------------

# Plot distances with count > 0 and no self-recruitment
ggplot(data=new_data, aes(x=Distance, y=avg))+
  geom_point()

# Summary, all edges with weight > 0 and no self-recruitment
summary(new_data$Distance)
plotrix::std.error(new_data$Distance)
# The greatest distance between two nodes with a connection was 923.15 km
# The minimum distance (not including self-recruitment) was 11.92 km
# The median distance was 176.95 km
# The average distance was 206.54 km +- 2.868126 

dist <- filter(new_data, Distance <=260)
(nrow(dist)/nrow(new_data))*100
# 75% of links are 260 km or less apart

dist <- filter(new_data, Distance <=175)
(nrow(dist)/nrow(new_data))*100
# 50% of links are 175 km or less apart

dist <- filter(new_data, Distance <=115)
(nrow(dist)/nrow(new_data))*100
# 25% of links are 115 km or less apart

# Fit an exponential decay curve ------------------------------------------

# https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/
# https://tjmurphy.github.io/jabstb/nonlinearreplicates.html 

# Nonlinear regression is a statistical method to fit nonlinear models to the 
# kinds of data sets that have nonlinear relationships between independent and 
# dependent variables.

# In nonlinear regression we fit a model formula to pairs of X,Y data from an 
# experiment. The best fitting model parameters responsible for giving nonlinear 
# shape to the relationship between X and Y are then determined by the regression
# method.

# Best fit of the regression curve is determined by a minimization of the sum of 
# the squared residuals between the model values for the response and 
# experimentally-derived values.

# Exponential decay using self-starting functions

# y(t) ~ yf + (y0 - yf)e-at

# The measured y value starts at y0 and decays towards yf at a rate of a

ggplot(data=new_data, aes(x=Distance, y=avg))+
  geom_point()

# 'nls' is the standard R base function to fit non-linear equations.
# However trying to fit exponential decay with nls doesn't work if you pick a 
# bad initial guess for the rate constant (a).

# A solution is to use a self-starting function - a special function for curve 
# fitting that guesses its own starting parameters. 
# The asymptotic regression function 'SSasymp' is equivalet to exponential decay.
# y and x in the formula are replaced by Count and Distance.
fit <- nls(avg~SSasymp(Distance, yf, y0, alpha), data=new_data)
fit
# Instead of fitting the rate constant directly, it searched for the logarithm 
# of a:
# y(t) ~ yf + (y0 - yf) * e ^ (-exp(a)) * t

# Extract the parameters
paras <- summary(fit)$coeff[,1]
paras

# alpha = exp(alpha)
exp(-4.317719)

# y(t) ~ -147.744063 + (34908.8 - -147.744063) * e ^ -0.013*t

fit
# The fact the regression converges to a solution suggests a good fit, but 
# that's not always the case. Nonlinear regression resolves parameter values by
# an iterative algorithm that converges onto the solution (a stable minimization 
# of residual error). Here, this took 5 cycles.

# Non-linear models do not have an intercept term, so most authors advocate 
# against the use of R-squared in nls analysis. 
# We can calculate pseudo-R-squared, but it cannot be interpreted as the 
# proportion of variance explained by the model.
aomisc::R2nls(fit)$PseudoR2
# 0.17 

# Graphically check the fitted curve
ggplot(data=augment(fit), aes(x=Distance, y=avg))+
  geom_point()+
  geom_line(aes(y=.fitted), col="red", size=1)+
  cc_theme()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# ----------------- Create new matrix from mortality values ---------------

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

# Quick visualisation
ggplot(data=metrics, aes(x=deg_out, y=deg_in))+
  geom_point()+
  geom_smooth()
# Looks like it could be linear

# Try a linear model
mod <- lm(deg_in~deg_out, data=metrics)
mod
# Regression equation: y = 6.324 + 0.493*x     OR     In degree = 6.324 + 0.493 * Out degree
par(mfrow = c(2,2))
plot(mod)
par(mfrow = c(1,1))
# Looks okay to me!

# Check the residuals for normality
shapiro.test(residuals(mod))
# P-value = 0.45
# Residuals are fine

# Summarise the model
summary(mod)
# RSE = 4.098 on 167 DoF
# Multiple R-squared = 0.3136
# Adjusted R-squared = 0.3095
# F-stat = 76.29
# P-value = 2.461e-15 
# In degree increases by 0.49 +- 0.06 for every 1 unit increase in out degree

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

# Plot as a map -----------------------------------------------------------

ocean_grid <- st_read(dsn=".", layer="ocean_grid")

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

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Community detection -----------------------------------------------------

# Convert to undirected and identify clusters
g2 <- as.undirected(g)
ceb <- cluster_fast_greedy(g2, weights=NULL)

# How many communities are there?
length(ceb)
# 15

# Plot in different styles --------------------------------------

# Load country shapefile
data(countriesHigh)
# Crop to area of interest
map_crop <- crop(countriesHigh, extent(-13, 13, 43, 65))

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
bridge2 <- bridge2 %>% tidyr::separate(nodes, c("Release_poly", "Settlement_poly"))
bridge2$`igraph::crossing(ceb, g)` <- NULL

nonbridge$nodes <- rownames(nonbridge)
nonbridge <- nonbridge %>% separate(nodes, c("Release_poly", "Settlement_poly"))
nonbridge$`igraph::crossing(ceb, g)` <- NULL

bridge2 <- merge(bridge2, myFreqs, by=c("Release_poly", "Settlement_poly"), all=FALSE)
(sum(bridge2$Count)/sum(myFreqs$Count))*100
# Bridges between communities account for 6.40% of all particle movement
# How much particle exchange occurs within clusters?
100-6.402047 # Particles exchanged within communities account for 93.60% of all particle movement
# what % of links occurred between clusters?
(nrow(bridge2)/sum(nrow(bridge)))*100

modularity(ceb)
# Modularity is 0.8503171

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
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

{ # Retrieve the number of nodes
  n_2000 <- filter(myFreqs_2000, !Count==0)
  n_2000 <- n_2000$Release_poly
  n_2000 <- n_2000[!duplicated(n_2000)]
  node[1,2] <- length(n_2000)
  # Retrieve the network density
  dens[1,2] <- ecount(g_2000)/(vcount(g_2000)*(vcount(g_2000)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2001 <- filter(myFreqs_2001, !Count==0)
  n_2001 <- n_2001$Release_poly
  n_2001 <- n_2001[!duplicated(n_2001)]
  node[2,2] <- length(n_2001)
  # Retrieve the network density
  dens[2,2] <- ecount(g_2001)/(vcount(g_2001)*(vcount(g_2001)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2002 <- filter(myFreqs_2002, !Count==0)
  n_2002 <- n_2002$Release_poly
  n_2002 <- n_2002[!duplicated(n_2002)]
  node[3,2] <- length(n_2002)
  # Retrieve the network density
  dens[3,2] <- ecount(g_2002)/(vcount(g_2002)*(vcount(g_2002)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2003 <- filter(myFreqs_2003, !Count==0)
  n_2003 <- n_2003$Release_poly
  n_2003 <- n_2003[!duplicated(n_2003)]
  node[4,2] <- length(n_2003)
  # Retrieve the network density
  dens[4,2] <- ecount(g_2003)/(vcount(g_2003)*(vcount(g_2003)-1)) # Directed network calculation
  
  # Retrieve the number of nodes
  n_2004 <- filter(myFreqs_2004, !Count==0)
  n_2004 <- n_2004$Release_poly
  n_2004 <- n_2004[!duplicated(n_2004)]
  node[5,2] <- length(n_2004)
  # Retrieve the network density
  dens[5,2] <- ecount(g_2004)/(vcount(g_2004)*(vcount(g_2004)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2005 <- filter(myFreqs_2005, !Count==0)
  n_2005 <- n_2005$Release_poly
  n_2005 <- n_2005[!duplicated(n_2005)]
  node[6,2] <- length(n_2005)
  # Retrieve the network density
  dens[6,2] <- ecount(g_2005)/(vcount(g_2005)*(vcount(g_2005)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2006 <- filter(myFreqs_2006, !Count==0)
  n_2006 <- n_2006$Release_poly
  n_2006 <- n_2006[!duplicated(n_2006)]
  node[7,2] <- length(n_2006)
  # Retrieve the network density
  dens[7,2] <- ecount(g_2006)/(vcount(g_2006)*(vcount(g_2006)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2007 <- filter(myFreqs_2007, !Count==0)
  n_2007 <- n_2007$Release_poly
  n_2007 <- n_2007[!duplicated(n_2007)]
  node[8,2] <- length(n_2007)
  # Retrieve the network density
  dens[8,2] <- ecount(g_2007)/(vcount(g_2007)*(vcount(g_2007)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2008 <- filter(myFreqs_2008, !Count==0)
  n_2008 <- n_2008$Release_poly
  n_2008 <- n_2008[!duplicated(n_2008)]
  node[9,2] <- length(n_2008)
  # Retrieve the network density
  dens[9,2] <- ecount(g_2008)/(vcount(g_2008)*(vcount(g_2008)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2009 <- filter(myFreqs_2009, !Count==0)
  n_2009 <- n_2009$Release_poly
  n_2009 <- n_2009[!duplicated(n_2009)]
  node[10,2] <- length(n_2009)
  # Retrieve the network density
  dens[10,2] <- ecount(g_2009)/(vcount(g_2009)*(vcount(g_2009)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2010 <- filter(myFreqs_2010, !Count==0)
  n_2010 <- n_2010$Release_poly
  n_2010 <- n_2010[!duplicated(n_2010)]
  node[11,2] <- length(n_2010)
  # Retrieve the network density
  dens[11,2] <- ecount(g_2010)/(vcount(g_2010)*(vcount(g_2010)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2011 <- filter(myFreqs_2011, !Count==0)
  n_2011 <- n_2011$Release_poly
  n_2011 <- n_2011[!duplicated(n_2011)]
  node[12,2] <- length(n_2011)
  # Retrieve the network density
  dens[12,2] <- ecount(g_2011)/(vcount(g_2011)*(vcount(g_2011)-1)) # Directed network calculation 
  
  # Retrieve the number of nodes
  n_2012 <- filter(myFreqs_2012, !Count==0)
  n_2012 <- n_2012$Release_poly
  n_2012 <- n_2012[!duplicated(n_2012)]
  node[13,2] <- length(n_2012)
  # Retrieve the network density
  dens[13,2] <- ecount(g_2012)/(vcount(g_2012)*(vcount(g_2012)-1)) # Directed network calculation 
}

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
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Calculate in and out degree ---------------------------------------------

{# Out degree
  deg_out <- degree(g_2000, v=V(g_2000), mode="out")
  out_deg_2000 <- as.data.frame(deg_out)
  out_deg_2000 <- filter(out_deg_2000, deg_out > 0) 
  colnames(out_deg_2000) <- "out_2000"
  # In degree
  deg_in <- degree(g_2000, v=V(g_2000), mode="in")
  in_deg_2000 <- as.data.frame(deg_in)
  colnames(in_deg_2000) <- "in_2000"
  
  # Out degree
  deg_out <- degree(g_2001, v=V(g_2001), mode="out")
  out_deg_2001 <- as.data.frame(deg_out)
  out_deg_2001 <- filter(out_deg_2001, deg_out > 0) 
  colnames(out_deg_2001) <- "out_2001"
  # In degree
  deg_in <- degree(g_2001, v=V(g_2001), mode="in")
  in_deg_2001 <- as.data.frame(deg_in)
  colnames(in_deg_2001) <- "in_2001"
  
  # Out degree
  deg_out <- degree(g_2002, v=V(g_2002), mode="out")
  out_deg_2002 <- as.data.frame(deg_out)
  out_deg_2002 <- filter(out_deg_2002, deg_out > 0) 
  colnames(out_deg_2002) <- "out_2002"
  # In degree
  deg_in <- degree(g_2002, v=V(g_2002), mode="in")
  in_deg_2002 <- as.data.frame(deg_in)
  colnames(in_deg_2002) <- "in_2002"
  
  # Out degree
  deg_out <- degree(g_2003, v=V(g_2003), mode="out")
  out_deg_2003 <- as.data.frame(deg_out)
  out_deg_2003 <- filter(out_deg_2003, deg_out > 0) 
  colnames(out_deg_2003) <- "out_2003"
  # In degree
  deg_in <- degree(g_2003, v=V(g_2003), mode="in")
  in_deg_2003 <- as.data.frame(deg_in)
  colnames(in_deg_2003) <- "in_2003"
  
  # Out degree
  deg_out <- degree(g_2004, v=V(g_2004), mode="out")
  out_deg_2004 <- as.data.frame(deg_out)
  out_deg_2004 <- filter(out_deg_2004, deg_out > 0) 
  colnames(out_deg_2004) <- "out_2004"
  # In degree
  deg_in <- degree(g_2004, v=V(g_2004), mode="in")
  in_deg_2004 <- as.data.frame(deg_in)
  colnames(in_deg_2004) <- "in_2004"
  
  # Out degree
  deg_out <- degree(g_2005, v=V(g_2005), mode="out")
  out_deg_2005 <- as.data.frame(deg_out)
  out_deg_2005 <- filter(out_deg_2005, deg_out > 0) 
  colnames(out_deg_2005) <- "out_2005"
  # In degree
  deg_in <- degree(g_2005, v=V(g_2005), mode="in")
  in_deg_2005 <- as.data.frame(deg_in)
  colnames(in_deg_2005) <- "in_2005"
  
  # Out degree
  deg_out <- degree(g_2006, v=V(g_2006), mode="out")
  out_deg_2006 <- as.data.frame(deg_out)
  out_deg_2006 <- filter(out_deg_2006, deg_out > 0) 
  colnames(out_deg_2006) <- "out_2006"
  # In degree
  deg_in <- degree(g_2006, v=V(g_2006), mode="in")
  in_deg_2006 <- as.data.frame(deg_in)
  colnames(in_deg_2006) <- "in_2006"
  
  # Out degree
  deg_out <- degree(g_2007, v=V(g_2007), mode="out")
  out_deg_2007 <- as.data.frame(deg_out)
  out_deg_2007 <- filter(out_deg_2007, deg_out > 0) 
  colnames(out_deg_2007) <- "out_2007"
  # In degree
  deg_in <- degree(g_2007, v=V(g_2007), mode="in")
  in_deg_2007 <- as.data.frame(deg_in)
  colnames(in_deg_2007) <- "in_2007"
  
  # Out degree
  deg_out <- degree(g_2008, v=V(g_2008), mode="out")
  out_deg_2008 <- as.data.frame(deg_out)
  out_deg_2008 <- filter(out_deg_2008, deg_out > 0) 
  colnames(out_deg_2008) <- "out_2008"
  # In degree
  deg_in <- degree(g_2008, v=V(g_2008), mode="in")
  in_deg_2008 <- as.data.frame(deg_in)
  colnames(in_deg_2008) <- "in_2008"
  
  # Out degree
  deg_out <- degree(g_2009, v=V(g_2009), mode="out")
  out_deg_2009 <- as.data.frame(deg_out)
  out_deg_2009 <- filter(out_deg_2009, deg_out > 0) 
  colnames(out_deg_2009) <- "out_2009"
  # In degree
  deg_in <- degree(g_2009, v=V(g_2009), mode="in")
  in_deg_2009 <- as.data.frame(deg_in)
  colnames(in_deg_2009) <- "in_2009"
  
  # Out degree
  deg_out <- degree(g_2010, v=V(g_2010), mode="out")
  out_deg_2010 <- as.data.frame(deg_out)
  out_deg_2010 <- filter(out_deg_2010, deg_out > 0) 
  colnames(out_deg_2010) <- "out_2010"
  # In degree
  deg_in <- degree(g_2010, v=V(g_2010), mode="in")
  in_deg_2010 <- as.data.frame(deg_in)
  colnames(in_deg_2010) <- "in_2010"
  
  # Out degree
  deg_out <- degree(g_2011, v=V(g_2011), mode="out")
  out_deg_2011 <- as.data.frame(deg_out)
  out_deg_2011 <- filter(out_deg_2011, deg_out > 0) 
  colnames(out_deg_2011) <- "out_2011"
  # In degree
  deg_in <- degree(g_2011, v=V(g_2011), mode="in")
  in_deg_2011 <- as.data.frame(deg_in)
  colnames(in_deg_2011) <- "in_2011"
  
  # Out degree
  deg_out <- degree(g_2012, v=V(g_2012), mode="out")
  out_deg_2012 <- as.data.frame(deg_out)
  out_deg_2012 <- filter(out_deg_2012, deg_out > 0) 
  colnames(out_deg_2012) <- "out_2012"
  # In degree
  deg_in <- degree(g_2012, v=V(g_2012), mode="in")
  in_deg_2012 <- as.data.frame(deg_in)
  colnames(in_deg_2012) <- "in_2012"
}

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

# Plot a correlation matrix of only those correlations that were significant
corrplot(M, p.mat=testRes$p, method="color", type="lower", addCoef.col="black",
         number.cex=0.6, diag=FALSE, tl.col="black", insig="blank", cl.ratio=0.2, tl.srt=45, sig.level=0.05,
         col.lim=c(0, 1), col=colorRampPalette(c("white", "#54b3e7ff", "white", "#f3202cff"))(200))

# Perceptually uniform colour palette
cols <- magma(200)
cols <- cols[200:100]

corrplot(M, p.mat=testRes$p, method="color", type="lower", addCoef.col="black",
         number.cex=0.6, diag=FALSE, tl.col="black", insig="blank", cl.ratio=0.2, tl.srt=45, sig.level=0.05,
         col.lim=c(0, 1), col=cols, is.corr=FALSE)

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Load the data -----------------------------------------------------------

# Load the data
{g2000 <- as.undirected(g_2000)
g2001 <- as.undirected(g_2001)
g2002 <- as.undirected(g_2002)
g2003 <- as.undirected(g_2003)
g2004 <- as.undirected(g_2004)
g2005 <- as.undirected(g_2005)
g2006 <- as.undirected(g_2006)
g2007 <- as.undirected(g_2007)
g2008 <- as.undirected(g_2008)
g2009 <- as.undirected(g_2009)
g2010 <- as.undirected(g_2010)
g2011 <- as.undirected(g_2011)
g2012 <- as.undirected(g_2012)
}

# Edit graphs to all have the same number of nodes ------------------------

# 2000 - 2010 and 2012 have fewer nodes than 2011
# Separate the nodes from each graph 
# Using the graph from 2011 to identify the missing nodes
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
v2005 <- cbind(igraph::as_data_frame(g2005, "vertices"))
v2005$Y_2005 <- "2005"
v2006 <- cbind(igraph::as_data_frame(g2006, "vertices"))
v2006$Y_2006 <- "2006"
v2007 <- cbind(igraph::as_data_frame(g2007, "vertices"))
v2007$Y_2007 <- "2007"
v2008 <- cbind(igraph::as_data_frame(g2008, "vertices"))
v2008$Y_2008 <- "2008"
v2009 <- cbind(igraph::as_data_frame(g2009, "vertices"))
v2009$Y_2009 <- "2009"
v2010 <- cbind(igraph::as_data_frame(g2010, "vertices"))
v2010$Y_2010 <- "2010"
v2011 <- cbind(igraph::as_data_frame(g2011, "vertices"))
v2011$Y_2011 <- "2011"
v2012 <- cbind(igraph::as_data_frame(g2012, "vertices"))
v2012$Y_2012 <- "2012"
}

# Merge them into one data frame, where if a node is not present in one year it 
# is filled with NA
{nodes <- merge(v2000, v2001, by="name", all=TRUE)
  nodes <- merge(nodes, v2002, by="name", all=TRUE)
  nodes <- merge(nodes, v2003, by="name", all=TRUE)
  nodes <- merge(nodes, v2004, by="name", all=TRUE)
  nodes <- merge(nodes, v2005, by="name", all=TRUE)
  nodes <- merge(nodes, v2006, by="name", all=TRUE)
  nodes <- merge(nodes, v2007, by="name", all=TRUE)
  nodes <- merge(nodes, v2008, by="name", all=TRUE)
  nodes <- merge(nodes, v2009, by="name", all=TRUE)
  nodes <- merge(nodes, v2010, by="name", all=TRUE)
  nodes <- merge(nodes, v2011, by="name", all=TRUE)
  nodes <- merge(nodes, v2012, by="name", all=TRUE)
}

# Add the missing nodes to each year
v2000 <- as.data.frame(nodes[, c(1,2)]) # Get the data for that year
colnames(v2000) <- c("name", "year") # Re-name the columns
v2000na <- v2000[rowSums(is.na(v2000))>0,] # Separate rows = NA
v2000na$year <- NULL # Remove the lat column
rownames(v2000na) <- v2000na$name # Assign the row names
g2000 <- add_vertices(g2000, 74, name=v2000na$name) # Add as vertices to the igraph for that year

# Do the same for 2001-2003
{v2001 <- as.data.frame(nodes[, c(1,3)])
  colnames(v2001) <- c("name", "year")
  v2001na <- v2001[rowSums(is.na(v2001))>0,]
  v2001na$year <- NULL
  rownames(v2001na) <- v2001na$name
  g2001 <- add_vertices(g2001, 41, name=v2001na$name)
  
  v2002 <- as.data.frame(nodes[, c(1,4)])
  colnames(v2002) <- c("name", "year")
  v2002na <- v2002[rowSums(is.na(v2002))>0,]
  v2002na$year <- NULL
  rownames(v2002na) <- v2002na$name
  g2002 <- add_vertices(g2002, 15, name=v2002na$name)
  
  v2003 <- as.data.frame(nodes[, c(1,5)])
  colnames(v2003) <- c("name", "year")
  v2003na <- v2003[rowSums(is.na(v2003))>0,]
  v2003na$year <- NULL
  rownames(v2003na) <- v2003na$name
  g2003 <- add_vertices(g2003, 2, name=v2003na$name)
  
  v2004 <- as.data.frame(nodes[, c(1,6)])
  colnames(v2004) <- c("name", "year")
  v2004na <- v2004[rowSums(is.na(v2004))>0,]
  v2004na$year <- NULL
  rownames(v2004na) <- v2004na$name
  g2004 <- add_vertices(g2004, 1, name=v2004na$name)
  
  v2005 <- as.data.frame(nodes[, c(1,7)])
  colnames(v2005) <- c("name", "year")
  v2005na <- v2005[rowSums(is.na(v2005))>0,]
  v2005na$year <- NULL
  rownames(v2005na) <- v2005na$name
  g2005 <- add_vertices(g2005, 1, name=v2005na$name)
  
  v2006 <- as.data.frame(nodes[, c(1,8)])
  colnames(v2006) <- c("name", "year")
  v2006na <- v2006[rowSums(is.na(v2006))>0,]
  v2006na$year <- NULL
  rownames(v2006na) <- v2006na$name
  g2006 <- add_vertices(g2006, 1, name=v2006na$name)
  
  v2007 <- as.data.frame(nodes[, c(1,9)])
  colnames(v2007) <- c("name", "year")
  v2007na <- v2007[rowSums(is.na(v2007))>0,]
  v2007na$year <- NULL
  rownames(v2007na) <- v2007na$name
  g2007 <- add_vertices(g2007, 2, name=v2007na$name)
  
  v2008 <- as.data.frame(nodes[, c(1,10)])
  colnames(v2008) <- c("name", "year")
  v2008na <- v2008[rowSums(is.na(v2008))>0,]
  v2008na$year <- NULL
  rownames(v2008na) <- v2008na$name
  g2008 <- add_vertices(g2008, 1, name=v2008na$name)
  
  v2009 <- as.data.frame(nodes[, c(1,11)])
  colnames(v2009) <- c("name", "year")
  v2009na <- v2009[rowSums(is.na(v2009))>0,]
  v2009na$year <- NULL
  rownames(v2009na) <- v2009na$name
  g2009 <- add_vertices(g2009, 1, name=v2009na$name)
  
  v2010 <- as.data.frame(nodes[, c(1,12)])
  colnames(v2010) <- c("name", "year")
  v2010na <- v2010[rowSums(is.na(v2010))>0,]
  v2010na$year <- NULL
  rownames(v2010na) <- v2010na$name
  g2010 <- add_vertices(g2010, 1, name=v2010na$name)
  
  v2012 <- as.data.frame(nodes[, c(1,14)])
  colnames(v2012) <- c("name", "year")
  v2012na <- v2012[rowSums(is.na(v2012))>0,]
  v2012na$year <- NULL
  rownames(v2012na) <- v2012na$name
  g2012 <- add_vertices(g2012, 1, name=v2012na$name)
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

# Perceptually uniform colour palette
cols <- magma(200)
cols <- cols[200:100]

corrplot(cors, method="color", type="lower", addCoef.col="black",
         number.cex=0.6, diag=FALSE, tl.col="black", cl.ratio=0.2, tl.srt=45,
         col.lim=c(0, 1), col=cols, is.corr=FALSE)
