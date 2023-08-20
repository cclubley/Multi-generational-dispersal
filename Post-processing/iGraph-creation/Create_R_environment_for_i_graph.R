# -------------------------------------------------------------------------
# ---------------- Creating and saving igraph environments ----------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# As the data I am working with is so large, I decided to create and save R 
# environments containing all f the data I need to be able to do network 
# analysis. This way I only have to open the connectivity .csv file (which is 
# very large) once instead of having to open it every time. I created one R
# environment for each PLD scenario for each year. The code in this script shows
# how to do so for one year.

# Set the working directory ---------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/Results/Post-processing/Connectivity")

# Load the required packages --------------------------------------------------
{ library(dplyr)
  library(reshape2)
  library(igraph)
  library(rworldxtra)
  library(raster)
  library(maps)
}

# Load the connectivity data --------------------------------------------------

# Load the release file that was generated for the following year, which should contain all of the polygons and their IDs
centroids <- read.csv(file.choose(), header=TRUE) 
# Should have the same number of observations as the dimension of s.dat (later in script)
# Give the row names the same values as the polygon ID
rownames(centroids) <- centroids[,3]
# Remove polygon ID as a variable
centroids <- centroids[,1:2] 

# Load the connectivity file
connectivity <- read.csv(file.choose(), header=T)
#head(connectivity)

# Re-structure the columns
connectivity <- connectivity[, c(5, 10, 8, 3, 2, 1, 7, 6, 4, 9)]
# Re-name the columns
colnames(connectivity) <- c("Release_poly","Settlement_poly","Settlement_date", 
                            "Release_date", "Source_lon", "Source_lat", 
                            "Sink_lon", "Sink_lat", "Particle", "Distance")

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
save.image(file='./Data/(1) iGraph environments/28 PLD/PLD28_2012.RData')
