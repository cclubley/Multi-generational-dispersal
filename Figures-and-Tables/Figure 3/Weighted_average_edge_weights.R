# -------------------------------------------------------------------------
# ---------------------- Weighted average edge weight ---------------------
# -------------------------------------------------------------------------
# Set the working directory ---------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/Data/Data/igraph environments")

# Load the required packages --------------------------------------------------

{ library(dplyr)
library(reshape2)
library(igraph)
library(raster)
library(ggplot2)
library(ccplot)
library(rnaturalearth)
library(wesanderson)
  library(plotrix)
}

# Load the connectivity environment --------------------------------------

# Load the data
load("./Combined/Combined_2000.RData")

# Re-calculate the number of connections between source and sink polygons for only 
# those where there *were* connections
myFreqs <- connectivity %>%  
  group_by(Release_poly, Settlement_poly) %>% 
  dplyr::summarise(Count = n()) 

# Save as that year
c_2000 <- myFreqs

# Re-do for all years
{load("./Combined/Combined_2001.RData")
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

# Calculate the weighted average of each edge -----------------------------

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

# Multiply the link weight by the number of years the link appears in to get the 
# weighted value
connections[3:15] <- connections[3:15]*connections$freq

# Now calculate the weighted average across years
connections$avg <- rowMeans(connections[3:15], na.rm=TRUE)

# Create the new myFreqs data frame 
myFreqs <- as.data.frame(connections[, c(1, 2, 17)])
colnames(myFreqs) <- c("Release_poly", "Settlement_poly", "Count")

# Write the data 
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(2) Dispersal Limitation/(4) Results/(1) Graph-level analysis/(Fig 3) Averaged link weight")
#write.csv(myFreqs, "./Weighted_average_graph.csv", row.names=FALSE)

# Load the previously saved data
myFreqs <- read.csv("./Weighted_average_graph.csv")

# Summarise the connectivity matrix (for all possible combinations) -----------

# Isolate the release polygons - make sure you are using the poly data from 2012
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

# Plotting the edges on a map ---------------------------------------------

# Convert the coordinates of the nodes into a matrix
coords$ID <- NULL
coords <- as.matrix(coords)

coords.df <- as.data.frame(coords)  ## convert the layout to a data.frame
coords.df$ID <- rownames(coords)  ## add in the polygon IDs

# remove local retention/self-recruitment
myFreqs_unique <- myFreqs[myFreqs$Release_poly != myFreqs$Settlement_poly, ] # Removes 172 rows
myFreqs_unique <- filter(myFreqs_unique, Count > 0) # Removes 27247 rows

# Create a separate data frame for only local retention/self-recruitment
myFreqs_sr <- myFreqs[myFreqs$Release_poly == myFreqs$Settlement_poly, ]
myFreqs_sr <- filter(myFreqs_sr, Count > 0) # Removes 9 rows

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

# PLot all links together
ggplot(data=world) +
  geom_sf(colour="black", fill="lightgrey")+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+ #Crop map to desired area
  geom_segment(data=myFreqs_unique, aes(x=from.x, xend=to.x, y=from.y, yend=to.y, 
                                        col=norm_count)) +
  scale_color_gradientn(colours=c("#54b3e7ff", "#cb0cbbff", "#f3202cff"), limits=c(0, 1))+
  geom_point(data=coords.df, aes(x=lon, y=lat), size=1.2, colour="black") +
  xlab("Longitude")+
  ylab("Latitude")+
  cc_theme()+
  theme(legend.position="right")

# Split the scale to values < 0.1 and >= 0.1  
summary(myFreqs_unique$Count)
std.error(myFreqs_unique$Count)
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
  scale_color_gradientn(colours=c("#54b3e7ff", "#cb0cbbff"), limits=c(0, 0.1))+
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
  scale_color_gradientn(colours=c("#cb0cbbff", "#f3202cff"), limits=c(0.1, 1))+
  geom_point(data=coords.df, aes(x=lon, y=lat), size=1.2, colour="black") +
  xlab("Longitude")+
  ylab("Latitude")+
  cc_theme()+
  theme(legend.position="right")

# With perceptually uniform colours ---------------------------------------

library(viridis)

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
std.error(myFreqs_unique$Count)
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
# self-recruitment accounted for 0.5509735% of connections

# Calculate percentage contribution of self recruitment to total edge weight
(sum(myFreqs_sr$Count)/sum(myFreqs$Count))*100
# Self-recruitment accounted for 25.69112% of edge weight

# Density of edges --------------------------------------------------------

# PLot a histogram of normalised link weight
ggplot(myFreqs_unique, aes(x=norm_count))+
  geom_histogram()+
  geom_vline(aes(xintercept=median(norm_count)),
             color="blue", linetype="dashed", size=1)+
  ylim(c(0, 1500))+
  xlab("Normalised Link weight")+
  ylab("Count")+
  cc_theme()

median(myFreqs_unique$norm_count)


