# Set working directory and load required packages ------------------------
{ library(sdmpredictors)
  library(rgdal)
  library(ggplot2)
  library(viridis)
  library(ccplot)
  library(dplyr)
  library(raster)
  library(RColorBrewer)
  library(rworldxtra)
  library(ggspatial)
}

# Load the grid ------------------------------------------------------------

# Load grid
ocean_grid <- readOGR(dsn=".", layer="ocean_grid")

# Plot to check
plot(ocean_grid)

# Remove the unwanted grid cells (outside domain)
# Annoyingly you can only on one at a time...
ocean_grid1 <- ocean_grid[grepl("416", ocean_grid$ID),]
ocean_grid2 <- ocean_grid[grepl("507", ocean_grid$ID),]
ocean_grid3 <- ocean_grid[grepl("508", ocean_grid$ID),]
ocean_grid4 <- ocean_grid[grepl("603", ocean_grid$ID),]
ocean_grid5 <- ocean_grid[grepl("737", ocean_grid$ID),]
ocean_grid6 <- ocean_grid[grepl("846", ocean_grid$ID),]
ocean_grid7 <- ocean_grid[grepl("940", ocean_grid$ID),]
ocean_grid8 <- ocean_grid[grepl("981", ocean_grid$ID),]

new_grid <- rbind(ocean_grid1, ocean_grid2, ocean_grid3, ocean_grid4, ocean_grid5, ocean_grid6, ocean_grid7, ocean_grid8)

# Plot to check
plot(new_grid, col="blue")

# Convert the settlement grid to spatial lines
grid <- as(new_grid, 'SpatialLinesDataFrame')
class(grid)
plot(grid)

library(rnaturalearth)

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "large", returnclass = "sf")

ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-13, 13), ylim=c(43, 65), expand=FALSE)+
  cc_theme()+
  geom_path(grid, mapping=aes(x=long, y=lat, group=group), col="red", fill="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  annotation_scale(location = "tl", line_width = 0.5, 
                   bar_cols=c("black", "dark grey"))+
  annotation_north_arrow(location="tl", which_north="true",
                         pad_x=unit(0.4, "in"), pad_y=unit(0.4, "in"))

