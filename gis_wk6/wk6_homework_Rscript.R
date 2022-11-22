# Week 6 Practical
# Date: 2022-11-16
#
# Research Question:
#  For any given London Borough, are the Blue Plaques within that borough 
#  distributed randomly or do they exhibit some kind of dispersed or clustered pattern?


# 0. Load packages --------------------------------------------------------

library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(stringr)
library(tidyverse)
library(raster)
library(fpc)
library(dbscan)
library(ggplot2)

# 1. Load in data ---------------------------------------------------------

# London Borough Data
LondonBoroughs <- st_read(here::here("Data", 
                                     "statistical-gis-boundaries-london", 
                                     "ESRI", 
                                     "London_Borough_Excluding_MHW.shp")) %>%
  # Transform CRS to British National Grid
  st_transform(., 27700)

# Restrict just to Borough shapes (using GSS code prefix)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))

qtm(BoroughMap)

# Blue plaques data
blueplaques <- st_read(here::here("Data", "open-plaques-london-2018-04-08.geojson")) %>%
  st_transform(27700)

# OSM hotels data
osm_hotels <- st_read(here::here("Data", "gis_osm_pois_a_free_1.shp")) %>%
  st_transform(., 27700) %>%
  filter(class == 'hotel')

# Airbnb listings data
airbnb <- read_csv(here::here("Data", "listings.csv")) %>%
  # Convert to spatial
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = 4326) %>%
  st_transform(., 27700) %>%
  # Filter to entire homes available all year
  filter(room_type == "Entire home/apt" & availability_365 == '365')


# 2. Explore data ---------------------------------------------------------

# Quick map of blue plaques on London boroughs
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(blueplaques) +
  tm_dots(col = "blue")


# 3. Clean data -----------------------------------------------------------

# Remove duplicate plaque points
blueplaques <- distinct(blueplaques) 

# Subset to just the points within London
blueplaques_subset <- blueplaques[BoroughMap,]  #fn defaults to 'st_intersect'
      # However, can specify a different topological relation, e.g.
      #  blueplaques[BoroughMap, , st_within]

# Re-map with subsetted data
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(blueplaques_subset) +
  tm_dots(col = "blue") +
  tm_shape(osm_hotels) +
  tm_dots(col = "red", alpha = 0.5)



# 4. Spatial joins --------------------------------------------------------


# Create a join function

fn_join <- function(layer1, layer2){
  output <- layer1 %>%
    st_join(layer2, .) %>%
    add_count(GSS_CODE, name = "points_count") %>%
    #Group into a single row per borough
    group_by(GSS_CODE, NAME) %>%
    summarise(points_sum = unique(points_count))
  
  return(output)
}

# Use function for hotels
borough_hotels <- fn_join(osm_hotels, BoroughMap)

# And for Airbnb
borough_listings <- fn_join(airbnb, BoroughMap)

# Now join hotels and airbnb by borough into single sf object
borough_combined <- st_join(borough_hotels, borough_listings,
                            join = st_equals)

# Plot map to check 

#TO CHECK: How to plot bivariate choropleths in tmap?
tm_shape(borough_combined) +
  tm_polygons("points_sum.x") +
  tm_fill(col = "Grn", alpha = 0.5) +
  tm_shape(borough_combined) +
  tm_polygons("points_sum.y") +
  tm_fill(col = "Blues", alpha = 0.5)



# 5. Borough specific analysis --------------------------------------------

# Extract polygon for a single borough
Hackney <- BoroughMap %>%
  filter(., NAME == "Hackney")

# Subset blue plaques data to just those within Hackney boundaries
blueplaques_hackney <- blueplaques[Hackney,]

tm_shape(Hackney) +
   tm_polygons() +
  tm_shape(blueplaques_hackney) +
   tm_dots(col = "blue")

# Create spatial analysis window for use of 'spatstat' package
window <- as.owin(Hackney)
plot(window)

# Note that spatstat package uses its own types of spatial objects (not sf)
# Therefore have to convert to a point pattern (ppp) object
blueplaques_hackney <- blueplaques_hackney %>%
  # Converts to sp object
  as('Spatial')

# Convert sp to ppp object
ppp_hackney <- ppp(x = blueplaques_hackney@coords[,1],
                   y = blueplaques_hackney@coords[,2],
                   window = window)

# Plot the new ppp object
ppp_hackney %>%
  plot(.,pch=16,cex=0.5, 
       main="Blue Plaques Hackney")


# 5.1 Kernel Density Estimates --------------------------------------------

# Can create a simple KDE plot using the density function
ppp_hackney %>%
  density(., 
          sigma=500) %>%   # sigma = the diameter of the kernel
                           # (as we are in BNG, units are in metres)
  plot()



# 5.2 Ripley's K analysis ----------------------------------------------------

# Produce a Ripley's k plot
#  Red line (Kpois) = theoretical value of k at each distance
#  Black line (Kbord) = estimated value of k accounting for the effects of edge study area
K <- ppp_hackney %>%
  Kest(., correction="border") %>%
  plot()

Kval <- as.data.frame(Kest(ppp_hackney, correction = "Ripley"))



# 6. DBSCAN ---------------------------------------------------------------

#first extract the points from the spatial points data frame
dfp_hackney <- blueplaques_hackney %>%
  coordinates(.)%>%
  as.data.frame()

# Create a knee plot to determine most appropriate value for 'epsilon' (distance)
dfp_hackney%>%
  dbscan::kNNdistplot(.,k=4)

#now run the dbscan analysis
db <- dfp_hackney %>%
  fpc::dbscan(.,eps = 900, MinPts = 4)

db
db$cluster

#now plot the results
plot(db, blueplaques_hackney, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)

# Add the cluster identifier back onto the dataframe
dbscan_hackney <- dfp_hackney %>%
  mutate(dbcluster = db$cluster)


# 6.1 Plot the DBSCAN results in ggplot ------------------------------------

# Create convex hulls
chulls <- dbscan_hackney %>%  
  group_by(dbcluster) %>%
  mutate(hull = 1:n(),
         hull = factor(hull, chull(coords.x1, coords.x2))) %>%
  arrange(hull) %>%
  # Drop cluster 0 (not an actual cluster; includes all the leftover points)
  filter(dbcluster >= 1)

# Plot in ggplot
dbplot <- ggplot(data=dbscan_hackney, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()



