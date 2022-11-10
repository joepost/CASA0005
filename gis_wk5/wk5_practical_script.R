# GIS Week 5 Practical 
# Mapping Airbnb listings in London Boroughs 
#
# Date: 2022-11-08


# 0. Load packages --------------------------------------------------------

library(janitor)
library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(here)


# 1. Load in data ---------------------------------------------------------

Londonborough <- st_read(here::here("Data",
                                    "statistical-gis-boundaries-london", 
                                    "ESRI", 
                                    "London_Borough_Excluding_MHW.shp"))%>%
  st_transform(., 27700)  #27700 = EPSG code for British National Grid


OSM <- st_read(here::here("Data",
                          "gis_osm_pois_a_free_1.shp")) %>%
  st_transform(., 27700) %>%
  #select hotels only
  filter(fclass == 'hotel')

Worldcities <- st_read(here::here("Data", 
                                  "World_Cities.shp")) %>%
  st_transform(., 27700)


# Read in the Airbnb .csv
Airbnb <- read_csv("Data/listings.csv") %>%
  # Transform into spatial data, using lat/long columns
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700) %>%
  # Filter to entire homes that are available all year
  filter(room_type == 'Entire home/apt' & availability_365 =='365')



# 2. Format and join data -------------------------------------------------

# Create join function
fnjoin <- function(layer1, layer2){
  output <- layer1 %>%
    st_join(layer2, .) %>%  #Note that st_join defaults to 'intersect' of layers
    # Shortcut of 'mutate' to create new column counting the unique values,
    #  grouped by 'GSS_CODE'
    add_count(., GSS_CODE, name = "hotels_in_borough") %>%
    group_by(., GSS_CODE, NAME) %>%  #Keeps both GSS_CODE and Borough NAME in output
    # Summarise to row per borough
    summarise(AccomCount = unique(hotels_in_borough))
  
  return(output)
}

# Use the function to join hotels to London Boroughs layer
Hotels <- fnjoin(OSM, Londonborough) 

# Repeat for Airbnb listings
Listings <- fnjoin(Airbnb, Londonborough) 


# 3. Map London boroughs --------------------------------------------------

tmap_mode("plot")

# Manually define breaks
breaks = c(0, 6, 11, 26, 51, 380) 

tm_hotels <- tm_shape(Hotels) +
  tm_polygons("AccomCount",
              breaks = breaks,
              # style = "jenks",
              palette = "PuBu") +
  tm_legend(show = FALSE) +
  tm_layout(frame=FALSE) +
  tm_credits("(a)", position=c(0,0.75), size=1.5)  
    #Add '(a)' for identification in faceted map

tm_listings <- tm_shape(Listings) +
  tm_polygons("AccomCount",
              breaks = breaks,
              # style = "jenks",
              palette = "PuBu") +
  tm_legend(show = FALSE) +
  tm_layout(frame=FALSE) +
  tm_credits("(b)", position=c(0,0.75), size=1.5)  
    #Add '(b)' for identification in faceted map

legend <- tm_shape(Hotels) +
  tm_polygons("AccomCount",
              palette="PuBu",
              breaks = breaks) +
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1,
            title = "Accommodation Count")+
  tm_credits("(c) OpenStreetMap contrbutors and Air b n b", position=c(0.0,0.0))

t <- tmap_arrange(tm_hotels, tm_listings, legend, ncol=2)
t
