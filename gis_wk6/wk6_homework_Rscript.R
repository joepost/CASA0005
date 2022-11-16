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


# 1. Load in data ---------------------------------------------------------

# London Borough Data
LondonBoroughs <- st_read(here::here("Data", "statistical-gis-boundaries-london", "ESRI", "London_Borough_Excluding_MHW.shp"))


BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09")) %>%
  st_transform(., 27700)

qtm(BoroughMap)

# Blue plaques data
blueplaques <- st_read(here::here("Data", "open-plaques-london-2018-04-08.geojson")) %>%
  st_transform(27700)



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






