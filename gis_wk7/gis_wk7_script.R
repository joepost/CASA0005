# GIS Week 7 Practical
#
# Date created: 2022-11-23


# 0. Load packages --------------------------------------------------------

library(here)
library(janitor)
library(sf)
library(tidyverse)
library(tmap)

# For spatial autocorrelation analysis
library(spdep)


# 1. Load data ------------------------------------------------------------

londonwards <- st_read(here::here("Data",
                                  "statistical-gis-boundaries-london", 
                                  "ESRI", 
                                  "London_Ward.shp")) %>%
  st_transform(., 27700)

londonwards_merged <- st_read(here::here("Data",
                                         "statistical-gis-boundaries-london",
                                         "ESRI", 
                                         "London_Ward_CityMerged.shp")) %>%
  st_transform(., 27700)

# Read ward data directly from URL
WardData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                     locale = locale(encoding = "latin1"),
                     na = c("NA", "n/a")) %>% 
  clean_names() 


# Join ward data to ward geometry 
londonwards_merged <- londonwards_merged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)


blueplaques <- st_read("../gis_wk6/Data/open-plaques-london-2018-04-08.geojson") %>%
  st_transform(.,27700)


# 2. Explore and clean data -----------------------------------------------

tmap_mode("plot")
tm_shape(londonwards_merged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(blueplaques) +
  tm_dots(col = "blue")

# Remove blue plaques outside London (as done in Week 6)
blueplaques_sub <- blueplaques[londonwards_merged,]


# Calculate points per ward
points_per_ward <- londonwards_merged %>%
  st_join(blueplaques_sub) %>%
  add_count(ward_name) %>%
  janitor::clean_names() %>%
  #calculate area
  mutate(area=st_area(.)) %>%
  #then density of the points per ward
  mutate(density=n/area) %>%
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)


# Plot the joined data to check polygon values
ward_summarised <- points_per_ward %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

tm_shape(ward_summarised) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")

