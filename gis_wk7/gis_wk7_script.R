# GIS Week 7 Practical
#
# Date created: 2022-11-23


# 0. Load packages --------------------------------------------------------

library(here)
library(janitor)
library(sf)
library(tidyverse)
library(tmap)
library(RColorBrewer)

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


# 3. Spatial Autocorrelation Calculations ---------------------------------

# Step 1: Calculate the centroids for all polygons

ward_centroids <- ward_summarised %>%
  st_centroid() %>%
  st_geometry()

plot(ward_centroids, axes = TRUE)

# Step 2: Generate list of ward neighbours
ward_neighbours <- ward_summarised %>%
  poly2nb(., queen = TRUE) #'Queen case' takes neighbours in all directions (think queen on a chess board)

summary(ward_neighbours)

# Plot the neighbours
plot(ward_neighbours, st_geometry(ward_centroids), col = "red")
# Add a map underneath
plot(ward_summarised$geometry, add = TRUE)

# Step 3: Generate a spatial weights matrix
# Creates a matrix the size of the number of neighbourhoods, 
#  with values indicating if elements are a neighbour or not.

weight_matrix <- ward_neighbours %>%
  nb2mat(., style = "B")  # B = binary coding

sum(weight_matrix) #Sums the binary elements to calculate total count of neighbours
                   # (important if using global standardisation)

sum(weight_matrix[1,]) #Sums the binary elements in a row (this example = row 1)
                       # (important if performing row standardisation)

# Step 3.2: Generate a list of spatial weights
# Necessary format to input into Moran's I function

weight_list <- ward_neighbours %>%
  nb2listw(., style = "C")  # C = globally standardised ('W' for row)

# Step 4: Calculate Moran's I

MoranI_globaldensity <- ward_summarised %>%
  pull(density) %>%  #'pull' is just a way to draw on a single column (similar to '$' but within pipes)
  as.vector() %>%  # Converts the 'density' column into a vector object
  #This vector is then input into the moran.test function
  moran.test(., weight_list)

MoranI_globaldensity

# Step 5: Calculate Geary's C

GearyC_globaldensity <- ward_summarised %>%
  pull(density) %>%
  as.vector() %>%
  geary.test(., weight_list)

GearyC_globaldensity

# Step 6: Calculate Getis-Ord

GetisOrd_globaldensity <- ward_summarised %>%
  pull(density) %>%
  as.vector() %>%
  globalG.test(., weight_list)

GetisOrd_globaldensity


# 4. Local autocorrelation calculations -----------------------------------

# Local Moran's I
MoranI_localcount <- ward_summarised %>%
  pull(plaquecount) %>%  # Use count instead of density because calculating wards individually
  as.vector() %>%
  localmoran(., weight_list) %>%
  as_tibble()  # End output = table with row for each polygon (ward)

MoranI_localcount

MoranI_localdensity <- ward_summarised %>%
  pull(density) %>% 
  as.vector() %>%
  localmoran(., weight_list) %>%
  as_tibble()  

# GI*
GIstar_localdensity <- ward_summarised %>%
  pull(density) %>%
  as.vector() %>%
  localG(., weight_list)


# Copy the output data back into the spatial dataframe, so that we map the results
ward_summarised <- ward_summarised %>%
  mutate(I_plaquecount = as.numeric(MoranI_localcount$Ii),
         Iz_plaquecount = as.numeric(MoranI_localcount$Z.Ii),
         I_density = as.numeric(MoranI_localcount$Ii),
         Iz_density = as.numeric(MoranI_localcount$Z.Ii),
         GIstar_density = as.numeric(GIstar_localdensity))

# Map the Moran's I statistic

# Set breaks
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

MoranColours<- rev(brewer.pal(8, "RdGy"))

tm_shape(ward_summarised) +
  tm_polygons("Iz_plaquecount",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")


# Map the local Getis Ord (GI*)

GIColours<- rev(brewer.pal(8, "RdBu"))

tm_shape(ward_summarised) +
  tm_polygons("GIstar_density",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")

