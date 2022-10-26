# GIS Homework: Week 3
# Produce descriptive statistics from raster data that show:
#   - the difference in maximum annual temperature for key cities
#   - between SSP1 and SSP5
#   - for the years 2081-2100
#   - Using any model and resolution
#
# Date: 2022-10-19


# 0. Load libraries -------------------------------------------------------

library(sf)
# library(raster)  #raster package is being retired in place of terra
library(terra)
library(here)
library(fs)
library(tidyverse)
library(ggplot2)
library(plotly)


# 1. Load in data ---------------------------------------------------------

# Check what layers are available in the GeoPkg
st_layers("Data/gadm41_FIN.gpkg")

# Read in the specified layer
Finland_map <- st_read("Data/gadm41_FIN.gpkg", layer = "ADM_ADM_0")

# Load in city data
cities <- read_sf("Data/World_Cities.shp") %>%
  #Filter to Finland
  filter(CNTRY_NAME == "Finland")

# Investigate CRS details for vector geographic data
st_crs(Finland_map)$IsGeographic   #Logical; returns whether CRS is geographic or projected
st_crs(Finland_map)$units_gdal   #Returns the CRS units (e.g. degrees, metres)
st_crs(Finland_map)$srid   #Returns the Spatial Reference Identifier (SRID)
st_crs(Finland_map)$proj4string   #Returns the proj4 string

# 2. Load in raster data --------------------------------------------------

# Read in multiple layers together
# 2.1 Explore our directory 
dir_info("Data/")

# 2.2 Create a list of our files to read in 
filelist <- dir_info("Data/") %>%
  filter(str_detect(path, "2100.tif")) %>%  #Only include the specific modelling files
  dplyr::select(path) %>%
  pull()  #Pulls out the selected column from a tibble/df into a vector

# 2.3 Stack the layers together
worldtmax <- filelist %>%
  terra::rast()  #Creates a 'SpatRaster' object 
worldtmax

# Investigate CRS details for raster geographic data
cat(terra::crs(worldtmax))   #Returns CRS details in Well Known Text (WKT) format
   #The 'crs()' function can also be used to set the CRS for a SpatRaster object
   #NOTE: the 'crs()' and 'st_crs()' functions do not alter coordinates’ values or geometries. 
   # Their role is only to set a metadata information about the object.
   # To re-project/transform data to a different CRS, use 'st_transform()'


# 3. Clean and format data --------------------------------------------------

# Name the layers appropriately
rastnames <- c("ssp1_JAN", "ssp1_FEB", "ssp1_MAR", "ssp1_APR", 
               "ssp1_MAY", "ssp1_JUN", "ssp1_JUL", "ssp1_AUG", 
               "ssp1_SEP", "ssp1_OCT", "ssp1_NOV", "ssp1_DEC",
               "ssp5_JAN", "ssp5_FEB", "ssp5_MAR", "ssp5_APR", 
               "ssp5_MAY", "ssp5_JUN", "ssp5_JUL", "ssp5_AUG", 
               "ssp5_SEP", "ssp5_OCT", "ssp5_NOV", "ssp5_DEC")

names(worldtmax) <- rastnames

# Extract the data from the SpatRaster using cities points
#  (i.e. draw out the raster value for the specified points - the cities)
FinCityTemps <- terra::extract(worldtmax, cities) %>%
  # Add column with city names
  as_tibble() %>%
  add_column(city = cities$CITY_NAME, .before = "ssp1_JAN")

# Format into tidy data
FinCT_tidy <- FinCityTemps %>%
  pivot_longer(cols = c(3:26),
               names_to = c("SSP", "month"),
               names_sep = "_",
               values_to = "tmax")

# Format into df with 2 SSP columns
FinDiffTemps <- FinCT_tidy %>%
  pivot_wider(names_from = SSP,
              values_from = tmax) %>%
  #Calculate temp difference column
  mutate(tdiff = round(ssp5 - ssp1,2))


# 4. Visualise the data  -----------------------------------------

# Plot the Finland map (simplified)
Finmap_simple <- Finland_map %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()

plot(Finmap_simple)

# Crop the raster data to the 'extent' of Finland boundaries
FinRaster <- Finland_map %>%
  terra::crop(worldtmax, .) %>%
  terra::mask(., Finland_map)

plot(FinRaster$ssp1_AUG, 
     decreasing = TRUE)

# Create a basic histogram of the temps across Finland
hist(FinRaster$ssp1_NOV,
     col = "Green",
     main = "Projected max temperature in November, Finland, SSP1")

hist(FinRaster$ssp5_NOV,
     col = "Orange",
     main = "Projected max temperature in November, Finland, SSP5")


# 4.2 Visualise with ggplot -----------------------------------------------

# Convert to df/tibble
FinRaster_df <- FinRaster %>%
  as.data.frame()

gghist <- ggplot(FinRaster_df,
                 aes(x=ssp1_NOV)) +
  geom_histogram(color = "black",
                 fill = "white") +
  labs(title = "Projected max temperature, November, SSP1",
       x = "Tmax",
       y = "Frequency")
# Add a vertical line to histogram showing mean tmax
gghist + geom_vline(aes(xintercept=mean(ssp1_NOV, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))


# Plot the different SSPs onto a single histogram

# Calculate the mean for each SSP
ssp_means <- FinCT_tidy %>%
  # Calculate the mean for each month
  group_by(SSP) %>%
  summarise(mean = mean(tmax, na.rm = TRUE))

# Then create a ggplot of the two SSPs distributions
ggplot(FinCT_tidy, 
       aes(x = tmax, color = SSP, fill = SSP)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  geom_vline(data = ssp_means,
             aes(xintercept = mean,
                 color = SSP),
             linetype = "dashed") +
  labs(title = "Histogram of projected max temperature in Finland, SSP1 and SSP5",
       x = "Max Temperature",
       y = "Frequency") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))  #Need to adjust the title after applying the theme

