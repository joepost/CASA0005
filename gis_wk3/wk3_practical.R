# GIS Practical: Week 3
# Practice analysing raster data using world climate data
#
# Date: 2022-10-19


# 0. Load packages --------------------------------------------------------

library(sf)
library(raster)
library(fs)
library(tidyverse)
library(ggplot2)
library(plotly)


# 1. Load in data and check CRS ---------------------------------------------------------

# Check what layers are available in the GeoPkg
st_layers("Data/wk3_gadm36_AUS.gpkg")

# Read in the specified layer
AusBasemap <- st_read("Data/wk3_gadm36_AUS.gpkg", layer = "gadm36_AUS_0")

# Check the CRS of data
print(AusBasemap)

# Alternatively, get the proj4 string
st_crs(AusBasemap)$proj4string

# Try changing/transforming the CRS (e.g. to an Australian specific)
AusBasemap_projected <- AusBasemap %>%
  st_transform(3112)  #Where 3112 is the EPSG code for GDA94 (a local CRS)



# 2. Load in raster data --------------------------------------------------

# Read in single layer
wind_jan <- raster("Data/wc2.1_5m_wind_01.tif")
plot(wind_jan)

# Read in multiple layers together
# 2.1 Explore our directory 
dir_info("Data/")

# 2.2 Create a list of our files to read in 
filelist <- dir_info("Data/") %>%
  filter(str_detect(path, ".tif")) %>%  #Only include '.tif' files
  dplyr::select(path) %>%
  pull()  #Pulls out the selected column from a tibble/df into a vector

# 2.3 Stack the layers together
worldwindspeed <- filelist %>%
  stack()



# 3. Analyse raster data --------------------------------------------------

# Raster data cannot be used with dplyr verbs (yet... keep an eye on 'tidyterra' package)
# Therefore need to use base methods for manipulating/viewing data

# Subset the first layer of the stack
worldwindspeed[[1]]

# Rename the layers of our raster (without using 'rename' verb)
rasternames <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                 "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

names(worldwindspeed) <- rasternames

# Subset using new names 
worldwindspeed[["JAN"]]


# 3.1 Analyse with Australian locations --------------------------------------

# Create df of Australian sites (data provided by Andy in prac materials)
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
              "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
              "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)

#Put all of this information into one list 
samples <- data.frame(site, lon, lat, row.names="site")

# Extract the data from the Rasterstack for all points 
AUcitywind <- raster::extract(worldwindspeed, samples)

# Add the city names to the rows of AUcitywind
rownames(AUcitywind) <- site

# OR convert to tibble and create city column
AUcitywind_cln <- AUcitywind %>%
  as_tibble() %>%
  add_column(city = site, .before = "JAN")


# 3.2 Descriptive statistics ----------------------------------------------

# View descriptive statistics for wind speed in Darwin
wind_darwin <- AUcitywind_cln %>%
  filter(city == "Darwin")

# View a histogram of the data
hist(as.numeric(wind_darwin))

hist(as.numeric(wind_darwin),
     breaks = c(2.0, 2.4, 2.8, 3.2, 3.6, 4.0),
     col = "PowderBlue",
     main = "Histogram of average monthly wind speed, Darwin Australia",
     xlab = "Wind speed",
     ylab = "Frequency")

# Review the histogram information produced by R
histinfo <- hist(as.numeric(wind_darwin))


# 4. Visualising data -----------------------------------------------------

# Plot the AusBasemap geopackage
# First, use the simplify function to simplify the polygon
AusBasemap_simple <- AusBasemap %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()

plot(AusBasemap_simple)

# To combine raster data to our outline, need to confirm both are in the same CRS
# Check Aus Map
print(AusBasemap)

# Check world wind speed data
crs(worldwindspeed)

# Now crop the raster data to the 'extent' of our Aus boundaries
AusWind <- AusBasemap %>%
  crop(worldwindspeed, .)  #Note that the piped data (.) is the second argument here

plot(AusWind)
  #AusWind is now stored as a RasterBrick (as opposed to RasterStack), where all the layers
  # are contained within a single file

# Cut the extent further to the exact Aus boundary
exactAus <- AusWind %>%
  mask(., AusBasemap, na.rm = TRUE)


# Recompute the histogram for our Aus windspeed 
hist(exactAus[[2]],
     col = 'Goldenrod',
     main = "Wind speed in February")

# Alternatively, subset the raster using the layer name (instead of index)
hist(raster::subset(exactAus, "DEC"),
     col = 'Goldenrod',
     main = "Wind speed in December")



# 4.2 Visualising with ggplot ---------------------------------------------

# In order to use ggplot, raster needs to be transformed into a df or tibble
exactAus_df <- as.data.frame(exactAus)

gghist <- ggplot(exactAus_df,
                 aes(x=DEC)) +
  geom_histogram(color = "black",
                 fill = "white") +
  labs(title = "Wind speed in December",
       x = "Wind speed",
       y = "Frequency")
# Add a vertical line to histogram showing mean wind speed 
gghist + geom_vline(aes(xintercept=mean(DEC, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))


# Plot multiple months onto a single histogram

# First step, pivot_longer to combine the months into a single column
windspeed_longfull <- exactAus_df %>%
  pivot_longer(., 
               cols = c(1:12),
               names_to = "Month",
               values_to = "WindSpeed")

# Subset to 2 specific months
windspeed_long <- windspeed_longfull %>%
  filter(Month == "JAN" | Month == "JUL")

windspeed_means <- windspeed_long %>%
  # Calculate the mean for each month
  group_by(Month) %>%
  summarise(mean = mean(WindSpeed, na.rm = TRUE))

# Then create a ggplot of the two months distributions
ggplot(windspeed_long, 
       aes(x = WindSpeed, color = Month, fill = Month)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  geom_vline(data = windspeed_means,
             aes(xintercept = mean,
                 color = Month),
             linetype = "dashed") +
  labs(title = "Histogram of Australian windspeeds in January and July",
       x = "Wind speed",
       y = "Frequency") +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))  #Need to adjust the title after applying the theme


# 4.3. Faceted plots --------------------------------------------------------

data_complete_cases <- windspeed_longfull %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("JAN","FEB","MAR",
                                          "APR","MAY","JUN",
                                          "JUL","AUG","SEP",
                                          "OCT","NOV","DEC")))

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=WindSpeed, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian Wind Speeds", 
       x="Wind Speed",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))



# 4.4 Interactive visualisation using plotly ------------------------------

# First, split the 2-month data into each month
jan <- windspeed_long %>% 
  filter(Month == "JAN") %>%
  drop_na()
jul <-windspeed_long %>% 
  filter(Month == "JUL") %>%
  drop_na()

# Prepare axis titles
xlab <- list(title = "Wind Speed")
ylab <- list(title = "Frequency")

# Set the bin width
xbinsno <- list(start = 0, end = 7, size = 0.2)

# Plot the histogram
ihist <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$WindSpeed, xbins = xbinsno, name = "January") %>%
  add_histogram(x = jul$WindSpeed, xbins = xbinsno, name = "July") %>%
  layout(barmode = "overlay", xaxis = xlab, yaxis = ylab)

ihist
