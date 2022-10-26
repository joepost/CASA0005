# GIS Week 2 Practical: Learning R

# See working directory 
getwd()

# Basic functions: create and plot dataset
Data1 <- c(1:100)
Data2 <- c(101:200)

plot(Data1, Data2, col="red")

Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)

plot(Data3, Data4, col="blue")

# Use tidyverse to manipulate data
library(tidyverse)

df <- data.frame(Data1, Data2, Data3, Data4)
df %>% head(10)

# rename columns
df <- df %>% rename(Sequence = Data1,
                    HighSeq = Data2,
                    LowNormDist = Data3,
                    HighNormDist = Data4)


# Subsetting columns without dplyr (necessary for raster data)
# Method 1: '$' symbol
df$Sequence

# Method 2: Double square brackets
df[["HighSeq"]]

# Read in data
# Use './../' notation to go back one folder in directory, to then navigate to the 'Data' folder
wards <- read_csv("./../Data/wk2_ward-profiles-excel-version.csv")

# Read in data using 'here' package
library(here)
here::here()
  # Need to investigate how to use 'here' to go up a folder in directory
  # (i.e. to access central 'Data' folder)

# Reading in data using the 'readr' (tidyverse) package
# Read the data straight from web using url 
LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")  #Tells R to read "n/a" as na cells (read string as missing)

# Review the input data using 'summarise_all'
Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(),   #Formats the classes into a tidy df to inspect
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

# Filter the LondonData to just include boroughs (coded 'E09')
LondonBoroughs<- LondonData %>% 
  #Note use of back quotes (``) to name the New Code var
  filter(str_detect(`New code`, "^E09")) %>%
  distinct() 

LondonBoroughs$`Ward name`

# Use janitor package to clean up column names
library(janitor)

LondonBoroughs <- LondonBoroughs %>%
  rename(Borough = `Ward name`) %>%
  clean_names() #Removes upper case and replaces space with underscore

uklifexpectancy <- 81.16

LondonCalcs <- LondonBoroughs %>%
  mutate(av_life_expectancy = (male_life_expectancy_2009_13 + female_life_expectancy_2009_13)/2,
         #Create a column for normalised life expectancy
         normalised_life_expectancy = av_life_expectancy/mean(av_life_expectancy),
         #Use case_when to class as above or below the UK average life expectancy
         class_life_expectancy = case_when(av_life_expectancy > uklifexpectancy ~ "Above average",
                                           av_life_expectancy < uklifexpectancy ~ "Below average",
                                           TRUE ~ "Equal to average")
         ) %>%
  select(new_code, borough, av_life_expectancy, normalised_life_expectancy, class_life_expectancy)

LondonCalcs_grp <- LondonCalcs %>%
  mutate(life_exp_diff = av_life_expectancy - uklifexpectancy) %>%
  group_by(class_life_expectancy) %>%
  summarise(range = max(life_exp_diff) - min(life_exp_diff), count = n(), av = mean(life_exp_diff))

# Use the across function to round the column values
LondonCalcs2 <- LondonCalcs %>%
  #Create column of difference from UK life expectancy, rounded to integer
  mutate(life_exp_diff = round(av_life_expectancy - uklifexpectancy, 0)) %>%
  #Round all other numeric columns to 3 digits
  mutate(across(where(is.numeric), round, 3)) %>%
  #Create a column classifying above/below/equal, and stating the difference
  mutate(class_life_expectancy = case_when(life_exp_diff > 0 ~ str_c("Above UK average by",
                                                                     life_exp_diff, "years",
                                                                     sep = " "),
                                           life_exp_diff < 0 ~ str_c("Below UK average by",
                                                                     life_exp_diff, "years",
                                                                     sep = " "),
                                           TRUE ~ "Equal to UK average"))

LondonCalcs2_grp <- LondonCalcs2 %>% 
  group_by(class_life_expectancy) %>%
  summarise(count = n())
LondonCalcs2_grp


# Try plotting the data
plot(LondonBoroughs$general_fertility_rate_2013,
     LondonBoroughs$percent_not_born_in_uk_2011)

library(plotly)
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~general_fertility_rate_2013, 
        #data for y axis
        y = ~percent_not_born_in_uk_2011, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")


# Spatial Data Analysis: Producing Maps -----------------------------------

library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(OpenStreetMap)

# Download data file directly from the internet
# EW <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson")


# Alternatively load saved file from directory
EW <- st_read("./../Data/wk2_Local_Authority_Districts_(December_2015)_Boundaries.geojson")

# Filter just to London boroughs and inspect map
EW_London <- EW %>%
  filter(str_detect(lad15cd, "^E09"))
qtm(EW_London)


# Merge map file with attribute data 
LondonData <- clean_names(LondonData)

BoroughDataMap <- EW %>%
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09")) %>%  #Filter to London Boroughs
  left_join(.,
        LondonData, 
        by = c("lad15cd" = "new_code"),
        no.dups = TRUE) %>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)

# Load a basemap using Open Street Map
tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

# Create tmap plot
tmap_mode("plot")

BoroughMap <- tm_shape(tmaplondon) +
  #RGB = raster function; takes an 'image' of the OSM vector data to use as basemap
  tm_rgb() +
  #Use second 'tm_shape' argument to add vector data over the basemap
  tm_shape(BoroughDataMap) +
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
        style="jenks",
        palette="YlOrBr",
        midpoint=NA,
        title="Rate per 1,000 people",
        alpha = 0.5) +
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))
BoroughMap


# Create map of London Boroughs using Life Expectancy Data
LifeExpData <- EW %>%
  filter(str_detect(lad15cd, "^E09")) %>%  #Filter to London Boroughs
  left_join(.,
            LondonCalcs2, 
            by = c("lad15cd" = "new_code"),
            no.dups = TRUE) %>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)

LifeExpMap <- tm_shape(tmaplondon) +
  tm_rgb() +
  tm_shape(LifeExpData) +
  tm_polygons("life_exp_diff", 
              style="jenks",
              palette="PRGn",
              midpoint=NA,
              title="Difference from UK average life expectancy (years)",
              alpha = 0.8) +
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in Life Expectancy", legend.position = c("right", "bottom"))
LifeExpMap
