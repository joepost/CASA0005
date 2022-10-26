# GIS Week 3: Practice Script for CRS transformation
#
# Examples from 'Geocomputation with R', Lovelace et al. 2019, Ch.7
# URL: https://geocompr.robinlovelace.net/reproj-geo-data.html 

library(spData)

# Review CRS for an example dataset (taken from spData)
st_crs(cycle_hire_osm)

# Transform dataset from WGS84 to a projected CRS (the British National Grid)
cycle_hire_osm_projected <- st_transform(cycle_hire_osm, "EPSG:27700")
st_crs(cycle_hire_osm_projected)

# Query the details of an EPSG code 
st_crs("EPSG:27700")$Name

