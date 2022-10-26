# Homework Week 2
# Task: You need calculate the average percent of science students (in all grades) 
#   per county meeting the required standards and 
#   produce a map to show where the Country averages are 
#   above or below the State of Washington average.

# Date authored: 2022-10-13


# 0. Load required Libraries ----------------------------------------------

library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(janitor)


# 1. Load in Data ---------------------------------------------------------

# First data source: Washington counties GeoJSON
WashingtonGeoJSON <- st_read("./../Data/wk2_Washington_Counties_with_Natural_Shoreline___washsh_area.geojson")

# Second data source: Washington school report results .csv
school_data <- read_csv(file = "./../Data/wk2_Report_Card_Assessment_Data_2018-19_School_Year.csv",
                        na = c("NULL"),
                        #Specify data type for each column, using col_types list
                        #Each column must be specified in this context
                        #Shortcut: use spec() to produce column list after running the original import
                        col_types =  cols(
                          SchoolYear = col_character(),
                          OrganizationLevel = col_character(),
                          County = col_character(),
                          ESDName = col_character(),
                          ESDOrganizationID = col_character(),
                          DistrictCode = col_character(),
                          DistrictName = col_character(),
                          DistrictOrganizationId = col_character(),
                          SchoolCode = col_character(),
                          SchoolName = col_character(),
                          SchoolOrganizationid = col_character(),
                          CurrentSchoolType = col_character(),
                          StudentGroupType = col_character(),
                          StudentGroup = col_character(),
                          GradeLevel = col_character(),
                          `Test Administration (group)` = col_character(),
                          TestAdministration = col_character(),
                          TestSubject = col_character(),
                          Suppression = col_character(),
                          `Count of Students Expected to Test` = col_double(),
                          `Count of students expected to test including previously passed` = col_double(),
                          CountMetStandard = col_double(),
                          PercentMetStandard = col_number(),
                          PercentLevel1 = col_double(),
                          PercentLevel2 = col_double(),
                          PercentLevel3 = col_double(),
                          PercentLevel4 = col_double(),
                          PercentMetTestedOnly = col_double(),
                          `Percent No Score` = col_double(),
                          DataAsOf = col_character()
                        )) %>%
  clean_names()



# 2. Clean and format data ------------------------------------------------

school_metstd <- school_data %>%
  #Select only the required columns
  select(school_year, organization_level, county,
         esd_organization_id, district_code, school_code,
         student_group_type, student_group,
         grade_level, test_subject, test_administration_group, suppression,
         count_of_students_expected_to_test, count_met_standard, percent_met_standard) %>%
  #Filter to science students (all grades)
  filter(test_subject == "Science",
         student_group == "All Students",
         grade_level == "All Grades",
         #Filter just to WCAS (Science) testing
         #See data source website for metadata explaining administration group
           #url: https://data.wa.gov/education/Report-Card-Assessment-Data-2018-19-School-Year/5y3z-mgxd
         test_administration_group == "WCAS",
         #Filter out rows with suppressed data
         suppression == "None",
         #Remove 'Multiple' county
         county != "Multiple",
         #Filter Organisation = School to remove summarised district counts
         organization_level == "School"
         ) %>%
  #Ensure only 1 row per district
  distinct(district_code, .keep_all = TRUE)

# Check to make sure there are no duplicates of school code
# test <- school_metstd %>%
#   group_by(school_code) %>%
#   summarise(count = n()) %>%
#   filter(count > 1)

countyav <- school_metstd %>%
  group_by(county) %>%
  summarise(av_metstd = mean(percent_met_standard),
            total_metstd = sum(count_met_standard, 
                               #Remove NA (suppressed low student counts) to calc approx. student sum
                               na.rm = TRUE))


# Calculate the state average
stateav <- school_data %>%
  filter(organization_level == "State",
         student_group_type == "All",
         grade_level == "All Grades",
         test_subject == "Science",
         test_administration_group == "WCAS") 
stateav_val <- stateav[[1,23]]

# Create new column comparing counties to state average
comparisondata <- countyav %>% 
  mutate(diff_to_state = round(stateav_val - av_metstd,1),
         diff_class = case_when(diff_to_state < -2 ~ "Below state average",
                                diff_to_state > 2 ~ "Above state average",
                                TRUE ~ "Equal to state average")) 


# 2.2 Calculate using numeric counts (not percentage) ---------------------

countysum <- school_metstd %>%
  group_by(county) %>%
  summarise(sum_met = sum(count_met_standard),
            sum_tested = sum(count_of_students_expected_to_test)) %>%
  mutate(percent_met = sum_met/sum_tested*100,
         diff_to_state = round(stateav_val - percent_met,1),
         diff_class = case_when(diff_to_state < -5 ~ "Well below state average",
                                diff_to_state < -1 ~ "Below state average",
                                diff_to_state > 5 ~ "Well above state average",
                                diff_to_state > 1 ~ "Above state average",
                                TRUE ~ "Equal to state average"))

countysum$diff_class <- factor(countysum$diff_class, levels = c("Well below state average",
                                                                "Below state average",
                                                                "Equal to state average",
                                                                "Above state average",
                                                                "Well above state average"))


# 3. Create Map -----------------------------------------------------------

# Join countyav to GeoJSON
map_countyav_data <- WashingtonGeoJSON %>%
  left_join(comparisondata, by = c("COUNTYLABEL" = "county"))

map_countysum_data <- WashingtonGeoJSON %>%
  left_join(countysum, by = c("COUNTYLABEL" = "county"))

# Create tmap of data
tmap_mode("plot")

map_countyav <- tm_shape(map_countyav_data) +
  tm_polygons("av_metstd",
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Average per cent of students who met standard") +
  # tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = "Science students that met test standards by county\nWashington State 2018-19",
            main.title.position = "center",
            main.title.size = 1,
            legend.position = c("right", "bottom"),
            legend.outside = TRUE,
            legend.title.size = 0.5,
            frame = FALSE)
  
map_countyav

# Create map comparing counties to state average

map_comparison <- tm_shape(map_countysum_data) +
  tm_polygons("diff_class",
              style="pretty",
              palette="RdYlBu",
              contrast = c(0,0.5),
              midpoint=NA,
              title="") +
  tm_layout(main.title = "Per cent of science students that met standard compared to state average\nWashington State 2018-19",
            main.title.position = "center",
            main.title.size = 1,
            legend.outside.position = "right",
            legend.outside = TRUE,
            legend.title.size = 0.5,
            frame = FALSE)

map_comparison





