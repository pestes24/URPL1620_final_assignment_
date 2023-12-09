library(tidyverse)
library(janitor)
library(tidycensus)
library(stringr) 
library(lubridate)
library(urbnthemes) 
set_urbn_defaults(style = "print")

#installed census api 
#census_api_key("cd05290c85440e7e445f0900d13bb133bfdfa6ad", install = TRUE)

variables_2022 <- load_variables(
  year = 2022, 
  dataset = "acs1", 
  cache = TRUE)

#pulling and cleaning data for full northern california megaregion (21 total)
#http://www.bayareaeconomy.org/report/the-northern-california-megaregion/ 
#https://www.spur.org/news/2018-06-19/where-exactly-bay-area


ncal_counties_2022 <- get_acs( 
  #pulling northern california megaregion counties, 21 total
  geography = "county",
  variables = c(
    agg_commute_all_types = "B08136_001",
    commute_car = "B08136_002",
    commute_pt = "B08136_007",
    commute_pt_bus = "B08136_008",
    commute_pt_shortrail = "B08136_009",
    commute_pt_longtrain = "B08136_010",
    commute_average = "B08303_001", #denominator
    commute_60to89 = "B08303_012",
    commute_over_90 = "B08303_013",
    income_percap = "B19301_001", #per Capita Income in the Past 12 Months (2022 Inflation-Adjusted $)
    income_median = "B19013_001", #Median Household Income in the Past 12 Months (2022 Inflation-Adjusted $)
    poverty_level = "B17009_002", 
    rentburd_median = "B25071_001", #Median gross rent as a percentage of household income 
    #re-read what this is
    rentburd_total = "B25070_001",  # denominator for rent burden (gross rent as a percentage of household income)
    #check what this denominator is -- % ?  or n count? 
    rentburd_30_35 = "B25070_007", # rent burden 30.0 to 34.9 percent 
    rentburd_35_40 = "B25070_008",  # rent burden 35.0 to 39.9 percent
    rentburd_40_50 = "B25070_009",  # rent burden 40.0 to 49.9 percent
    rentburd_over_50 = "B25070_010",  # rent burden 50.0 percent or more
    rentburd_subtract = "B25070_011",  # not computed; subtract from denominator
    total_pop = "B01001_001" #total population, from sex by age
  ),
  state = "California",
  year = 2022,
  survey = "acs1",
  output = "wide",
  ) %>% 
  filter(
    #San Francisco Bay Area  
    NAME == "San Francisco County, California" |
      NAME == "Alameda County, California" |
      NAME == "Contra Costa County, California" |
      NAME == "Marin County, California" |
      NAME == "Napa County, California" |
      NAME == "Solano County, California" |
      NAME == "Sonoma County, California" |
      NAME == "San Mateo County, California" |
      NAME == "Santa Clara County, California" |
      #Sacramento Area
      NAME == "Sacramento County, California" |
      NAME == "Yolo County, California" |
      NAME == "Yuba County, California" |
      NAME == "Sutter County, California" |
      NAME == "Placer County, California" |
      NAME == "El Dorado County, California" |
      #Northern San Joaquin Valley
      NAME == "San Joaquin County, California" |
      NAME == "Stanislaus County, California" |
      NAME == "Merced County, California" |
      #Monterey Bay Area
      NAME == "San Benito County, California" |
      NAME == "Monterey County, California" |
      NAME == "Santa Cruz County, California"
  )

counties_norcal_megaregion <- 
  "San Francisco|Contra Costa|Marin|Napa|Solano|Sonoma|San Mateo|Santa Clara|Sacramento|Yolo|Yuba|Sutter|Placer|El Dorado|San Joaquin|Stanislaus|Merced|San Benito|Monterey|Santa Cruz"

#filter for the specific pumas in norcal megaregion
ncal_pumas_2022 <- get_acs(
  geography = "public use microdata area",
  variables = c(
    agg_commute_all_types = "B08136_001", #too many PUMAs with NA to run for all
    commute_car = "B08136_002", #too many PUMAs with NA to run for all
    commute_pt = "B08136_007", #too many PUMAs with NA to run for all
    commute_pt_bus = "B08136_008", #too many PUMAs with NA to run for all
    commute_pt_shortrail = "B08136_009", #too many PUMAs with NA to run for all
    commute_pt_longtrain = "B08136_010", #too many PUMAs with NA to run for all
    commute_average = "B08303_001", #denominator
    commute_60to89 = "B08303_012",
    commute_over_90 = "B08303_013",
    income_percap = "B19301_001", #per Capita Income in the Past 12 Months (2022 Inflation-Adjusted $)
    income_median = "B19013_001", #Median Household Income in the Past 12 Months (2022 Inflation-Adjusted $)
    poverty_level = "B17009_002", 
    rentburd_median = "B25071_001", #Median gross rent as a percentage of household income 
    #re-read what this is
    rentburd_total = "B25070_001",  # denominator for rent burden (gross rent as a percentage of household income)
    #check what this denominator is -- %? n count? 
    rentburd_30_35 = "B25070_007", # rent burden 30.0 to 34.9 percent 
    rentburd_35_40 = "B25070_008",  # rent burden 35.0 to 39.9 percent
    rentburd_40_50 = "B25070_009",  # rent burden 40.0 to 49.9 percent
    rentburd_over_50 = "B25070_010",  # rent burden 50.0 percent or more
    rentburd_subtract = "B25070_011",  # not computed; subtract from denominator
    total_pop = "B01001_001" #total population, from sex by age
  ),
  state = "California",
  year = 2022,
  survey = "acs1",
  output = "wide") %>% 
  filter(str_detect(NAME, counties_norcal_megaregion)) %>% 
  filter(!str_detect(NAME, "Los Angeles")) 
   #^because of "Marin" the filter above was pulling two LA counties with "Marina"


#now re-running with copied code from above but pulling pre-pandemic data
#acs 5-year 2015-2019

ncal_counties_2019 <- get_acs( 
  #pulling northern california megaregion counties, 21 total
  geography = "county",
  variables = c(
    agg_commute_all_types = "B08136_001",
    commute_car = "B08136_002",
    commute_pt = "B08136_007",
    commute_pt_bus = "B08136_008",
    commute_pt_shortrail = "B08136_009",
    commute_pt_longtrain = "B08136_010",
    commute_average = "B08303_001", #denominator
    commute_60to89 = "B08303_012",
    commute_over_90 = "B08303_013",
    income_percap = "B19301_001", #per Capita Income in the Past 12 Months (2022 Inflation-Adjusted $)
    income_median = "B19013_001", #Median Household Income in the Past 12 Months (2022 Inflation-Adjusted $)
    poverty_level = "B17009_002", 
    rentburd_median = "B25071_001", #Median gross rent as a percentage of household income 
    #re-read what this is
    rentburd_total = "B25070_001",  # denominator for rent burden (gross rent as a percentage of household income)
    #check what this denominator is -- % ?  or n count? 
    rentburd_30_35 = "B25070_007", # rent burden 30.0 to 34.9 percent 
    rentburd_35_40 = "B25070_008",  # rent burden 35.0 to 39.9 percent
    rentburd_40_50 = "B25070_009",  # rent burden 40.0 to 49.9 percent
    rentburd_over_50 = "B25070_010",  # rent burden 50.0 percent or more
    rentburd_subtract = "B25070_011",  # not computed; subtract from denominator
    total_pop = "B01001_001" #total population, from sex by age
  ),
  state = "California",
  year = 2019,
  survey = "acs5",
  output = "wide",
) %>% 
  filter(
    #San Francisco Bay Area  
    NAME == "San Francisco County, California" |
      NAME == "Alameda County, California" |
      NAME == "Contra Costa County, California" |
      NAME == "Marin County, California" |
      NAME == "Napa County, California" |
      NAME == "Solano County, California" |
      NAME == "Sonoma County, California" |
      NAME == "San Mateo County, California" |
      NAME == "Santa Clara County, California" |
      #Sacramento Area
      NAME == "Sacramento County, California" |
      NAME == "Yolo County, California" |
      NAME == "Yuba County, California" |
      NAME == "Sutter County, California" |
      NAME == "Placer County, California" |
      NAME == "El Dorado County, California" |
      #Northern San Joaquin Valley
      NAME == "San Joaquin County, California" |
      NAME == "Stanislaus County, California" |
      NAME == "Merced County, California" |
      #Monterey Bay Area
      NAME == "San Benito County, California" |
      NAME == "Monterey County, California" |
      NAME == "Santa Cruz County, California"
  )


#filter for the specific pumas in norcal megaregion
ncal_pumas_2019 <- get_acs(
  geography = "public use microdata area",
  variables = c(
    agg_commute_all_types = "B08136_001", #too many PUMAs with NA to run for all
    commute_car = "B08136_002", #too many PUMAs with NA to run for all
    commute_pt = "B08136_007", #too many PUMAs with NA to run for all
    commute_pt_bus = "B08136_008", #too many PUMAs with NA to run for all
    commute_pt_shortrail = "B08136_009", #too many PUMAs with NA to run for all
    commute_pt_longtrain = "B08136_010", #too many PUMAs with NA to run for all
    commute_average = "B08303_001", #denominator
    commute_60to89 = "B08303_012",
    commute_over_90 = "B08303_013",
    income_percap = "B19301_001", #per Capita Income in the Past 12 Months (2022 Inflation-Adjusted $)
    income_median = "B19013_001", #Median Household Income in the Past 12 Months (2022 Inflation-Adjusted $)
    poverty_level = "B17009_002", 
    rentburd_median = "B25071_001", #Median gross rent as a percentage of household income 
    #re-read what this is
    rentburd_total = "B25070_001",  # denominator for rent burden (gross rent as a percentage of household income)
    #check what this denominator is -- %? n count? 
    rentburd_30_35 = "B25070_007", # rent burden 30.0 to 34.9 percent 
    rentburd_35_40 = "B25070_008",  # rent burden 35.0 to 39.9 percent
    rentburd_40_50 = "B25070_009",  # rent burden 40.0 to 49.9 percent
    rentburd_over_50 = "B25070_010",  # rent burden 50.0 percent or more
    rentburd_subtract = "B25070_011",  # not computed; subtract from denominator
    total_pop = "B01001_001" #total population, from sex by age
  ),
  state = "California",
  year = 2019,
  survey = "acs5",
  output = "wide") %>% 
  filter(str_detect(NAME, counties_norcal_megaregion)) %>% 
  filter(!str_detect(NAME, "Los Angeles")) 


#clean names and mutate some variables to create pooled driving times 
  