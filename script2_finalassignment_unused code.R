#script2_finalassignment_unused code

#unused variables
variables = c(
  agg_commute_all_types = "B08136_001", #too many counties with NA to run for all
  agg_commute_car = "B08136_002",#too many counties with NA to run for all
  agg_commute_pt = "B08136_007",#too many counties with NA to run for all
  agg_commute_pt_bus = "B08136_008",#too many counties with NA to run for all
  agg_commute_pt_shortrail = "B08136_009",#too many counties with NA to run for all
  agg_commute_pt_longtrain = "B08136_010",#too many counties with NA to run for all

#clevelanddot chart for 2022
  ncal_counties_2022_clevelanddot <- ncal_counties_2022 %>%
    arrange(commutepercent_over90) %>%
    mutate(NAME = factor(NAME, levels = unique(NAME))) %>%
    ggplot(aes(commutepercent_over90, NAME),
           color = "blue", size = 2) +
    geom_point() +
    scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 15)) +
    labs(
      title = "2022 test",
      subtitle = "just a test",
      x = NULL, 
      y = "% of commutes lasting longer than 90 minutes",
      caption = "Source: Census Bureau") +
    theme_minimal()
  
  ncal_counties_2022_clevelanddot   
  
  
#filtering for the specific pumas in norcal megaregion
  ncal_pumas_2022 <- get_acs(
    geography = "public use microdata area",
    variables = c(
      commutes_total = "B08134_001",#denominator
      commutes_over60 = "B08134_010",#highest subset available for this stub
      commutes_by_car = "B08134_011",#Car, truck, or van
      commutes_by_car_over60 = "B08134_020",
      commutes_by_pt = "B08134_061",#Public transportation (excluding taxicab)
      commutes_by_pt_over60 = "B08134_070",
      commutes_by_pt_bus = "B08134_071",
      commutes_by_pt_bus_over60 = "B08134_080",
      commutes_by_pt_shortrail = "B08134_081",#Subway or elevated rail, Light rail, streetcar, or trolley
      commutes_by_pt_shortrail_over60 = "B08134_090",
      commutes_by_pt_longrail = "B08134_091",# Long-distance train or commuter rail or Ferryboat
      commutes_by_pt_longrail_over60 = "B08134_100",
      #for all above this line, lots of counties with missing data so should be cautious in comparing
      commutes_total_by_workplace ="B08412_001", #by destination of commute, denominator 
      commutes_by_workplace_over90 = "B08412_013",#by destination of commute
      commute_average = "B08303_001", #denominator
      commute_60to89 = "B08303_012",
      commute_over_90 = "B08303_013",
      income_percap = "B19301_001", #per Capita Income in the Past 12 Months (2022 Inflation-Adjusted $)
      income_median = "B19013_001", #Median Household Income in the Past 12 Months (2022 Inflation-Adjusted $)
      poverty_level = "B17009_002", 
      rentburd_median = "B25071_001", #Median gross rent as a percentage of household income 
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

  ncal_pumas_2022 <- ncal_pumas_2022 %>% 
    mutate(
      commutepercent_over60 = 100*(commute_60to89E+commute_over_90E)/commute_averageE,
      commutepercent_over90 = 100*commute_over_90E/commute_averageE,
      #of all commutes, what % are done by...
      commutepercent_car = 100*commutes_by_carE/commutes_totalE,
      commutepercent_pt = 100*commutes_by_ptE/commutes_totalE,
      commutepercent_by_pt_bus = 100*commutes_by_pt_busE/commutes_totalE,
      commutepercent_by_pt_shortrail = 100*commutes_by_pt_shortrailE/commutes_totalE,
      commutepercent_by_pt_longrail = 100*commutes_by_pt_longrailE/commutes_totalE,
      #within each type of commute, what % take over 60 min?
      commutepercent_by_car_over60 = 100*commutes_by_car_over60E/commutes_by_carE,   
      commutepercent_by_pt_over60 = 100*commutes_by_pt_over60E/commutes_by_ptE, 
      commutepercent_by_pt_bus_over60 = 100*commutes_by_pt_bus_over60E/commutes_by_pt_busE,
      commutepercent_by_pt_shortrail_over60 = 100*commutes_by_pt_shortrail_over60E/commutes_by_pt_shortrailE,
      commutepercent_by_pt_longrail_over60 = 100*commutes_by_pt_longrail_over60E/commutes_by_pt_longrailE,
      commutepercent_over90_by_workplace = 100*commutes_by_workplace_over90E/commutes_total_by_workplaceE,
      rb_over30_percent = 100*(rentburd_30_35E+rentburd_35_40E+rentburd_40_50E+rentburd_over_50E)/(rentburd_totalE-rentburd_subtractE),
      poverty_levelpercent = 100*poverty_levelE/total_popE,
      NAME = str_remove(NAME, "County, California")
    )
  
  #filter for the specific pumas in norcal megaregion
  ncal_pumas_2019 <- get_acs(
    geography = "public use microdata area",
    variables = c(
      commutes_total = "B08134_001",#denominator
      commutes_over60 = "B08134_010",#highest subset available for this stub
      commutes_by_car = "B08134_011",#Car, truck, or van
      commutes_by_car_over60 = "B08134_020",
      commutes_by_pt = "B08134_061",#Public transportation (excluding taxicab)
      commutes_by_pt_over60 = "B08134_070",
      commutes_by_pt_bus = "B08134_071",
      commutes_by_pt_bus_over60 = "B08134_080",
      commutes_by_pt_shortrail = "B08134_081",#Subway or elevated rail, Light rail, streetcar, or trolley
      commutes_by_pt_shortrail_over60 = "B08134_090",
      commutes_by_pt_longrail = "B08134_091",# Long-distance train or commuter rail or Ferryboat
      commutes_by_pt_longrail_over60 = "B08134_100",
      #for all above this line, lots of counties with missing data so should be cautious in comparing
      commutes_total_by_workplace ="B08412_001", #by destination of commute, denominator 
      commutes_by_workplace_over90 = "B08412_013",#by destination of commute
      commute_average = "B08303_001", #denominator
      commute_60to89 = "B08303_012",
      commute_over_90 = "B08303_013",
      income_percap = "B19301_001", #per Capita Income in the Past 12 Months (2022 Inflation-Adjusted $)
      income_median = "B19013_001", #Median Household Income in the Past 12 Months (2022 Inflation-Adjusted $)
      poverty_level = "B17009_002", 
      rentburd_median = "B25071_001", #Median gross rent as a percentage of household income 
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
  #note: after pulling these, realized that there are 10 new PUMAs in Norcal
  #they all have different geoIDs too
  #ran some stuff in the console, found one new PUMA in each of these counties: 
  #Alameda,Sacramento,SF,San Joaquin,Santa Clara,Sonoma,Yolo
  #given this, comparison between PUMAs in these areas will be trickier and may need to be avoided
  
  ncal_pumas_2019 <- ncal_pumas_2019 %>% 
    mutate(
      commutepercent_over60 = 100*(commute_60to89E+commute_over_90E)/commute_averageE,
      commutepercent_over90 = 100*commute_over_90E/commute_averageE,
      #of all commutes, what % are done by...
      commutepercent_car = 100*commutes_by_carE/commutes_totalE,
      commutepercent_pt = 100*commutes_by_ptE/commutes_totalE,
      commutepercent_by_pt_bus = 100*commutes_by_pt_busE/commutes_totalE,
      commutepercent_by_pt_shortrail = 100*commutes_by_pt_shortrailE/commutes_totalE,
      commutepercent_by_pt_longrail = 100*commutes_by_pt_longrailE/commutes_totalE,
      #within each type of commute, what % take over 60 min?
      commutepercent_by_car_over60 = 100*commutes_by_car_over60E/commutes_by_carE,   
      commutepercent_by_pt_over60 = 100*commutes_by_pt_over60E/commutes_by_ptE, 
      commutepercent_by_pt_bus_over60 = 100*commutes_by_pt_bus_over60E/commutes_by_pt_busE,
      commutepercent_by_pt_shortrail_over60 = 100*commutes_by_pt_shortrail_over60E/commutes_by_pt_shortrailE,
      commutepercent_by_pt_longrail_over60 = 100*commutes_by_pt_longrail_over60E/commutes_by_pt_longrailE,
      commutepercent_over90_by_workplace = 100*commutes_by_workplace_over90E/commutes_total_by_workplaceE,
      rb_over30_percent = 100*(rentburd_30_35E+rentburd_35_40E+rentburd_40_50E+rentburd_over_50E)/(rentburd_totalE-rentburd_subtractE),
      poverty_levelpercent = 100*poverty_levelE/total_popE,
      NAME = str_remove(NAME, "County, California")
    )  
  
#testing out some scatterplots
#not much here for supercommute change vs pl change or housing burden change
scatter_diff_pop_supercommute <- ncal_counties_2019_2022 %>%
  ggplot(aes(x = total_pop_percent_diff, y = commute_over90_diff)) +
  geom_point() +
  scale_x_continuous(expand = expansion(mult = c(0.002, 0)), 
                     limits = c(-10, 15),
                     breaks = -5:10) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.002)), 
                     limits = c(-5, 5),
                     breaks = -15:15 * 5) +
  labs(x = "Change in Rent Burden Over 30%)",
       y = "Change in Supercommutes") +
  scatter_grid()

scatter_diff_pl_supercommute