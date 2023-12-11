library(tidyverse)
library(janitor)
library(tidycensus)
library(stringr) 
library(lubridate)

#installed census api 
#census_api_key("cd05290c85440e7e445f0900d13bb133bfdfa6ad", install = TRUE)

variables_2022 <- load_variables(
  year = 2022, 
  dataset = "acs1", 
  cache = TRUE)

#pulling and cleaning data for full northern california megaregion (21 total)
#http://www.bayareaeconomy.org/report/the-northern-california-megaregion/ 
#https://www.spur.org/news/2018-06-19/where-exactly-bay-area

counties_norcal_megaregion <- 
  "San Francisco|Alameda|Contra Costa|Marin|Napa|Solano|Sonoma|San Mateo|Santa Clara|Sacramento|Yolo|Yuba|Sutter|Placer|El Dorado|San Joaquin|Stanislaus|Merced|San Benito|Monterey|Santa Cruz"

#San Francisco Bay Area (9) 
#San Francisco,Alameda,Contra Costa,Marin,Napa,Solano,Sonoma,San Mateo,Santa Clara
#Sacramento Area (6)
#Sacramento,Yolo,Yuba,Sutter,Placer,El Dorado
#Northern San Joaquin Valley (3)
#San Joaquin,Stanislaus,Merced
#Monterey Bay Area (3)
#San Benito,Monterey,Santa Cruz


ncal_counties_2022 <- get_acs( 
  #pulling northern california megaregion counties, 21 total
  geography = "county",
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
  filter(str_detect(NAME, counties_norcal_megaregion))
 

ncal_counties_2022 <- ncal_counties_2022 %>% 
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

#now re-running with copied code from above but pulling pre-pandemic data
#acs 5-year 2015-2019

ncal_counties_2019 <- get_acs( 
  #pulling northern california megaregion counties, 21 total
  geography = "county",
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
  filter(str_detect(NAME, counties_norcal_megaregion))

ncal_counties_2019 <- ncal_counties_2019 %>% 
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

#joining to do comparisons and create some new variables
ncal_counties_2019_2022 <- 
  full_join(ncal_counties_2019, ncal_counties_2022, by = "GEOID", suffix = c("_2019", "_2022")) %>% 
  mutate(
    commute_over90_diff = commutepercent_over90_2022-commutepercent_over90_2019,
    rb_median_diff = rentburd_medianE_2022-rentburd_medianE_2019,
    rb_over30_diff = rb_over30_percent_2022-rb_over30_percent_2019,
    poverty_levelpercent_diff = poverty_levelpercent_2022-poverty_levelpercent_2019,
    total_pop_percent_diff = 100*(total_popE_2022 - total_popE_2019)/total_popE_2019,
    commutepercent_over90_by_workplace_diff = commutepercent_over90_by_workplace_2022-commutepercent_over90_by_workplace_2019,
    commutepercent_by_car_over60_diff = commutepercent_by_car_over60_2022-commutepercent_by_car_over60_2019,
    commutepercent_car_diff = commutepercent_car_2022-commutepercent_car_2019
  )

ncal_counties_2019_2022 %>% 
  write_csv(file = "ncal_counties_2019_2022.csv")

ncalcounties_over90_average_percentchange_2019_2022 <- ncal_counties_2019_2022 %>%
  summarize(commute_over90_average_percentchange=mean(commute_over90_diff,na.rm=TRUE),
            commute_over90_median_percentchange=median(commute_over90_diff,na.rm=TRUE),
                   n=n()) 

ncalcounties_over90_by_workplace_average_percentchange_2019_2022 <- ncal_counties_2019_2022%>% 
  summarize(commute_over90_by_workplace_average_percentchange=mean(commutepercent_over90_by_workplace_diff,na.rm=TRUE),
            commute_over90_by_workplace_median_percentchange=median(commutepercent_over90_by_workplace_diff,na.rm=TRUE),
            n=n())

ncalcounties_summarystats_2019_2022 <- cbind(ncalcounties_over90_by_workplace_average_percentchange_2019_2022, ncalcounties_over90_average_percentchange_2019_2022) %>% 
  write_csv(file = "ncalcounties_summarystats_2019_2022.csv")

#ok, I think what I have for percents with agg are actually just percent of all commuting time...
#which is actually sort of interesting too -- it is showing total commuting hours, how much is done by car

top10ncal_counties_2022 <- ncal_counties_2022 %>%
  top_n(10, wt = commutepercent_over90)

#exploratory ggplotting
chart_ncal_counties_test <- ggplot(top10ncal_counties_2022) +
  geom_col( 
    aes(x = reorder(NAME, -commutepercent_over90), y = commutepercent_over90)) +
  scale_y_continuous(
    limits = c(0,15),
    labels = scales::number_format(scale = 1, big.mark = ""),
    breaks= seq(0, 15, by=1)
  ) +
  labs(
    title = "2022 test",
    subtitle = "just
    a
    test",
    x = "",
    y = "% of commutes over 90 minutes",
    caption = "Source: Census Bureau" 
  )+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

chart_ncal_counties_test

top10ncal_counties_2019 <- ncal_counties_2019 %>%
  top_n(10, wt = commutepercent_over90)

barchart_ncal_counties <- ggplot(top10ncal_counties_2019) +
  geom_col( 
    aes(x = reorder(NAME, -commutepercent_over90), y = commutepercent_over90)) +
  scale_y_continuous(
    limits = c(0,15),
    labels = scales::number_format(scale = 1, big.mark = ""),
    breaks= seq(0, 15, by=1)
  ) +
  labs(
    title = "2019 test",
    subtitle = "just
    a
    test",
    x = "",
    y = "% of commutes lasting longer than 90 minutes",
    caption = "Source: Census Bureau" 
  )+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

barchart_ncal_counties

#cleveland dot plots
ncal_counties_2019_clevelanddot <- ncal_counties_2019 %>%
  arrange(commutepercent_over90) %>%
  mutate(NAME = factor(NAME, levels = unique(NAME))) %>%
  ggplot(aes(commutepercent_over90, NAME)) +
  geom_point(color = "blue") +  
  scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 15)) +
  labs(
    title = "Rate of Supercommutes",
    subtitle = "In Northern California counties, 2019",
    x = "% of commutes lasting longer than 90 minutes", 
    y = NULL,
    caption = "Source: Census Bureau") +
  theme_minimal()

print(ncal_counties_2019_clevelanddot)


ncal_counties_2019_2022_clevelanddot <- ncal_counties_2019_2022 %>%
  arrange(commutepercent_over90_2019) %>%
  mutate(NAME_2019 = factor(NAME_2019, levels = unique(NAME_2019))) %>%
  ggplot(aes(x = commutepercent_over90_2019, xend = commutepercent_over90_2022, 
             y = NAME_2019, yend = NAME_2019)) +
  geom_point(aes(x = commutepercent_over90_2019), 
             color = "blue", size = 2) +
  geom_point(aes(x = commutepercent_over90_2022), 
             color = "darkgreen", size = 2) +
  geom_segment(color = "darkgrey", linewidth = 1, alpha = 0.5, linetype = "solid",
               aes(x = commutepercent_over90_2019, xend = commutepercent_over90_2022,
                   y = NAME_2019, yend = NAME_2019),
                   arrow = arrow(length = unit(0.25,"cm"),
                                 type = "closed", 
                                 angle=25),
               arrow.fill = "darkgrey", 
               ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 15)) +
  labs(
    title = "Change in % of supercommutes",
    subtitle = "In Northern California counties, 2019 vs 2022",
    x = "% of commutes lasting longer than 90 minutes", 
    y = "",
    caption = "Source: Census Bureau") +
  theme_minimal()
ncal_counties_2019_2022_clevelanddot
#will have to return and add legend

ncal_counties_2019_2022_longcarcommutes_clevelanddot <- ncal_counties_2019_2022 %>%
  arrange(commutepercent_by_car_over60_2019) %>%
  mutate(NAME_2019 = factor(NAME_2019, levels = unique(NAME_2019))) %>%
  ggplot(aes(x = commutepercent_by_car_over60_2019, xend = commutepercent_by_car_over60_2022, 
             y = NAME_2019, yend = NAME_2019)) +
  geom_point(aes(x = commutepercent_by_car_over60_2019), 
             color = "blue", size = 2) +
  geom_point(aes(x = commutepercent_by_car_over60_2022), 
             color = "darkgreen", size = 2) +
  geom_segment(color = "darkgrey", linewidth = 1, alpha = 0.5, linetype = "solid",
               aes(x = commutepercent_by_car_over60_2019, xend = commutepercent_by_car_over60_2022,
                   y = NAME_2019, yend = NAME_2019),
               arrow = arrow(length = unit(0.25,"cm"),
                             type = "closed", 
                             angle=25),
               arrow.fill = "darkgrey", 
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 25)) +
  labs(
    title = "Change in % of long car commutes",
    subtitle = "In Northern California counties, 2019 vs 2022",
    x = "% of car commutes lasting longer than 60 minutes", 
    y = "",
    caption = "Source: Census Bureau") +
  theme_minimal()
ncal_counties_2019_2022_longcarcommutes_clevelanddot

#will have to return and add legend
#    commutepercent_by_car_over60_diff = commutepercent_by_car_over60_2022-commutepercent_by_car_over60_2019,
#    commutepercent_car_diff = commutepercent_car_2022-commutepercent_car_2019


#alsotested out some scatterplots, saved in unused code file
#not much here for ...
#supercommute change vs poverty level change  
#supercommute change vs housing burden change
#supercommute change vs pop change
#moving to QGIS for mapping