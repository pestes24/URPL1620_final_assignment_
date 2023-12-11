#script2_finalassignment_unused code

#unused variables
variables = c(
  agg_commute_all_types = "B08136_001", #too many counties with NA to run for all
  agg_commute_car = "B08136_002",#too many counties with NA to run for all
  agg_commute_pt = "B08136_007",#too many counties with NA to run for all
  agg_commute_pt_bus = "B08136_008",#too many counties with NA to run for all
  agg_commute_pt_shortrail = "B08136_009",#too many counties with NA to run for all
  agg_commute_pt_longtrain = "B08136_010",#too many counties with NA to run for all

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