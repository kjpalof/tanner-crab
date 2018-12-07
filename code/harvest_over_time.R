# K.Palof
# 12-6-18

# Look at Tanner crab harvest over time in each managment area.
# Total pounds and percentage of mature biomass in that area.

## Data needs:
# 1) Harvest over time
# 2) Biomass estimate from each area from most recent year's CSAs - currently from 2018 survey

# load ----
source('./code/helper.R')


# data ----
# obtained from project - crab_data_processing - in results folder - has fish ticket totals assigned with the
# survey area that they correspond to - also includes confidentiality. 
# see code file from the above project 'tanner_harvest.R'
harvest <- read.csv('./data/tanner_comm_catch_97_2018_confid.csv')
biomass <- read.csv('./data/tanner_2018_biomassmodel.csv')

# data manipulation -----
biomass %>% 
  select(Year, Mature, survey.area = Area) %>% 
  left_join(harvest) %>% 
  select(Year, Mature, survey.area, Season, pounds, confidential) %>% 
  mutate(hrate = pounds/Mature, hrate2 = round(hrate, 2)*100)-> data_combine

# by area - harvest over time ----------
harvest %>% 
  filter(survey.area != "Other") %>% 
  ggplot(aes(Year, pounds)) +
  geom_point() +
  facet_wrap(~survey.area)

# by area - harvest with labels ------
data_combine %>% 
  ggplot(aes(x = Year, y = pounds)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = hrate2), vjust = -0.3, size = 3.5) +
  geom_line(aes(x = Year, y = Mature)) +
  facet_wrap(~survey.area)
