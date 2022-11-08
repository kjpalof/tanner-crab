# K.Palof   11-8-18 / 11-5-19 / 11-15-20 / 11-8-21 / 11-7-22

# calculationg of annual harvest and STD cpue for tanner crab fishery
# Year: 2022 - 2023
# result were previously graphed in: sigma plot 'Tanner SEAK Standardized CPUE_2017.JNB'

# these results are used to create figures in SE_crab_assessments - more here **FIX**

# Objective: harvest and std cpue for tanner crab current season

# Load packages -------------
library(tidyverse)
library(readxl)
library(extrafont)
library(grid)
library(gridExtra)
#font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# global -----
cur_yr = 2022 # update annually 

#Load data ----------------
fishtkt <- read.csv(paste0('./data/Tanner_Detailed Fish Tickets_ALL_years_', cur_yr,'.csv'))
# pull from Ocean AK 
#logbook <- read_excel(path = "./data/TannerLogbookData_2017.xlsx", sheet = 1)

# moved to OceanAK ???? in 2021 therefore just have .csv with 2021 output. This will change in the future
#logbook <- read_excel(path = paste0('./data/Tanner crab_Invertebrate Logbook Data_', cur_yr, '.xlsx'), sheet = "AlexData")
logbook <- read.csv(paste0('./data/tanner_logbook_2020_', cur_yr,'.csv'))
#logbook <- read_excel(path = paste0('./data/tanner_logbook_2020_', cur_yr,'.xlsx'), sheet = "tanner_logbook_2020_2022")
statarea <- read.csv("./data/area_stat_areas.csv")
logbook_all <- read_excel(path = "./data/All_logbook_tanner.xlsx", sheet = "AlexData") # from ALEX not in OCEAN AK
# only goes to 2019

## data manipulation -------

## current year ---------
logbook %>% 
  filter(Year >= 2020) %>% 
  select(Year, effort.date = Entry.Date, District, 
                   Sub.district, ADFG_NO = ADFG.Number, pots = Number.of.Pots.Lifted, 
                   numbers = Target.Species.Retained) %>% 
  mutate(effort.date = as.Date(effort.date, "%m/%d/%Y")) %>% 
  as.data.frame() -> logbook1 

logbook1 %>% 
  mutate(day = strftime(effort.date, format = "%j")) -> logbook1

# std cpue current year -------
logbook1 %>% 
  filter(pots > 0) %>% 
  arrange(day) %>% 
  group_by(Year) %>% 
  arrange(day) %>% 
  mutate(cum.pots = cumsum(pots), cpue = numbers/pots) %>% 
  filter(cum.pots <= 12521) %>% 
  summarise(avg.cpue = mean(cpue), 
            se = sd(cpue)/sqrt(length(cpue)))


## all years -------------
# add current years data 
#logbook_all %>% 
#  bind_rows(logbook) -> logbook_all

# need to convert effort date to day of year
logbook_all %>% select(Year = YEAR, effort.date = EFFORT_DATE, District = DISTRICT, 
                   Sub.district = SUB_DISTRICT, ADFG_NO, pots = NUMBER_POTS_LIFTED, 
                   numbers = TARGET_SPECIES_RETAINED) %>% 
  as.data.frame() -> logbook_all1 

logbook_all1 %>% 
  mutate(day = strftime(effort.date, format = "%j")) -> logbook_all1

## add years since 2019 since that's all I have in all logbook data...need to **fix** this.
logbook_all1 %>% 
  bind_rows(logbook1) -> logbook_all_cur
### std cpue -----------------
## determine cumulative pots ordered by day
logbook_all_cur %>% 
  filter(pots >0) %>% 
  arrange(day) %>% 
  group_by(Year) %>% 
  arrange(day) %>% 
  mutate(cum.pots = cumsum(pots), cpue = numbers/pots) %>% 
  filter(cum.pots <= 12521) %>% 
  summarise(avg.cpue = mean(cpue, na.rm = TRUE), 
            se = sd(cpue, na.rm = TRUE)/sqrt(length(cpue))) %>% 
  as.data.frame() %>% 
  write_csv(paste0('./results/std_commericial_cpue' , cur_yr, '.csv'))





