# K.Palof   11-8-18 / 11-5-19
# calculationg of annual harvest and STD cpue for tanner crab fishery
# Year: 2019-2020
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
#Load data ----------------
fishtkt <- read.csv("./data/Tanner_Detailed Fish Tickets_ALL_years.csv")
#logbook <- read_excel(path = "./data/TannerLogbookData_2017.xlsx", sheet = 1)
logbook <- read_excel(path = "./data/2019 Tanner Logbook Data.xlsx", sheet = "AlexData")
statarea <- read.csv("./data/area_stat_areas.csv")
logbook_all <- read_excel(path = "./data/All_logbook_tanner.xlsx", sheet = "AlexData") # from ALEX not in OCEAN AK
cur_yr = 2019

## data manipulation -------

## current year ---------
logbook %>% select(Year = YEAR, effort.date = EFFORT_DATE, District = DISTRICT, 
                   Sub.district = SUB_DISTRICT, ADFG_NO, pots = NUMBER_POTS_LIFTED, 
                   numbers = TARGET_SPECIES_RETAINED) %>% 
  as.data.frame() -> logbook1 

logbook1 %>% 
  mutate(day = strftime(effort.date, format = "%j")) -> logbook1

# std cpue current year -------
logbook1 %>% 
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

# need to convery effort date to day of year
logbook_all %>% select(Year = YEAR, effort.date = EFFORT_DATE, District = DISTRICT, 
                   Sub.district = SUB_DISTRICT, ADFG_NO, pots = NUMBER_POTS_LIFTED, 
                   numbers = TARGET_SPECIES_RETAINED) %>% 
  as.data.frame() -> logbook_all1 

logbook_all1 %>% 
  mutate(day = strftime(effort.date, format = "%j")) -> logbook_all1

### std cpue -----------------
## determine cumulative pots ordered by day
logbook_all1 %>% 
  arrange(day) %>% 
  group_by(Year) %>% 
  arrange(day) %>% 
  mutate(cum.pots = cumsum(pots), cpue = numbers/pots) %>% 
  filter(cum.pots <= 12521) %>% 
  summarise(avg.cpue = mean(cpue, na.rm = TRUE), 
            se = sd(cpue, na.rm = TRUE)/sqrt(length(cpue))) %>% 
  as.data.frame() %>% 
  write_csv(paste0('./results/std_commericial_cpue' , cur_yr, '.csv'))





