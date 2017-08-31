# K.Palof   8-31-17
# calculationg of annual harvest and STD cpue for tanner crab fishery
# Year: 2017-2018
# results are currently graphed in: sigma plot 'Tanner SEAK Standardized CPUE_2017.JNB'

# Objective: harvest and std cpue for tanner crab 2017/2018

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
fishtkt <- read_excel(path = "./data/TannerFishTicketData_2017.xlsx", sheet = 1)
logbook <- read_excel(path = "./data/TannerLogbookData_2017.xlsx", sheet = 1)

## logbook data by day of year

# need to add survey area 3 designation - see "All logbook data_17" spreadsheet
# need to convery effort date to day of year

logbook %>% select(Year = YEAR, effort.date = EFFORT_DATE, District = DISTRICT, 
                   Sub.district = SUB_DISTRICT, ADFG_NO, pots = NUMBER_POTS_LIFTED, 
                   numbers = TARGET_SPECIES_RETAINED) -> logbook1 

logbook1 %>% mutate(day = strftime(effort.date, format = "%j")) 
