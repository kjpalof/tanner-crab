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
