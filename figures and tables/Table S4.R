####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Table S4 all weeks sensitivity

# AUTHOR - Dustin T. Hill

# Date Created: August 11, 2025

# LAST UPDATED - September 30, 2025

# --------------------------
# Packages
# --------------------------

library(pROC)
library(showtext)
library(confintr)
library(flextable)
library(grid)
library(scales)
library(sjPlot)
library(gridExtra)
library(MetBrewer)
library(tidygeocoder)
library(janitor)
library(readxl)
library(zoo)
library(lubridate)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

# --------------------------
# Functions
# --------------------------

source("RSV - functions.R")

# ------------
# ggplot theme
# ------------

source("RSV - plot themes.R")

# --------------------------
# Load combined data file
# --------------------------

load(file = "data/rsv_combined_data.Rdata")

# Add seasons and pilot county objects

# list of pilot counties
pilot <- c("Jefferson", "Erie", "Onondaga", "Westchester")
pilot_cases <- c("Jefferson", "Onondaga")
onondaga <- c("Onondaga")


# Sensitivity of detection
# Sewershed and county level



# ------------------------------------------------------------------------------

###
# save tables
###

# sensitivity all weeks
sens_all_weeks <- sens_output_table_function(season_select = c("Weeks 21 to 39",
                                                               "Weeks 40 to 20"),
                                             season_name = "All weeks")
# make the df a flextable
sens_table_ft <- table_as_flex_function(
  dataframe = sens_all_weeks,
  title = paste("Sensitivity, specificity, PPV, and NPV of RSV wastewater",
                " surveillance (all weeks)", sep = "")
)

sens_table_ft

# save as table
save_as_docx(FitFlextableToPage(sens_table_ft), 
             path = "figures and tables/RSV sensitivity table - all weeks.docx")