####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Table S5 - winter sensitivity values table

# AUTHOR - Dustin T. Hill

# Date Created: August 11, 2025

# LAST UPDATED - July 20, 2026

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

load(file = "Data/rsv_combined_data.Rdata")

# Add seasons and pilot county objects

# list of pilot counties
pilot <- c("Jefferson", "Erie", "Onondaga", "Westchester")
pilot_cases <- c("Jefferson", "Onondaga")
onondaga <- c("Onondaga")

####
# winter
### 

sens_winter <- sens_output_table_function(season_select = c("Weeks 40 to 20"),
                                          season_name = "Weeks 40 to 20")


# make the df a flextable
sens_table_ft <- table_as_flex_function(
  dataframe = sens_winter,
  title = paste("Sensitivity, specificity, PPV, and NPV of RSV wastewater",
                " surveillance (Weeks 40-20)", sep = "")
)

sens_table_ft

# save as table
save_as_docx(FitFlextableToPage(sens_table_ft), 
             path = "figures and tables/RSV sensitivity table - winter.docx")
