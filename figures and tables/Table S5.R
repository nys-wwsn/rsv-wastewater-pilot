####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Table S5 correlations

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

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# correlation tables

# jefferson all months case data
row_3 <- cor_hosp_function(
  dataframe = dat_county,
  county_name = "Jefferson",
  comparison_variable = "rsv_cases_total_100",
  comparison_name = "log+1(Cases ~ concentration)",
  ww_variable = "rsv_ww_county",
  season_name = c("Weeks 21 to 39", "Weeks 40 to 20")
)

# onondaga all months case data
row_4 <- cor_hosp_function(
  dataframe = dat_county,
  county_name = "Onondaga",
  comparison_variable = "rsv_cases_total_100",
  comparison_name = "log+1(Cases ~ concentration)",
  ww_variable = "rsv_ww_county",
  season_name = c("Weeks 21 to 39", "Weeks 40 to 20")
)

# hospitalizations data

# all hospital admissions
all_hosp_cor <- hosp_correlations_stack_function(
  comparison_variable = "rsv_hosp_all_100",
  ww_variable = "rsv_ww_county",
  hosp_group = "All hospitalizations"
)

# under ten
under10_hosp_cor <- hosp_correlations_stack_function(
  comparison_variable = "rsv_hosp_10_100",
  ww_variable = "rsv_ww_county",
  hosp_group = "Under 10"
)

# over 50
over50_hosp_cor <- hosp_correlations_stack_function(
  comparison_variable = "rsv_hosp_50_100",
  ww_variable = "rsv_ww_county",
  hosp_group = "Over 50"
)


# combine into table
cor_table <- bind_rows(row_3, row_4, all_hosp_cor) %>%
  select(-hosp_group)
colnames(cor_table) <- c("season", "Aggregation", "values tested", 
                         "Spearman's r", 
                         "95% LL", "95% UL", "observation")
cor_table <- as.data.frame(cor_table)
names(cor_table)

# save table
cor_table_ft <- table_as_flex_function(dataframe = cor_table,
                                       title = "Spearman correlations")
save_as_docx(FitFlextableToPage(cor_table_ft), 
             path = "figures and tables/Table - RSV correlation table.docx")
