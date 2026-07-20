####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Table S2

# AUTHOR - Dustin T. Hill

# Date Created: August 11, 2025

# LAST UPDATED - July 20, 2026

# --------------------------
# Packages
# --------------------------
library(gt)
library(gtExtras)
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


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# -----------------------------------------
# detection rate all months and by season
# -----------------------------------------

dat_descrip_DR <- dat_sewershed %>%
  group_by(county, sw_id, season) %>%
  summarize(
    n_samples = sum(n_samples, na.rm = TRUE),
    min_date = min(week, na.rm = TRUE),
    max_date = max(week, na.rm = TRUE),
    n_positives = sum(positives, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(sw_id,  county, season) %>%
  mutate(`Detection rate` = (n_positives / n_samples)*100) %>%
  ungroup() %>%
  group_by(season, county) %>%
  summarize(
    n_sewersheds = length(unique(sw_id)),
    n_samples = sum(n_samples),
    mean = mean(`Detection rate`, na.rm = TRUE),
    min = min(`Detection rate`, na.rm = TRUE),
    median = median(`Detection rate`, na.rm = TRUE),
    max = max(`Detection rate`, na.rm = TRUE),
    distribution = list(`Detection rate`)
  ) %>%
  ungroup()%>%
  mutate(group = "Detection rate") %>%
  distinct() %>%
  arrange(season)


dat_descrip_DR_total <- dat_sewershed %>%
  group_by(county, sw_id) %>%
  summarize(
    n_samples = sum(n_samples, na.rm = TRUE),
    min_date = min(week, na.rm = TRUE),
    max_date = max(week, na.rm = TRUE),
    n_positives = sum(positives, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(sw_id,  county) %>%
  mutate(`Detection rate` = (n_positives / n_samples)*100) %>%
  ungroup() %>%
  group_by(county) %>%
  summarize(
    n_sewersheds = length(unique(sw_id)),
    n_samples = sum(n_samples),
    mean = mean(`Detection rate`, na.rm = TRUE),
    min = min(`Detection rate`, na.rm = TRUE),
    median = median(`Detection rate`, na.rm = TRUE),
    max = max(`Detection rate`, na.rm = TRUE),
    distribution = list(`Detection rate`)
  ) %>%
  ungroup()%>%
  mutate(season = "All weeks") %>%
  distinct()



# reduce significant figures
dat_descrip_DR_table <- dat_descrip_DR %>%
  bind_rows(dat_descrip_DR_total) %>%
  mutate_if(is.numeric, ~round(., 2)) %>%
  select(-group, -distribution)

# make it a flextable
dr_ft <- table_as_flex_function(dataframe = dat_descrip_DR_table, 
                                title = 
                                  "Table: Descriptive statistics for RSV detection rates")
dr_ft 

# save as doc
save_as_docx(FitFlextableToPage(dr_ft), 
             path = paste0("figures and tables/RSV wastewater detection rate",
                           " descriptive statistics.docx"))