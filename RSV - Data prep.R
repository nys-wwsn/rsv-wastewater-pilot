####
#### RSV WASTEWATER DATA ANALYSIS FOR PILOT STUDY
####

# Data prep script
# This script loads and preprocesses RSV ww concentration data, hospitaization,
# and case data for analysis.

# AUTHOR - Dustin T. Hill

# Date Created: August 8, 2025

# LAST UPDATED - July 20, 2026

# --------------
# PACKAGES
# --------------

library(showtext)
library(flextable)
library(grid)
library(scales)
library(tidygeocoder)
library(janitor)
library(readxl)
library(zoo)
library(lubridate)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidycensus)

# -----------------
# Load data
# -----------------

# Wastewater data
rsv_ww_county <- read.csv("data/rsv-wastewater-county.csv")

# cases
rsv_cases_county_weekly <- read.csv("data/rsv-cases-county.csv")

# hospitalizations
hosp_county_weekly <- read.csv("data/rsv-hospitalizations-county.csv")

# ---------------
# Merge data
# ---------------

# County age adjusted incidence using population of the age groups

# county level merge
dat_county <- left_join(rsv_ww_county,
                        hosp_county_weekly,
                        by = c("week",
                               "county"))

# add  case data
dat_county <- left_join(dat_county,
                        rsv_cases_county_weekly,
                        by = c("week",
                               "county"))

# replace missing hosp and case data with 0
dat_county$rsv_hosp_10 <- ifelse(is.na(dat_county$rsv_hosp_10),
                                 0,
                                 dat_county$rsv_hosp_10)
dat_county$rsv_hosp_all <- ifelse(is.na(dat_county$rsv_hosp_all),
                                  0,
                                  dat_county$rsv_hosp_all)
dat_county$rsv_hosp_50 <- ifelse(is.na(dat_county$rsv_hosp_50),
                                 0,
                                 dat_county$rsv_hosp_50)
dat_county$rsv_cases_total <- ifelse(is.na(dat_county$rsv_cases_total),
                                     0,
                                     dat_county$rsv_cases_total)
# add county population
county_region <- read.csv("data/County_Region.csv") %>%
  dplyr::select(region, county, county_pop)%>%
  rename(Region = region,
         County_pop = county_pop)

dat_county <- left_join(dat_county,
                        county_region,
                        by = c("county"))
# add per 100k adjustments
dat_county$rsv_hosp_all_100 <- dat_county$rsv_hosp_all  * 
  (100000)/dat_county$County_pop

# Add age adjusted per 100k pulled from tidycensus
pop_data <- read.csv(
  paste0("data/", "ny_pop_numbers.csv")) %>%
  separate_wider_delim(
    cols = NAME, 
    delim = ",", 
    names = c("county", "state")
  ) %>%
  separate(county, into = c("county", "extra"), sep = " ")

# merge to main dataset
dat_county <- left_join(
  dat_county,
  pop_data,
  by = c("county")
)

dat_county$rsv_hosp_10_100 <- dat_county$rsv_hosp_10  * 
  (100000)/dat_county$total_under10
dat_county$rsv_hosp_50_100 <- dat_county$rsv_hosp_50  * 
  (100000)/dat_county$total_over50

# case data per 100k adjustment
dat_county$rsv_cases_total_100 <- dat_county$rsv_cases_total *
  (100000)/dat_county$County_pop

summary(dat_county)

# add seasons
# season using weeks 40 to 20

# add week number to the data
dat_county$week_number <- strftime(dat_county$week, format = "%V")

# add season based on week number
dat_county$season <- ifelse(dat_county$week_number >= 40 | dat_county$week_number <= 20,
                            "Weeks 40 to 20", "Weeks 21 to 39")

# --- save combined files ---
save(dat_county,
     file = "data/rsv_combined_data.Rdata")

