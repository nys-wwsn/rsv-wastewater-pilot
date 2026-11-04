####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Table 1 - descriptives for all data

# AUTHOR - Dustin T. Hill

# Date Created: November 3, 2025

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

source("../RSV - functions.R")

# ------------
# ggplot theme
# ------------

source("../RSV - plot themes.R")

# --------------------------
# Load combined data file
# --------------------------

load("../data/rsv_combined_data.Rdata")

# ---------------------------------
# Summary Table - ww data all months and by season
# ---------------------------------

# ---------------------------------
# Summary Table - ww data all months and by season
# ---------------------------------

# summarize the ww data by season
dat_ww_descrip <- dat_sewershed  %>%
  ungroup() %>%
  group_by(county) %>%
  summarize(n_sewersheds = length(unique(sw_id)),
            n_samples = sum(n_samples, na.rm = TRUE),
            `min collection date` = min(week),
            `max collection date` = max(week),
            mean = mean(rsv_ww_mean, na.rm = TRUE),
            min = min(rsv_ww_mean, na.rm = TRUE),
            median = median(rsv_ww_mean, na.rm = TRUE),
            max = max(rsv_ww_mean, na.rm = TRUE),
            distribution = list(rsv_ww_mean)
  ) %>%
  ungroup() %>%
  mutate(group = "Wastewater data") %>%
  select(-distribution) %>%
  select(group, everything())

# detection rate
dat_descrip_DR <- dat_sewershed %>%
  group_by(sw_id, county) %>%
  summarize(
    n_samples = sum(n_samples, na.rm = TRUE),
    min_date = min(week, na.rm = TRUE),
    max_date = max(week, na.rm = TRUE),
    n_positives = sum(positives, na.rm = TRUE)
  ) %>%
  ungroup()%>%
  group_by(county) %>%
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
  mutate(group = "Detection rate") %>%
  distinct() %>%
  select(-distribution) %>%
  select(group, everything())

# hosp data

# pivot the hosp data longer
dat_county_long <- dat_county %>%
  select(week,county,season, rsv_hosp_all_100, rsv_hosp_50_100, 
         rsv_hosp_10_100) %>%
  pivot_longer(cols = c(rsv_hosp_all_100,
                        rsv_hosp_10_100,
                        rsv_hosp_50_100)) %>%
  mutate(name = case_when(
    name == "rsv_hosp_all_100" ~ "All hospitalizations",
    name == "rsv_hosp_10_100" ~ "Under 10",
    name == "rsv_hosp_50_100" ~ "Over 50"
  ))


# hosp data
dat_descrip_hosp <- dat_county_long %>%
  group_by(name, county) %>%
  summarize(`number of weeks above 0 admissions` = sum(value >0),
            mean = mean(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            distribution = list(value)
  ) %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  rename(group = name) %>%
  select(group, everything()) %>%
  select(-distribution)

# add total admissions reported
dat_hosp_count <- dat_county %>%
  select(week,county,season, rsv_hosp_all, rsv_hosp_50, 
         rsv_hosp_10) %>%
  pivot_longer(cols = c(rsv_hosp_all,
                        rsv_hosp_10,
                        rsv_hosp_50)) %>%
  mutate(name = case_when(
    name == "rsv_hosp_all" ~ "All hospitalizations",
    name == "rsv_hosp_10" ~ "Under 10",
    name == "rsv_hosp_50" ~ "Over 50"
  ))%>%
  group_by(name, county) %>%
  summarize(`total admissions reported` = sum(value, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  rename(group = name) %>%
  select(group, everything()) 

# merge to data
dat_descrip_hosp <- left_join(dat_descrip_hosp,
                              dat_hosp_count,
                              by = c("group", "county"))

# case data
dat_descrip_case <- dat_county %>%
  filter(
    county == "Jefferson" |
      county == "Onondaga") %>%
  ungroup()%>%
  group_by(county) %>%
  summarize(`Weeks above 0 case reports` = sum(rsv_cases_total_100 >0),
            mean = mean(rsv_cases_total_100, na.rm = TRUE),
            min = min(rsv_cases_total_100, na.rm = TRUE),
            median = median(rsv_cases_total_100, na.rm = TRUE),
            max = max(rsv_cases_total_100, na.rm = TRUE),
            `total cases reported` = sum(rsv_cases_total, na.rm = TRUE),
            distribution = list(rsv_cases_total_100)
  ) %>%
  ungroup() %>%
  mutate(group = "Case data") %>%
  replace(is.na(.), 0) %>%
  select(-distribution) %>%
  select(group, everything())

# ── Section 1: Wastewater data ────────────────────────────────────────────────

# Concentration (copies per mL)
ww_conc <- dat_ww_descrip %>%
  mutate(
    section   = "Wastewater data",
    sub_label = "Concentration\n(copies per mL)"
  ) %>%
  select(section, sub_label,
         county,
         col3 = n_sewersheds,
         col4 = n_samples,
         mean, min, median, max) %>%
  mutate(
    col3   = as.character(round(col3, 2)),
    col4   = as.character(round(col4, 2)),
    mean   = as.character(round(mean, 2)),
    min    = as.character(round(min, 2)),
    median = as.character(round(median, 2)),
    max    = as.character(round(max, 2))
  )

# Detection rate (percent)
ww_dr <- dat_descrip_DR %>%
  mutate(
    section   = "Wastewater data",
    sub_label = "Detection rate\n(percent)"
  ) %>%
  select(section, sub_label,
         county,
         col3 = n_sewersheds,
         col4 = n_samples,
         mean, min, median, max) %>%
  mutate(
    col3   = as.character(round(col3, 2)),
    col4   = as.character(round(col4, 2)),
    mean   = as.character(round(mean, 2)),
    min    = as.character(round(min, 2)),
    median = as.character(round(median, 2)),
    max    = as.character(round(max, 2))
  )

# Wastewater header row
ww_header <- tibble(
  section   = "Wastewater data",
  sub_label = "",
  county    = "County",
  col3      = "Number of\nsewersheds",
  col4      = "Number of\nsamples",
  mean      = "Mean",
  min       = "Min",
  median    = "Median",
  max       = "Max",
  is_header = TRUE
)

dat_ww_final <- bind_rows(ww_header, ww_conc, ww_dr) %>%
  mutate(is_header = ifelse(is.na(is_header), FALSE, is_header))

# ── Section 2: Case data ──────────────────────────────────────────────────────

# Case header row
case_header <- tibble(
  section   = "NRVESS Case Data",
  sub_label = "",
  county    = "County",
  col3      = "Weeks above 0\ncase reports",
  col4      = "Total cases\nreported",
  mean      = "Mean",
  min       = "Min",
  median    = "Median",
  max       = "Max",
  is_header = TRUE
)

dat_case_final <- dat_descrip_case %>%
  mutate(
    section   = "NRVESS Case Data",
    sub_label = ""
  ) %>%
  select(section, sub_label,
         county,
         col3 = `Weeks above 0 case reports`,
         col4 = `total cases reported`,
         mean, min, median, max) %>%
  mutate(is_header = FALSE) %>%
  mutate(
    col3   = as.character(round(col3, 2)),
    col4   = as.character(round(col4, 2)),
    mean   = as.character(round(mean, 2)),
    min    = as.character(round(min, 2)),
    median = as.character(round(median, 2)),
    max    = as.character(round(max, 2))
  )

dat_case_final <- bind_rows(case_header, dat_case_final)

# ── Section 3: Hospitalizations ───────────────────────────────────────────────

# Hospitalization header row
hosp_header <- tibble(
  section   = "Hospitalizations to treat RSV",
  sub_label = "",
  county    = "County",
  col3      = "Number of weeks\nabove 0 admissions",
  col4      = "Total admissions\nreported",
  mean      = "Mean",
  min       = "Min",
  median    = "Median",
  max       = "Max",
  is_header = TRUE
)

dat_hosp_final <- dat_descrip_hosp %>%
  mutate(
    section   = "Hospitalizations to treat RSV",
    sub_label = group
  ) %>%
  select(section, sub_label,
         county,
         col3 = `number of weeks above 0 admissions`,
         col4 = `total admissions reported`,
         mean, min, median, max) %>%
  mutate(is_header = FALSE) %>%
  mutate(
    col3   = as.character(round(col3, 2)),
    col4   = as.character(round(col4, 2)),
    mean   = as.character(round(mean, 2)),
    min    = as.character(round(min, 2)),
    median = as.character(round(median, 2)),
    max    = as.character(round(max, 2))
  )

dat_hosp_final <- bind_rows(hosp_header, dat_hosp_final)

# ── Combine all sections in correct order ─────────────────────────────────────
table_1 <- bind_rows(dat_ww_final,
                     dat_case_final,
                     dat_hosp_final) %>%
  # Move section label into sub_label on header rows BEFORE dropping section
  mutate(sub_label = ifelse(is_header == TRUE, section, sub_label)) %>%
  select(-section) %>%
  mutate_all(as.character) %>%
  mutate(across(everything(), ~ifelse(. == "NA" | is.na(.), "", .)))

dataframe <- table_1


# save
table_1_ft <- table_as_flex_rsv_function(
  dataframe = table_1, 
  title = 
    "Table 1: Descriptive statistics")
table_1_ft

# save as doc
save_as_docx(FitFlextableToPage(table_1_ft), 
             path = paste0("../figures and tables/Table 1_rev_",
                           " descriptive statistics.docx"))
