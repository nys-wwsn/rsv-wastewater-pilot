####
#### RSV WASTEWATER DATA ANALYSIS
####

# Data prep script
# This script loads and preprocesses RSV ww concentration data, hospitalization,
# and case data for analysis.

# AUTHOR - Dustin T. Hill

# Date Created: August 8, 2025

# LAST UPDATED - November 4, 2026

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

# -------------------------------------
# Hospitalizations data processing
# -------------------------------------

# all RSV hosp admissions 2022-2024 geocoded to sewershed
hosp_sewer_all <- read.csv(
  "../Data/RSV/Data pull June 2025 - split by age/RSV_All_HOSP_2022_2024_sewershed.csv",
  stringsAsFactors = FALSE
) %>%
  clean_names()

# select fields to keep and rename
hosp_sewer_all <- hosp_sewer_all %>%
  select(
    county, sw_id, admit_dt, rsv_hosp_1
  ) %>%
  rename(rsv_hosp_all = rsv_hosp_1
  ) %>%
  mutate(date = ymd(admit_dt))%>%
  select(-admit_dt)

# all RSV hosp admissions 50 and over 2022-2024 geocoded to sewershed
hosp_sewer_50 <- read.csv(
  "../Data/RSV/Data pull June 2025 - split by age/RSV_Greater_50_HOSP_2022_2024_sewershed.csv",
  stringsAsFactors = FALSE
) %>%
  clean_names()

# select fields to keep and rename
hosp_sewer_50 <- hosp_sewer_50 %>%
  select(
    county, sw_id, admit_dt, rsv_hosp_1
  ) %>%
  rename(rsv_hosp_50 = rsv_hosp_1
  ) %>%
  mutate(date = ymd(admit_dt))%>%
  select(-admit_dt)

# all RSV hosp admissions 10 and under 2022-2024 geocoded to sewershed
hosp_sewer_10 <- read.csv(
  "../Data/RSV/Data pull June 2025 - split by age/RSV_Less_10_HOSP_2022_2024_sewershed.csv",
  stringsAsFactors = FALSE
) %>%
  clean_names()

# select fields to keep and rename
hosp_sewer_10 <- hosp_sewer_10 %>%
  select(
    county, sw_id, admit_dt, rsv_hosp_1
  ) %>%
  rename(rsv_hosp_10 = rsv_hosp_1
  ) %>%
  mutate(date = ymd(admit_dt))%>%
  select(-admit_dt)

# combine into one sewershed hosp dataframe
hosp_sewer <- left_join(hosp_sewer_all, hosp_sewer_50,
                        by = c("date", "county", "sw_id"))
hosp_sewer <- left_join(hosp_sewer, hosp_sewer_10,
                        by = c("date", "county", "sw_id"))

# group by week and sum hospital admissions
hosp_sewer_weekly <- hosp_sewer %>%
  group_by(sw_id, 
           week  = floor_date(date, unit = "weeks", week_start = 7)) %>%
  summarize(
    rsv_hosp_all = sum(rsv_hosp_all, na.rm = TRUE),
    rsv_hosp_50 = sum(rsv_hosp_50, na.rm = TRUE),
    rsv_hosp_10 = sum(rsv_hosp_10, na.rm = TRUE)
  ) %>%
  ungroup()

######################################
# COUNTY LEVEL HOSPITALIZATION DATA
######################################

# all RSV hosp admissions 2022-2024 geocoded to county
hosp_county_all <- read.csv(
  "../Data/RSV/Data pull June 2025 - split by age/RSV_All_HOSP_2022_2024_county.csv",
  stringsAsFactors = FALSE
) %>%
  clean_names()

# select fields to keep and rename
hosp_county_all <- hosp_county_all %>%
  select(
    county, admit_dt, rsv_hosp_1
  ) %>%
  rename(rsv_hosp_all = rsv_hosp_1
  ) %>%
  mutate(date = ymd(admit_dt))%>%
  select(-admit_dt)

# all RSV hosp admissions 50 and over 2022-2024 geocoded to county
hosp_county_50 <- read.csv(
  "../Data/RSV/Data pull June 2025 - split by age/RSV_Greater_50_HOSP_2022_2024_county.csv",
  stringsAsFactors = FALSE
) %>%
  clean_names()

# select fields to keep and rename
hosp_county_50 <- hosp_county_50 %>%
  select(
    county, admit_dt, rsv_hosp_1
  ) %>%
  rename(rsv_hosp_50 = rsv_hosp_1
  ) %>%
  mutate(date = ymd(admit_dt))%>%
  select(-admit_dt)

# all RSV hosp admissions 10 and under 2022-2024 geocoded to county
hosp_county_10 <- read.csv(
  "../Data/RSV/Data pull June 2025 - split by age/RSV_Less_10_HOSP_2022_2024_county.csv",
  stringsAsFactors = FALSE
) %>%
  clean_names()

# select fields to keep and rename
hosp_county_10 <- hosp_county_10 %>%
  select(
    county, admit_dt, rsv_hosp_1
  ) %>%
  rename(rsv_hosp_10 = rsv_hosp_1
  ) %>%
  mutate(date = ymd(admit_dt))%>%
  select(-admit_dt)

# combine into one county hospitalization file
hosp_county <- left_join(hosp_county_all,
                         hosp_county_50,
                         by = c("county", "date"))
hosp_county <- left_join(hosp_county,
                         hosp_county_10,
                         by = c("county", "date"))

# group by week and sum hospital admissions
hosp_county_weekly <- hosp_county %>%
  group_by(county, 
           week  = floor_date(date, unit = "weeks", week_start = 7)) %>%
  summarize(
    rsv_hosp_all = sum(rsv_hosp_all, na.rm = TRUE),
    rsv_hosp_50 = sum(rsv_hosp_50, na.rm = TRUE),
    rsv_hosp_10 = sum(rsv_hosp_10, na.rm = TRUE)
  ) %>%
  ungroup()

write.csv(hosp_county_weekly, "Data/rsv-hospitalizations-county.csv",
          row.names = FALSE)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# -------------------------------------
# Case data processing
# -------------------------------------

# raw case data by institution
rsv_cases <- read_xlsx("../Data/RSV/NREVSS 2012-May 2026.wastewater.xlsx") %>%
  janitor::clean_names()

# Case data are at the facility level. First, sum data by week and facility
# Then geocode facilities to assign to sewersheds and counties

# group by facility, week, coordinates and sum 
rsv_cases_summarized <- rsv_cases %>%
  group_by(date = mdy(week_ending), lab_id, inst, city, state,
           week  = floor_date(date, unit = "weeks", week_start = 7)) %>%
  summarize(rsv_tests = sum(rsv_test, na.rm = TRUE),
            rsva_cases = sum(rsva_pos, na.rm = TRUE),
            rsvb_cases = sum(rsvb_pos, na.rm = TRUE),
            rsvunk_cases = sum(rsvunk_pos, na.rm = TRUE)
  ) %>%
  ungroup()

# sum across positive results because ww does not have distinction 
# (note, we can test the individuals if correlations are poor)
rsv_cases_summarized <- rsv_cases_summarized %>%
  mutate(rsv_cases_total = rsva_cases + rsvb_cases + rsvunk_cases)

# geocode inst, then if NA latitude, geocode address

rsv_cases_summarized <- rsv_cases_summarized %>%
  geocode(
    inst,
    method = "osm",
    lat = latitude,
    long = longitude
  )

# geocode address
rsv_cases_summarized <- rsv_cases_summarized %>%
  mutate(address = paste(city, state, sep = ", ")) %>%
  geocode(address, method = "osm", lat = city_latitude, long = city_longitude)

# if latitude is missing for initial geocode, replace with city address
rsv_cases_summarized$latitude <- ifelse(is.na(rsv_cases_summarized$latitude), 
                                        rsv_cases_summarized$city_latitude, 
                                        rsv_cases_summarized$latitude
)
rsv_cases_summarized$longitude <- ifelse(is.na(rsv_cases_summarized$longitude), 
                                         rsv_cases_summarized$city_longitude,
                                         rsv_cases_summarized$longitude)

# replace the women and children's hospital of buffalo with city cooridanates / 
# also Mount Sinai hospital since they did not geocode properly
rsv_cases_summarized$latitude <- ifelse(rsv_cases_summarized$lab_id == "002053", 
                                        rsv_cases_summarized$city_latitude,
                                        rsv_cases_summarized$latitude
)
rsv_cases_summarized$longitude <- ifelse(rsv_cases_summarized$lab_id == "002053", 
                                         rsv_cases_summarized$city_longitude,
                                         rsv_cases_summarized$longitude)

rsv_cases_summarized$latitude <- ifelse(rsv_cases_summarized$lab_id == "020341", 
                                        rsv_cases_summarized$city_latitude,
                                        rsv_cases_summarized$latitude
)
rsv_cases_summarized$longitude <- ifelse(rsv_cases_summarized$lab_id == "020341", 
                                         rsv_cases_summarized$city_longitude,
                                         rsv_cases_summarized$longitude)

# change the data to sf object for mapping and intersecting with sewersheds
rsv_cases_sf <- rsv_cases_summarized %>%
  st_as_sf(coords =  c("longitude", "latitude"),
           crs = "+proj=longlat +zone=18 +datum=WGS84")

# county boundaries
ny_counties <- st_read(paste0("C:/Users/dthill/OneDrive - Syracuse University/",
                              "spatial data/Counties-Shoreline/",
                              "Counties_Shoreline.shp"))

# transform to match crs of counties
rsv_cases_sf <- st_transform(rsv_cases_sf, st_crs(ny_counties))

# plot the points to make sure they geocoded properly to NYS
map_1 <- 
  ggplot()+
  geom_sf(data = ny_counties, fill = "white")+
  geom_sf(data = rsv_cases_sf, size = 3, alpha = 0.5) +
  theme_void()+
  labs(title = "NREVSS Testing Sites 2013 to 2024")+
  theme(plot.title = element_text(hjus = 0.5))

map_1


# assign the points to counties
rsv_cases_county<- st_intersection(rsv_cases_sf, ny_counties)

# sum by county
rsv_cases_county <- rsv_cases_county %>%
  group_by(NAME, date, week) %>%
  summarize(
    rsv_tests = sum(rsv_tests, na.rm = TRUE),
    rsva_cases = sum(rsva_cases, na.rm = TRUE),
    rsvb_cases = sum(rsvb_cases, na.rm = TRUE),
    rsvunk_cases = sum(rsvunk_cases, na.rm = TRUE),
    rsv_cases_total = sum(rsv_cases_total, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  rename(county = NAME)

# drop coordinate reference
rsv_cases_county <-  st_drop_geometry(rsv_cases_county)

# group by week and sum
rsv_cases_county_weekly <- rsv_cases_county %>%
  group_by(county, week) %>%summarize(
    rsv_tests = sum(rsv_tests, na.rm = TRUE),
    rsva_cases = sum(rsva_cases, na.rm = TRUE),
    rsvb_cases = sum(rsvb_cases, na.rm = TRUE),
    rsvunk_cases = sum(rsvunk_cases, na.rm = TRUE),
    rsv_cases_total = sum(rsv_cases_total, na.rm = TRUE)
  ) %>%
  ungroup()

# save
write.csv(rsv_cases_county_weekly, "Data/rsv-cases-county.csv",
          row.names = FALSE)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# -------------------------------------
# Wastewater data processing
# -------------------------------------
# 
# # Full concentration dataset to add crassphage/PMMoV
conc <- read.csv("../Data/2026-10-21_concentration.csv")

# wastewater concentration data
rsv_ww <- read.csv("../Data/2025-01-08_fluA_rsv.csv") %>%
  mutate(date = collection_date)

# ww meta data load
meta <- read.csv(paste0("../Data/",
                        "nys-wws-sewersheds.csv")) %>%
  mutate(facility_id = cdc_id)

min_date <- min(ymd(rsv_ww$collection_date), na.rm = TRUE)
max_date <- max(ymd(rsv_ww$collection_date), na.rm = TRUE)

# --- duplicate ww data exploration ---

dups_raw <- rsv_ww %>%
  group_by(wwl_cdc_id) %>%
  filter(duplicated(collection_date)) %>%
  select(wwl_cdc_id, collection_date)

dups_raw <- left_join(dups_raw, rsv_ww, by = c("wwl_cdc_id", "collection_date"))

# --- deal with duplicate data (avg) ---

# ww data - average per sample day, if >0, set to "detect"
rsv_ww <- rsv_ww %>%
  group_by(collection_date,wwl_cdc_id) %>%
  summarize(rsv_concentration_copies_u_l = mean(rsv_concentration_copies_u_l, na.rm = TRUE),
  ) %>%
  mutate(rsv_result = ifelse(rsv_concentration_copies_u_l >0, "DETECTED", "Not Detected")) %>%
  ungroup()

# --- convert ub and quadrant data ---

# Data were not provided as concentration values, so converting back

# join in county names
meta_c <- meta %>% select(facility_id, county)

# UB data multiply by 5.1282
# Quadrant data multiply by 2.5
rsv_ww <- rsv_ww %>%
  mutate(facility_id = wwl_cdc_id) %>%
  left_join(meta, by = c("facility_id")) %>%
  mutate(rsv_concentration_copies_m_l =
           ifelse(
             county == "Erie",
             rsv_concentration_copies_u_l * 5.1282,
             rsv_concentration_copies_u_l * 2.5
           ))

# --- weekly aggregation: mean per week ---

# drop 1 sampleing location that was not part of the pilot
# 36045NY0099091JCWWV
# ww data 
rsv_ww_fill <- rsv_ww %>%
  dplyr::select(collection_date, wwl_cdc_id, rsv_result, 
                rsv_concentration_copies_m_l) %>%
  mutate(date = ymd(collection_date)) %>%
  select(-collection_date) %>%
  mutate(
    cdc_id = wwl_cdc_id) %>%
  rename(facility_id = wwl_cdc_id) %>%
  group_by(cdc_id) %>%
  mutate(week  = floor_date(date, unit = "weeks", week_start = 7)
  ) %>%
  # removing sites that were not part of pilot
  # less than 15 samples collected
  filter(cdc_id != "NY045099091A") %>%
  filter(cdc_id != "NY029032093A") %>%
  filter(cdc_id != "NY045030686A") %>%
  filter(cdc_id != "NY029028436A")

# --- add fecal data back in ---

# grab the data to merge (fecal only / flow data are almost all manual imputations)
fecal <- conc %>%
  select(
    sample_collect_date,
    facility_id,
    hum_frac_mic_copies_ml,
    hum_frac_mic_target,
    sample_type,
    rec_eff_percent
  ) %>%
  # rename facility id
  mutate(date = ymd(sample_collect_date))

rsv_ww_fill <- left_join(rsv_ww_fill,
                         fecal,
                         by = c("facility_id", "date"))

# filter for pilot counties only
pilot <- c("Westchester", "Erie", "Jefferson", "Onondaga")

pilot_meta <- meta_c %>%
  filter(county %in% pilot)

rsv_ww_fill <- rsv_ww_fill %>%
  filter(facility_id %in% pilot_meta$facility_id)

# sample types
table(rsv_ww_fill$sample_type)

# Add fecal normalized data
table(rsv_ww_fill$hum_frac_mic_target)

# Normalize by fecal indicator
rsv_ww_fill$rsv_copies_fecal_norm <- rsv_ww_fill$rsv_concentration_copies_m_l / rsv_ww_fill$hum_frac_mic_copies_ml

# group by week, calculate average RSV detection
rsv_ww_weekly <- rsv_ww_fill %>%
  group_by(week, facility_id) %>%
  #filter(!is.na(hum_frac_mic_copies_ml)) %>%
  summarize(
    rsv_ww_mean = mean(rsv_concentration_copies_m_l, na.rm = TRUE),
    rsv_fecal_norm_mean = mean(rsv_copies_fecal_norm, na.rm = TRUE),
    n_samples = sum(!is.na(rsv_concentration_copies_m_l)),
    positives = sum(rsv_result == "DETECTED")
  )

# summary of the data
summary(rsv_ww_weekly)

# filter for pilot counties only
pilot <- c("Westchester", "Erie", "Jefferson", "Onondaga")

pilot_meta <- meta_c %>%
  filter(county %in% pilot)

rsv_ww_weekly <- rsv_ww_weekly %>%
  filter(facility_id %in% pilot_meta$facility_id)

# --- county level ww data - pop weighted mean ---

# county metadata
meta_c <- meta %>% select(facility_id, county, population_served)

# county pop weighted mean
rsv_ww_county <-rsv_ww_weekly %>%
  left_join(meta_c, by = c("facility_id"))%>%
  group_by(county, week) %>%
  summarize(
    rsv_ww_county = weighted.mean(x = rsv_ww_mean,
                                  w = population_served),
    rsv_ww_fecal_norm_county = weighted.mean(
      x = rsv_fecal_norm_mean,
      w = population_served
    ),
    n_sewersheds = length(unique(facility_id)),
    n_samples = sum(n_samples, na.rm = TRUE),
    n_positives = sum(positives, na.rm = TRUE)
  )

# select pilot counties only
rsv_ww_county <- rsv_ww_county %>%
  filter(county %in% pilot)

# save
write.csv(rsv_ww_county, "Data/rsv-wastewater-county.csv",
          row.names = FALSE)

# ---------------------------------------------------------------------
# COMBINE DATA INTO SUMMARIZED DATASETS FOR SEWERSHED AND COUNTY
# ---------------------------------------------------------------------

# add facility id to the sewershed hosp data
meta_ids <- meta %>%
  select(facility_id, sw_id,
         population_served,
         county,
         capacity_mgd,
         wwtp_latitude,
         wwtp_longitude)

# join to the rsv ww data for sewersheds
rsv_ww_weekly <-left_join(
  rsv_ww_weekly,
  meta_ids, by = c("facility_id")
)

# sewershed data
dat_sewershed <- left_join(rsv_ww_weekly,
                           hosp_sewer_weekly,
                           by = c("week",
                                  "sw_id"))

# replace missing data with 0 for hosp admissions
dat_sewershed$rsv_hosp_10 <- ifelse(is.na(dat_sewershed$rsv_hosp_10),
                                    0,
                                    dat_sewershed$rsv_hosp_10)
dat_sewershed$rsv_hosp_all <- ifelse(is.na(dat_sewershed$rsv_hosp_all),
                                     0,
                                     dat_sewershed$rsv_hosp_all)
dat_sewershed$rsv_hosp_50 <- ifelse(is.na(dat_sewershed$rsv_hosp_50),
                                    0,
                                    dat_sewershed$rsv_hosp_50)

# add per 100k adjustments
dat_sewershed$rsv_hosp_10_100 <- dat_sewershed$rsv_hosp_10  * 
  (100000)/dat_sewershed$population_served
dat_sewershed$rsv_hosp_all_100 <- dat_sewershed$rsv_hosp_all  * 
  (100000)/dat_sewershed$population_served
dat_sewershed$rsv_hosp_50_100 <- dat_sewershed$rsv_hosp_50  * 
  (100000)/dat_sewershed$population_served

# county age adjusted incidence using population of the age groups


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
county_region <- read.csv("Data/County_Region.csv") %>%
  dplyr::select(region, county, county_pop)%>%
  rename(Region = region,
         County_pop = county_pop)

dat_county <- left_join(dat_county,
                        county_region,
                        by = c("county"))
# add per 100k adjustments
dat_county$rsv_hosp_all_100 <- dat_county$rsv_hosp_all  * 
  (100000)/dat_county$County_pop

# Add age adjusted per 100k 
pop_data <- read.csv(
  paste0("Data/", "ny_pop_numbers.csv")) %>%
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
dat_sewershed$week_number <- strftime(dat_sewershed$week, format = "%V")
dat_county$week_number <- strftime(dat_county$week, format = "%V")

# add season based on week number
dat_sewershed$season <- ifelse(dat_sewershed$week_number >= 40 | dat_sewershed$week_number <= 20,
                               "Weeks 40 to 20", "Weeks 21 to 39")
dat_county$season <- ifelse(dat_county$week_number >= 40 | dat_county$week_number <= 20,
                            "Weeks 40 to 20", "Weeks 21 to 39")

# --- save combined files ---
save(dat_sewershed,
     dat_county,
     file = "../Data/rsv_combined_data.Rdata")
