####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Figure S8  - age distribution

# AUTHOR - Dustin T. Hill

# Date Created: August 11, 2025

# LAST UPDATED - November 4, 2026

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

# packages
library(tigris)
library(tidycensus)
# guidance from tidycensus help manual
# https://walker-data.com/census-r/exploring-us-census-data-with-visualization.html

# pull age data
age_df <- get_estimates(
  geography = "county",
  state = "NY",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019 # most recent year with this data
)

# pilot county fips codes
county_fips <- c("36045", "36067",
                 "36029",
                 "36119")

# filter for pilot counties
age_df <- age_df %>%
  filter(GEOID %in% county_fips)

# check we have the right counties
table(age_df$NAME)

# filter for separate age groups, not combined fields
age_df_filtered <- filter(age_df, stringr::str_detect(AGEGROUP, "^Age"), 
                          SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

age_pyramid(age_df_filtered, AGEGROUP, split_by = SEX)+
  facet_wrap(~NAME)

age_plot <- 
  ggplot(age_df_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) + 
  geom_col(width = 0.95, alpha = 0.75) + 
  theme_dth_1 +
  scale_x_continuous(
    labels = ~ number_format(scale = .001, suffix = "k")(abs(.x)),
    limits = 40000 * c(-1,1)
  ) + 
  scale_y_discrete(labels = ~ stringr::str_remove_all(.x, "Age\\s|\\syears")) + 
  scale_fill_manual(values = met.brewer(name = "Hiroshige",
                                        n = 2)) + 
  labs(x = "", 
       y = "2019 Census Bureau population estimate", 
       title = "Population structure in Pilot counties", 
       fill = "", 
       caption = "Data source: US Census Bureau population estimates & tidycensus R package")+
  facet_wrap(~NAME)

# save
png("figures and tables/Figure - age pyramid.png",
    units = "in",
    width = 12, height = 9,
    res = 600)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 600)
age_plot
showtext_end()
dev.off()