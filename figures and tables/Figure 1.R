####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Figure 1  - map

# AUTHOR - Dustin T. Hill

# Date Created: August 11, 2025

# LAST UPDATED - November 4, 2025

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
library(cowplot)

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

load(file = "../Data/rsv_combined_data.Rdata")

# Add seasons and pilot county objects

# list of pilot counties
pilot <- c("Jefferson", "Erie", "Onondaga", "Westchester")
pilot_cases <- c("Jefferson", "Onondaga")
onondaga <- c("Onondaga")

# spatial data
ny_counties <- st_read(paste0("C:/Users/dthill/OneDrive - Syracuse University/",
                              "spatial data/Counties-Shoreline/",
                              "Counties_Shoreline.shp"))


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# ----------------------------
# Map of sampling locations
# ----------------------------

# summary of all weeks
sewershed_summary <- dat_sewershed %>%
  group_by(facility_id, wwtp_latitude, wwtp_longitude, county, capacity_mgd) %>%
  summarize(
    n_samples = sum(n_samples, na.rm = TRUE),
    n_positives = sum(positives, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(`Detection rate` = n_positives / n_samples) %>%
  rename(`Sewershed size` = capacity_mgd) %>%
  filter(`Detection rate` > 0)

# make the object spatial for mapping the points
sewershed_summary <- sewershed_summary %>%
  st_as_sf(coords =  c("wwtp_longitude", "wwtp_latitude"),
           crs = "+proj=longlat +zone=18 +datum=WGS84") %>% 
  st_transform(st_crs(ny_counties))

# categories for sewershed size and for detection rate
summary(sewershed_summary$`Sewershed size`)
summary(sewershed_summary$`Detection rate`)

sewershed_summary <- sewershed_summary %>%
  mutate(
    `Detection rate` = case_when(
      `Detection rate` < 0.3 ~ "< 30%",
      `Detection rate` >= 0.3 & `Detection rate` <= 0.4 ~ "30% to 40%",
      `Detection rate` > 0.4 & `Detection rate` <= 0.5 ~ "40% to 50%",
      `Detection rate` > 0.5 ~ "> 50%"
    ) 
  )%>%
  mutate(`Detection rate` = factor(`Detection rate`,
                                   levels = c("< 30%",
                                              "30% to 40%",
                                              "40% to 50%",
                                              "> 50%"))
  ) %>%
  mutate(`Sewershed size` = case_when(
    `Sewershed size` < 6 ~ "< 6 mgd",
    `Sewershed size` >= 6 & `Sewershed size` <= 25 ~ "6 mgd to 25 mgd",
    `Sewershed size` > 25 & `Sewershed size` <= 100 ~ "25 mgd to 100 mgd",
    `Sewershed size` > 100 ~ "> 100 mgd"
  )) %>%
  mutate(`Sewershed size` = factor(`Sewershed size`,
                                   levels = c("< 6 mgd",
                                              "6 mgd to 25 mgd",
                                              "25 mgd to 100 mgd",
                                              "> 100 mgd")))


table(sewershed_summary$`Detection rate`)
table(sewershed_summary$`Sewershed size`)

# Four maps, one for each county, plotted together
# maps as one plot, line graphs as another plot
jefferson_map <- map_function(county_name = "Jefferson")
erie_map <- map_function(county_name = "Erie")
onondaga_map <- map_function(county_name = "Onondaga")
westchester_map <- map_function(county_name = "Westchester")

# make them a panel
map_panel <- ggarrange(jefferson_map, 
                       erie_map,
                       onondaga_map,
                       westchester_map,
                       nrow = 2, ncol = 2,
                       labels = c("A", "B", "C", "D"))


# example map to format legend
# legend  capture
map_leg <- 
  ggplot()+
  annotation_map_tile(zoom = 9,
                      type = "osm") +
  geom_sf(data = ny_counties %>%
            filter(NAME == "Erie"),
          fill = "NA", color = "black",
          lwd = 1.2)+
  geom_sf(data = sewershed_summary %>%
            filter(county == "Erie"), 
          aes(color = `Detection rate`,  size = `Sewershed size`),
          alpha = 0.7) +
  geom_sf(data = sewershed_summary %>%
            filter(county == "Erie"),
          aes(size = `Sewershed size`),
          shape = 1)+
  scale_color_manual(name = "Detection rate (%)",
                     values = c("< 30%" = pal[4],
                                "30% to 40%" = pal[3],
                                "40% to 50%" = pal[2],
                                "> 50%" = pal[1])
  )+
  scale_size_discrete(range = c(6, 14))+
  theme_dth_maps+
  labs(title = paste("Erie", " County", sep = ""),
       size = "Sewershed size\n(mgd)")+
  theme(legend.position = "bottom",
        legend.background = element_blank())+
  guides(color = guide_legend(nrow = 2, byrow = TRUE,
                              title.position="top", title.hjust = 0.5,
                              override.aes = list(size=10)))+
  guides(size = guide_legend(nrow = 2, byrow = TRUE,
                             title.position="top", title.hjust = 0.5))



# get legend
legend <- get_legend2(map_leg)

# save
png("figures and tables/Figure 1.png",s
    units = "in",
    width = 6, height = 7,
    res = 600)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 600)
plot_grid(map_panel, legend,
          ncol = 1,
          rel_heights = c(1, 0.3))
showtext_end()
dev.off()
