
####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Figure 3 - panel for lag/lead time and time series

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

load(file = "data/rsv_combined_data.Rdata")
# export the table from the lag/lead function

# change week to date object
dat_county$week <- ymd(dat_county$week)

# Panel 1 - lags and leads

# all cases
lags_case_weekly <- lag_lead_function_df(
  dataframe = dat_county %>%
    filter(county == "Onondaga"), 
  steps = 8, 
  method = "spearman",
  ww_var = "rsv_ww_county", 
  clin_var = "rsv_cases_total",
  plot = "",
  clin_name = "Positive tests",
  county_group = "No"
) %>%
  mutate(group = "Positive tests",
         season = "All weeks")

# all hospitalizations
lags_hosp_weekly <- lag_lead_function_df(
  dataframe = dat_county, 
  steps = 8, 
  method = "spearman",
  ww_var = "rsv_ww_county", 
  clin_var = "rsv_hosp_all",
  plot = "",
  clin_name = "RSV hospitalizations",
  county_group = "No"
) %>%
  mutate(group = "All hospitalizations",
         season = "All weeks")

# under 10, all weeks
lags_10_weekly <- lag_lead_function_df(
  dataframe = dat_county , 
  steps = 8, 
  method = "spearman",
  ww_var = "rsv_ww_county", 
  clin_var = "rsv_hosp_10",
  plot = "",
  clin_name = "Under 10 hospitalizations",
  county_group = "No"
)%>%
  mutate(group = "Under 10 hospitalizations",
         season = "All weeks")

# over 50, all weeks
lags_50_weekly <- lag_lead_function_df(
  dataframe = dat_county, 
  steps = 8, 
  method = "spearman",
  ww_var = "rsv_ww_county", 
  clin_var = "rsv_hosp_50",
  plot = "",
  clin_name = "Over 50 hospitalizations",
  county_group = "No"
)%>%
  mutate(group = "Over 50 hospitalizations",
         season = "All weeks")


# all weeks dataframe
lag_all_weeks <- rbind(lags_case_weekly,
                       lags_hosp_weekly, lags_10_weekly, lags_50_weekly)

# plot
plot_lags_and_leads<-
  ggplot(data = lag_all_weeks %>%
           filter(!is.na(cor)) , 
         aes(x = as.numeric(as.character(lag))*-1, y = cor, color = group, group = group))+
  geom_vline(xintercept = 0,  color = "black",
             lwd = 1)+
  geom_line(lwd = 1)+
  geom_point(aes(shape = as.character(highest_lag)), size = 5)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(
    color = "",
    x = "Lag",
    y = "Spearman Correlation")+
  guides(shape = "none")+
  scale_shape_manual(values = c("1" = 17,
                                "0" = 1))+
  scale_color_manual(values = c("All hospitalizations" = pal[1],
                                "Positive tests" = pal[2],
                                "Over 50 hospitalizations" = "#e76254",
                                "Under 10 hospitalizations" = "#376795"),
                     name = "",
                     breaks=c( "Positive tests" ,
                               "All hospitalizations", "Over 50 hospitalizations",
                               "Under 10 hospitalizations"))+
  ylim(0,1)+
  annotate("text",
           x = -4,
           y = 0.25,
           label = "Wastewater signal\nleads incidence",
           size = 4)+
  annotate("text",
           x = 4,
           y = 0.25,
           label = "Wastewater signal\nlags incidence",
           size = 4)+
  scale_x_continuous(breaks = as.numeric(as.character(lag_all_weeks$lag)))


plot_lags_and_leads


# Panel 2 - time series

# Revised for one plot, no panel

# palette for lines
pal <- met.brewer("Hiroshige", 3)

pal2 <- met.brewer("Hiroshige", 7)

# pal2[1]  "#e76254"
# pal2[2] "#ef8a47"
# pal2[3] "#ffd06f"
# #"#ffe6b7"
# pal2[4] "#aadce0"
# pal2[5]"#72bcd5"
# pal2[6] "#376795"

plot <- ggplot()+
  geom_point(data = dat_county,
             aes(x = week, y = rsv_ww_county/3.5,
                 color = "Wastewater concentration"),
             alpha = 0.8)+
  geom_point(data = dat_county,
             aes(x = week, y = rsv_ww_county/3.5),
             shape = 1) +
  geom_smooth(data = dat_county,
              aes(x = week, y = rsv_ww_county/3.5,
                  color = "Wastewater concentration"),
              method = "loess",
              span = 0.08,
              lwd = 1.5,
              alpha = 0.2,
              se = FALSE)+
  geom_smooth(data = dat_county,
              aes(x = week, y = rsv_hosp_all_100, color = "All hospitalizations"),
              method = "loess",
              span = 0.08,
              lwd = 1.5,
              alpha = 0.2,
              se = FALSE)+
  geom_smooth(data = dat_county,
              aes(x = week, y = rsv_hosp_10_100/3, color = "Under 10 hospitalizations"),
              method = "loess",
              span = 0.08,
              lwd = 1.5,
              alpha = 0.2,
              se = FALSE)+
  geom_smooth(data = dat_county,
              aes(x = week, y = rsv_hosp_50_100, color = "Over 50 hospitalizations"),
              method = "loess",
              span = 0.08,
              lwd = 1.5,
              alpha = 0.2,
              se = FALSE)+
  geom_smooth(data = dat_county,
              aes(x = week, y = rsv_cases_total_100/2, color = "Positive tests"),
              method = "loess",
              span = 0.08,
              lwd = 1.5,
              alpha = 0.2,
              se = FALSE)+
  theme_dth_1+
  theme(legend.position = "bottom",
        legend.background = element_blank())+
  scale_color_manual(values = c("All hospitalizations" = pal[1],
                                "Positive tests" = pal[2],
                                "Wastewater concentration" = pal[3],
                                "Over 50 hospitalizations" = "#e76254",
                                "Under 10 hospitalizations" = "#376795"),
                     name = "",
                     breaks=c("Wastewater concentration", "Positive tests" ,
                              "All hospitalizations", "Over 50 hospitalizations",
                              "Under 10 hospitalizations")
  )+
  labs(x = "")+
  scale_y_continuous(
    "RSV hospital incidence (per 100k)", 
    sec.axis = sec_axis(~ .*3.5, name = "RSV wastewater concentartion (copies/mL)")
  )+
  theme(strip.text = element_text(hjust = 0))+
  scale_x_date(
    date_breaks = "3 months", # Set breaks every 3 months
    date_labels = "%b %Y"     # Format labels as abbreviated month and full year
  )

plot

# combine plot
panel <- plot_grid(plot_lags_and_leads + theme(legend.position = "none"),
                   plot + theme(legend.position = "none"),
                   nrow = 1,
                   rel_widths = c(1.5,2),
                   align = "h", axis = "bt",
                   labels = c("A", "B"))

# one legend
# get legend
legend <- get_legend2(plot)

# combine
plot_grid(panel, legend,
          ncol = 1,
          rel_heights = c(2, 0.1))

# save
png("figures and tables/Figure - lag and lead panel with time series.png",
    units = "in",
    width = 12, height = 6,
    res = 600)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 600)
plot_grid(panel, legend,
          ncol = 1,
          rel_heights = c(2, 0.1))
showtext_end()
dev.off()
