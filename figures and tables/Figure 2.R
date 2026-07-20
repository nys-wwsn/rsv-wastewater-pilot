####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Figure 2 - time series plots

# AUTHOR - Dustin T. Hill

# Date Created: November 12, 2025

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

# make week a date vector
dat_county$week <- ymd(dat_county$week)

# Add seasons and pilot county objects

# list of pilot counties
pilot <- c("Jefferson", "Erie", "Onondaga", "Westchester")
pilot_cases <- c("Jefferson", "Onondaga")
onondaga <- c("Onondaga")

# time series plot function

time_plot_function <- function(county_name,
                               span_case,
                               span_ww,
                               span_hosp,
                               case_yes,
                               scale_hosp,
                               scale_case,
                               scale_number){
  pal <- met.brewer(name = "Hiroshige",
                    n = 3)
  
  dat_county$rsv_ww_county <- ifelse(dat_county$county == "Onondaga" &
                                        dat_county$rsv_ww_county > 25,
                                      NA,
                                      dat_county$rsv_ww_county)
  
  if(case_yes == "No"){
    # temporal plot (curves for ww, cases, and hospitalizations)
    plot <- 
      ggplot(data = dat_county %>%
               filter(county == county_name))+
      geom_point(aes(x = week, y = rsv_ww_county,
                     color = "Wastewater concentration"))+
      geom_point(aes(x = week, y = rsv_ww_county),
                 shape = 1)+
      geom_smooth(aes(x = week, y = rsv_ww_county,
                      color = "Wastewater concentration",
                      #fill =  "Wastewater concentration",
                      #linetype = "Wastewater concentration"
      ),
      span = span_ww,
      se = FALSE,
      size = 1.5)+
      geom_smooth(data = dat_county%>%
                    filter(county == county_name),
                  aes(x = week, y = rsv_hosp_all_100/scale_hosp,
                      color = "RSV hospital admissions",
                      #fill = "RSV hospital admissions",
                      #linetype = "RSV hospital admissions"
                  ),
                  span = span_hosp,
                  se = FALSE,
                  size = 1.5)+
      theme_dth_1+
      labs(title = paste(county_name, " County", sep = ""),
           x = "")+
      scale_y_continuous(
        "RSV wastewater concentration (copies/mL)", 
        sec.axis = sec_axis(~ .* scale_number, name = "RSV Positive tests/RSV hospital admissions")
      )+
      theme(legend.position = "none",
            axis.title = element_blank())+
      scale_color_manual(values = c("RSV hospital admissions" = pal[1],
                                    "Positive tests" = pal[2],
                                    "Wastewater concentration" = pal[3],
                                    name = ""))+
      # scale_fill_manual(values = met.brewer(name = "Hiroshige",
      #                                       n = 3),
      #                   name = "")+
      # scale_linetype_manual(values = c("dashed", "twodash", "solid"),
      #                       name = "")+
      theme(
        axis.ticks = element_line(size = 0.5, color = "black"))+
      scale_x_date(date_labels = "%b %Y")
    
    return(plot)
  } else if(case_yes == "Yes"){
    
    dat_county$rsv_cases_total_100 <- ifelse(
      dat_county$county == "Jefferson" & dat_county$week >= "2023-06-01",
      NA, 
      dat_county$rsv_cases_total_100
    )
    
    # temporal plot (curves for ww, cases, and hospitalizations)
    plot <- 
      ggplot(data = dat_county %>%
               filter(county == county_name))+
      geom_point(aes(x = week, y = rsv_ww_county,
                     color = "Wastewater concentration"))+
      geom_point(aes(x = week, y = rsv_ww_county),
                 shape = 1)+
      geom_smooth(aes(x = week, y = rsv_ww_county,
                      color = "Wastewater concentration",
                      #fill =  "Wastewater concentration",
                      #linetype = "Wastewater concentration"
      ),
      span = span_ww,
      se = FALSE,
      size = 1.5)+
      geom_smooth(data = dat_county%>%
                    filter(county == county_name),
                  aes(x = week, y = rsv_cases_total_100/scale_case,
                      color = "Positive tests",
                      #fill = "RSV hospital admissions",
                      #linetype = "RSV hospital admissions"
                  ),
                  span = span_hosp,
                  se = FALSE,
                  size = 1.5)+
      geom_smooth(data = dat_county%>%
                    filter(county == county_name),
                  aes(x = week, y = rsv_hosp_all_100/scale_hosp,
                      color = "RSV hospital admissions",
                      #fill = "RSV hospital admissions",
                      #linetype = "RSV hospital admissions"
                  ),
                  span = span_hosp,
                  se = FALSE,
                  size = 1.5)+
      theme_dth_1+
      labs(title = paste(county_name, " County", sep = ""),
           x = "")+
      scale_y_continuous(
        "RSV wastewater concentration (copies/mL)", 
        sec.axis = sec_axis(~ .* scale_number, name = "RSV Positive tests/RSV hospital admissions")
      )+
      theme(legend.position = "none",
            axis.title = element_blank())+
      scale_color_manual(values = c("RSV hospital admissions" = pal[1],
                                    "Positive tests" = pal[2],
                                    "Wastewater concentration" = pal[3],
                                    name = ""))+
      # scale_fill_manual(values = met.brewer(name = "Hiroshige",
      #                                       n = 3),
      #                   name = "")+
      # scale_linetype_manual(values = c("dashed", "twodash", "solid"),
      #                       name = "")+
      theme(
        axis.ticks = element_line(size = 0.5, color = "black"))+
      scale_x_date(date_labels = "%b %Y")
    
    return(plot)
  }
  
}

# made with incident data

# make each figure
jefferson_time <- time_plot_function(
  county_name = "Jefferson",
  span_case = 0.25,
  span_hosp = 0.25,
  span_ww = 0.25,
  case_yes = "Yes",
  scale_case =1,
  scale_hosp = 1,
  scale_number =1
)

jefferson_time

erie_time <- time_plot_function(
  county_name = "Erie",
  span_case = 0,
  span_hosp = 0.25,
  span_ww = 0.25,
  case_yes = "No",
  scale_number = 0.1,
  scale_hosp = 0.1
)

erie_time

onondaga_time <- time_plot_function(
  county_name = "Onondaga",
  span_case = 0.25,
  span_hosp = 0.25,
  span_ww = 0.25,
  case_yes = "Yes",
  scale_number = 1,
  scale_hosp = 1,
  scale_case = 5
)

onondaga_time

westchester_time <- time_plot_function(
  county_name = "Westchester",
  span_case = 0.25,
  span_hosp = 0.1,
  span_ww = 0.25,
  case_yes = "No",
  scale_number = 0.5,
  scale_hosp = 0.5
)

westchester_time

# panel
time_panel <- ggarrange(jefferson_time, 
                        erie_time,
                        onondaga_time,
                        westchester_time,
                        nrow = 2, ncol = 2,
                        labels = c("A", "B", "C", "D"))


# legend  capture
plot <- 
  ggplot(data = dat_county )+
  geom_point(aes(x = week, y = rsv_ww_county,
                 color = "Wastewater concentration"))+
  geom_point(aes(x = week, y = rsv_ww_county),
             shape = 1)+
  geom_smooth(aes(x = week, y = rsv_ww_county,
                  color = "Wastewater concentration",
                  #fill =  "Wastewater concentration",
                  #linetype = "Wastewater concentration"
  ),
  span = 0.25,
  se = FALSE,
  size = 1.5)+
  geom_smooth(data = dat_county,
              aes(x = week, y = rsv_cases_total/10,
                  color = "Positive tests",
                  #fill =  "Positive tests",
                  #linetype = "Positive tests"
              ),
              span = 0.25,
              se = FALSE,
              size = 1.5)+
  geom_smooth(data = dat_county,
              aes(x = week, y = rsv_hosp_all/10,
                  color = "RSV hospital admissions",
                  #fill = "RSV hospital admissions",
                  #linetype = "RSV hospital admissions"
              ),
              span = 0.25,
              se = FALSE,
              size = 1.5)+
  theme_dth_1+
  labs(title = paste("Erie", " County", sep = ""),
       x = "")+
  scale_y_continuous(
    "RSV wastewater concentration (copies/mL)", 
    sec.axis = sec_axis(~ .* 10, name = "RSV Positive tests/RSV hospital admissions")
  )+
  scale_color_manual(values = c("RSV hospital admissions" = pal[1],
                                "Positive tests" = pal[2],
                                "Wastewater concentration" = pal[3]),
                     name = "")+
  # scale_fill_manual(values = met.brewer(name = "Hiroshige",
  #                                       n = 3),
  #                   name = "")+
  # scale_linetype_manual(values = c("dashed", "twodash", "solid"),
  #                       name = "")+
  theme(
    axis.ticks = element_line(size = 0.5, color = "black"))+
  scale_x_date(date_labels = "%b %Y")+
  theme(legend.position = "bottom",
        legend.background = element_blank())+
  facet_wrap(~county)



# get legend
legend <- get_legend2(plot)

# add labels to x and y axis
panel_2 <- as_ggplot(arrangeGrob(time_panel,
                                 left = "RSV wastewater concentration (copies/mL)",
                                 right = "RSV hospital incidence (per 100k)/RSV PCR positive test incidence (per 100k)"))

# add legend


# save
png("figures and tables/Figure 2.png",
    units = "in",
    width = 9, height = 7,
    res = 600)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 600)
plot_grid(panel_2, legend,
          ncol = 1,
          rel_heights = c(2, 0.1))
showtext_end()
dev.off()