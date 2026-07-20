
####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Figure S6 - sensitivity panel

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

# Sensitivity of detection
# Sewershed and county level



# ------------------------------------------------------------------------------

###
# save tables
###

# sensitivity all weeks
sens_all_weeks <- sens_output_table_function(season_select = c("Weeks 21 to 39",
                                                               "Weeks 40 to 20"),
                                             season_name = "All weeks")
# make the df a flextable
sens_table_ft <- table_as_flex_function(
  dataframe = sens_all_weeks,
  title = paste("Sensitivity, specificity, PPV, and NPV of RSV wastewater",
                " surveillance (all weeks)", sep = "")
)

###
# summer
###

sens_summer <- sens_output_table_function(season_select = c("Weeks 21 to 39"),
                                          season_name = "Weeks 21 to 39")


# save as table
save_as_docx(FitFlextableToPage(sens_table_ft), 
             path = "figures and tables/RSV sensitivity table - summer.docx")

####
# winter
### 

sens_winter <- sens_output_table_function(season_select = c("Weeks 40 to 20"),
                                          season_name = "Weeks 40 to 20")


# ------------------------------------------------------------------------------

###
# Figure
###

# one large figure
sensitivity_table <- bind_rows(sens_all_weeks, sens_winter, sens_summer)

# factor the values
sensitivity_table$group <- factor(sensitivity_table$group,
                                  levels = c("sensitivity",
                                             "specificity",
                                             "ppv",
                                             "npv"),
                                  labels = c("Sensitivity",
                                             "Specificity",
                                             "PPV",
                                             "NPV"))

# plot with all of them, no labels
showtext_auto(FALSE)

sensitivity_table$outcome <- factor(sensitivity_table$outcome,
                                    levels = c("cases",
                                               "All hospitalizations",
                                               "Under 10",
                                               "Over 50"),
                                    labels = c("Cases",
                                               "All hospitalizations",
                                               "Under 10 hospitalizations",
                                               "Over 50 hospitalizations"))

# make it a plot
sens_plot <- 
  ggplot(data = sensitivity_table %>%
           filter(aggregation != "sewershed" | outcome != "cases"))+
  geom_pointrange(
    aes(x = group, y = value, ymin = `95% LL`, ymax = `95% UL`,
        shape = outcome, color = outcome),
    size = 0.7, lwd = 1.3,
    position = position_dodge(1)
  )+
  facet_grid(season_name ~ aggregation)+
  scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 4)),
                     name = "")+
  labs(title = "Sensitivity, specificity, PPV, NPV",
       x = "Value",
       y = "")+
  scale_shape_manual(values = c(17, 19, 3, 15), name = "")+
  theme_dth_1+
  theme(legend.position = "bottom")+
  geom_vline(xintercept = 1.5)+
  geom_vline(xintercept = 2.5)+
  geom_vline(xintercept = 3.5)

sens_plot 

# save
png("figures and tables/RSV sensitivity plot - all options.png",
    units = "in",
    width = 11, height = 8,
    res = 600)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 600)
sens_plot 
showtext_end()
dev.off()
showtext_auto(FALSE)

