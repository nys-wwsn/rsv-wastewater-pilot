####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Figure 2 ROC curves

# AUTHOR - Dustin T. Hill

# Date Created: August 11, 2025

# LAST UPDATED - September 30, 2025

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

load(file = "data/rsv_combined_data.Rdata")

# Add seasons and pilot county objects

# list of pilot counties
pilot <- c("Jefferson", "Erie", "Onondaga", "Westchester")
pilot_cases <- c("Jefferson", "Onondaga")
onondaga <- c("Onondaga")

###
# --- county level ---
###
# weekly data
dat_weekly <- 
  dat_county %>%
  group_by(county,  week) %>%
  summarize(rsv_conc_weekly = mean(rsv_ww_county, na.rm = TRUE),
            rsv_hosp_all = sum(rsv_hosp_all, na.rm = TRUE),
            rsv_hosp_10 = sum(rsv_hosp_10, na.rm = TRUE),
            rsv_hosp_50 = sum(rsv_hosp_50, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(rsv_conc_weekly))

dat_weekly$outcome_all_10 <- ifelse(
  dat_weekly$rsv_hosp_all > 1, 1, 0)

# simple glm
model_hosp <- glm(outcome_all_10 ~ rsv_conc_weekly, 
                  data = dat_weekly,
                  family = binomial)

# probabilities
probs_mod1 <- predict(model_hosp, type = 'response')

# roc curve
roc_mod1 <- roc(response = dat_weekly$outcome_all_10, 
                predictor = probs_mod1)

# plot all three seasons by color
group <- "All hospitalizations"
AUC_all <- round(auc(roc_mod1), digits = 2)
all <- cbind(coords(roc_mod1, "all"), group)

# under 10
dat_weekly$outcome_10_10 <- ifelse(
  dat_weekly$rsv_hosp_10 > 1, 1, 0)

# simple glm
model_hosp <- glm(outcome_10_10 ~ rsv_conc_weekly, 
                  data = dat_weekly,
                  family = binomial)

# probabilities
probs_mod1 <- predict(model_hosp, type = 'response')

# roc curve
roc_mod1 <- roc(response = dat_weekly$outcome_10_10, 
                predictor = probs_mod1)

# plot all three seasons by color
group <- "Under 10"
AUC_under10 <- round(auc(roc_mod1), digits = 2)
under10 <- cbind(coords(roc_mod1, "all"), group)

# over 50
dat_weekly$outcome_50_10 <- ifelse(
  dat_weekly$rsv_hosp_50 > 1, 1, 0)

# simple glm
model_hosp <- glm(outcome_50_10 ~ rsv_conc_weekly, 
                  data = dat_weekly,
                  family = binomial)

# probabilities
probs_mod1 <- predict(model_hosp, type = 'response')

# roc curve
roc_mod1 <- roc(response = dat_weekly$outcome_50_10, 
                predictor = probs_mod1)

# plot all three seasons by color
group <- "Over 50"
AUC_over50 <- round(auc(roc_mod1), digits = 2)
over50 <- cbind(coords(roc_mod1, "all"), group)


# combine
roc_df <- bind_rows(all, under10, over50)

# plot
pal <- met.brewer("Hiroshige", 3)
showtext_auto()
mod1_plot <- ggplot(roc_df, aes(x = 1-specificity, y = sensitivity,
                                color = group)) +
  geom_line(lwd=1)+
  labs(x = 'False-positive rate', y = 'True-positive rate',
       title = 'ROC curve\nsurvived ~ pclass') +
  theme_dth_1+
  labs(x = "1 - specificity (false-positive rate)",
       y = "Sensitivity (true-positive rate)",
       title = paste("ROC curve - ", "county", " level", sep = ""),
       subtitle = 
         paste("at least 1 weekly ", "hospitalization"," ~ RSV ww concentration", 
               sep = "")
  )+
  scale_color_manual(values = c("All hospitalizations" = pal[1],
                                "Over 50" = "#e76254",
                                "Under 10" = "#376795"),
                     name = ""
  ) +
  annotate('text', x = .75, y = .5,
           label = paste("AUC: ", AUC_all, sep = ""),
           color = pal[1]) +
  annotate('text', x = .75, y = .425,
           label = paste("AUC: ", AUC_under10, sep = ""),
           color = "#376795") +
  annotate('text', x = .75, y = .35,
           label = paste("AUC: ", AUC_over50, sep = ""),
           color = "#e76254")+
  theme(legend.position = "bottom",
        legend.background = element_blank())

mod1_plot


# save
png("figures and tables/RSV ROC curve - all weeks.png",
    units = "in",
    width = 7.5, height = 6,
    res = 600)
showtext::showtext_auto()
showtext::showtext_opts(dpi = 600)
mod1_plot
dev.off()
