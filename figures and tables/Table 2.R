
####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Table 2

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
library(glmmTMB)
library(rcompanion)

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

# change week to date object
dat_county$week <- ymd(dat_county$week)

# model 1 - lagged wastewater by one week, under 10s predicting hosp incidence
# for over 50s

# glmmtbm
# ar1 adjustment
dat_county$date_Factor <- factor(dat_county$week,
                                 levels = seq.Date(
                                   from = min(as_date(dat_county$week)), 
                                   to = max(as_date(dat_county$week)),
                                   by = 1)
)
dat_county$date_Factor <- cut(dat_county$week, breaks = "week")

dat_county <- dat_county %>%
  group_by(county) %>%
  arrange(week) %>%
  mutate(rsv_ww_county_lag_1 = lag(rsv_ww_county,
                                   1,
                                   default = NA),
         county_factor = factor(county),
         rsv_ww_county_lag_2 = lag(rsv_ww_county,
                                   2,
                                   default = NA),
         rsv_ww_county_lag_3 = lag(rsv_ww_county,
                                   3,
                                   default = NA),
         rsv_hosp_10_100_lag_1 = lag(rsv_hosp_10_100, 1,
                                     default = NA),
         rsv_hosp_10_100_lag_2 = lag(rsv_hosp_10_100, 2,
                                     default = NA),
         rsv_hosp_10_100_lag_3 = lag(rsv_hosp_10_100, 3,
                                     default = NA)) %>%
  ungroup()

# remove nas
dat_county_na <- dat_county %>%
  filter(!is.na(rsv_hosp_10_100)) %>%
  filter(!is.na(rsv_hosp_50_100)) %>%
  filter(!is.na(rsv_ww_county_lag_1)) %>%
  filter(!is.na(date_Factor))

# gaussian with random effect only
# negative binomial time series glmm
model_county_0 <- glmmTMB::glmmTMB(rsv_hosp_50_100 ~ 
                                     + scale(log1p(rsv_hosp_10_100))
                                   + scale(log1p(rsv_ww_county))
                                   +ar1(date_Factor + 0|county_factor),
                                   family = gaussian()
                                   ,
                                   data = dat_county_na,
                                   control=glmmTMBControl(optimizer=optim, 
                                                          optArgs=list(method="BFGS"))
)

summary(model_county_0)
AIC(model_county_0)

# model results export
model_table_0 <- model_summary_function(dataframe = dat_county_na,
                                        outcome = dat_county_na$rsv_hosp_50_100,
                                        model = model_county_0,
                                        glmer_option = "glmmtmb") %>%
  mutate(group = "No time lag") %>%
  select(group, everything()) %>%
  select(-`t value2`)

# ------------------------------------------------------------------------------

# 1 week lag
model_county_1 <- glmmTMB::glmmTMB(rsv_hosp_50_100 ~ 
                                     + scale(log1p(rsv_hosp_10_100_lag_1))
                                   + scale(log1p(rsv_ww_county_lag_1))
                                   +ar1(date_Factor + 0|county_factor),
                                   family = gaussian()
                                   ,
                                   data = dat_county_na,
                                   control=glmmTMBControl(optimizer=optim, 
                                                          optArgs=list(method="BFGS"))
)

summary(model_county_1)
AIC(model_county_1)

# model results export
model_table_1 <- model_summary_function(dataframe = dat_county_na,
                                        outcome = dat_county_na$rsv_hosp_50_100,
                                        model = model_county_1,
                                        glmer_option = "glmmtmb") %>%
  mutate(group = "1 week lag") %>%
  select(group, everything())

# 2 week lag
model_county_2 <- glmmTMB::glmmTMB(rsv_hosp_50_100 ~ 
                                     + scale(log1p(rsv_hosp_10_100_lag_2))
                                   + scale(log1p(rsv_ww_county_lag_2))
                                   +ar1(date_Factor + 0|county_factor),
                                   family = gaussian()
                                   ,
                                   data = dat_county_na,
                                   control=glmmTMBControl(optimizer=optim, 
                                                          optArgs=list(method="BFGS"))
)

summary(model_county_2)
AIC(model_county_2)

# model results export
model_table_2 <- model_summary_function(dataframe = dat_county_na,
                                        outcome = dat_county_na$rsv_hosp_50_100,
                                        model = model_county_2,
                                        glmer_option = "glmmtmb") %>%
  mutate(group = "2 week lag") %>%
  select(group, everything())

# 3 week lag
model_county_3 <- glmmTMB::glmmTMB(rsv_hosp_50_100 ~ 
                                     + scale(log1p(rsv_hosp_10_100_lag_3))
                                   + scale(log1p(rsv_ww_county_lag_3))
                                   +ar1(date_Factor + 0|county_factor),
                                   family = gaussian()
                                   ,
                                   data = dat_county_na,
                                   control=glmmTMBControl(optimizer=optim, 
                                                          optArgs=list(method="BFGS"))
)

summary(model_county_3)
AIC(model_county_3)

# model results export
model_table_3 <- model_summary_function(dataframe = dat_county_na,
                                        outcome = dat_county_na$rsv_hosp_50_100,
                                        model = model_county_3,
                                        glmer_option = "glmmtmb") %>%
  mutate(group = "3 week lag") %>%
  select(group, everything())


model_county_4 <- glmmTMB::glmmTMB(rsv_hosp_50_100 ~ 
                                     + scale(log1p(rsv_hosp_10_100_lag_3))
                                   + scale(log1p(rsv_ww_county))
                                   +ar1(date_Factor + 0|county_factor),
                                   family = gaussian()
                                   ,
                                   data = dat_county_na,
                                   control=glmmTMBControl(optimizer=optim, 
                                                          optArgs=list(method="BFGS"))
)

summary(model_county_4)

model_table_4 <- model_summary_function(dataframe = dat_county_na,
                                        outcome = dat_county_na$rsv_hosp_50_100,
                                        model = model_county_4,
                                        glmer_option = "glmmtmb") %>%
  mutate(group = "No lag ww/3 week lag under 10 hosp") %>%
  select(group, everything())


# combine
model_table <- rbind(model_table_0,
                     model_table_1 %>%
                       select(-`t value2`),
                     model_table_2 %>%
                       select(-`t value2`)) %>%
  mutate(`Pr(>|z|)` = round(`Pr(>|z|)`, 3))

# factor groups
model_table$group <- factor(model_table$group,
                            levels = c("No time lag",
                                       "1 week lag",
                                       "2 week lag"))

# save table
# make it a flextable
model_table_ft <- table_as_flex_function(dataframe = model_table, 
                                         title = paste0("Table Mixed model results"))

model_table_ft

# save
save_as_docx(FitFlextableToPage(model_table_ft), 
             path = paste0(
               "figures and tables/Table Mixed model results",
               ".docx"))

