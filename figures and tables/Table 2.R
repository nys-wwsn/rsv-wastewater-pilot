
####
#### RSV WASTEWATER DATA ANALYSIS FOR 2025 DOH AND LHD REPORT
####

# Script for Table 4

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
library(cowplot)
library(glmmTMB)
library(rcompanion)

# --------------------------
# Functions
# --------------------------

# functions across multiple scripts
source("RSV - functions.R")

# Functions for this script
# ------------------------------------------------------------------------------
# Helper: clean raw model term names into readable labels
# ------------------------------------------------------------------------------
clean_variable_names <- function(x) {
  x <- as.character(x)
  x <- gsub("\\(Intercept\\)",                                  "Intercept",                           x)
  x <- gsub("scale\\(log1p\\(rsv_ww_county.*?\\)\\)",          "Log+1 RSV wastewater conc",           x)
  x <- gsub("scale\\(log1p\\(rsv_ww_county\\)\\)",             "Log+1 RSV wastewater conc",           x)
  x <- gsub("scale\\(log1p\\(rsv_hosp_10_100_lag_[0-9]\\)\\)", "Log+1 RSV under 10 hospitalizations", x)
  x <- gsub("scale\\(log1p\\(rsv_hosp_10_100\\)\\)",           "Log+1 RSV under 10 hospitalizations", x)
  x <- gsub("scale\\(rsv_hosp_10_100_lag_[0-9]\\)",            "Log+1 RSV under 10 hospitalizations", x)
  x <- gsub("scale\\(rsv_hosp_10_100\\)",                      "Log+1 RSV under 10 hospitalizations", x)
  x <- gsub("EfronRSquared",                                    "Efron R\u00B2",                       x)
  x <- gsub("Efron R2",                                        "Efron R\u00B2",                       x)
  x
}

# ------------------------------------------------------------------------------
# Helper: format a single model's results into a named vector of cell strings
# "Est (SE)\np=value" for predictor rows, plain value for metric rows
# -----------------------------------------------------------------------------
format_model_col <- function(df_model) {
  # df_model must have columns: variable, Est, Std.Error, Pr
  metric_rows <- c("n", "Efron R\u00B2", "AIC")
  
  mapply(function(var, est, se, pval) {
    if (var %in% metric_rows || is.na(est)) {
      if (is.na(est)) return("")
      formatC(as.numeric(est), digits = 3, format = "fg", flag = "#")
    } else {
      est_fmt <- formatC(as.numeric(est), digits = 3, format = "fg", flag = "#")
      se_fmt  <- formatC(as.numeric(se),  digits = 3, format = "fg", flag = "#")
      p_fmt   <- format_pvalue(pval)
      p_str   <- ifelse(grepl("<", p_fmt),
                        paste0("p", p_fmt),
                        paste0("p=", p_fmt))
      paste0(est_fmt, " (", se_fmt, ")\n", p_str)
    }
  }, df_model$variable, df_model$Est, df_model$SE, df_model$Pval,
  SIMPLIFY = TRUE, USE.NAMES = FALSE)
}

# ------------------------------------------------------------------------------
# Helper: prepare a single model table into a standardised long dataframe
# with columns: group, variable, Est, SE, Pval
# ------------------------------------------------------------------------------
prep_model <- function(tbl) {
  tbl %>%
    dplyr::rename_with(~ dplyr::case_when(
      . == "Std. Error" ~ "SE",
      . == "Pr(>|z|)"   ~ "Pval",
      TRUE              ~ .
    )) %>%
    dplyr::mutate(variable = clean_variable_names(variable))
}

# ------------------------------------------------------------------------------
# Helper: build the full variable roster for a lag group (all possible rows)
# ------------------------------------------------------------------------------
lag_variables <- c("Intercept",
                   "Log+1 RSV wastewater conc",
                   "Log+1 RSV under 10 hospitalizations",
                   "n", "Efron R\u00B2", "AIC")

# ------------------------------------------------------------------------------
# Helper: given a prepped model df and a group label, return a single-group
# wide row dataframe aligned to lag_variables
# ------------------------------------------------------------------------------
align_to_roster <- function(prepped, grp) {
  base <- data.frame(variable = lag_variables, stringsAsFactors = FALSE)
  sub  <- prepped %>% dplyr::filter(group == grp)
  dplyr::left_join(base, sub, by = "variable")
}

# ------------------------------------------------------------------------------
# Main: build wide flextable from three model sets (both, ww_only, hosp_only)
# Each set is a list of prepped model tables named by lag group
# ------------------------------------------------------------------------------
model_table_as_flex_function <- function(both_list,
                                         ww_list,
                                         hosp_list,
                                         lag_groups,
                                         title) {
  
  # --- 1. Build wide dataframe ------------------------------------------------
  rows <- lapply(lag_groups, function(grp) {
    b <- align_to_roster(both_list, grp)
    w <- align_to_roster(ww_list,   grp)
    h <- align_to_roster(hosp_list, grp)
    
    data.frame(
      Model    = grp,
      Variable = lag_variables,
      Both     = format_model_col(b),
      WW       = format_model_col(w),
      Hosp     = format_model_col(h),
      stringsAsFactors = FALSE
    )
  })
  
  df <- dplyr::bind_rows(rows)
  df$Model <- factor(df$Model, levels = lag_groups)
  
  # --- 2. Build flextable -----------------------------------------------------
  ft <- flextable::flextable(df,
                             col_keys = c("Model", "Variable", "Both", "WW", "Hosp"))
  
  # --- 3. Spanning + sub headers ----------------------------------------------
  ft <- flextable::add_header_row(ft,
                                  values = c("", "", "Both Predictors", "Wastewater Only", "Hospitalizations Only"),
                                  colwidths = c(1, 1, 1, 1, 1))
  
  ft <- flextable::set_header_labels(ft,
                                     Model    = "Model",
                                     Variable = "Variable",
                                     Both     = "Est (SE)\np value",
                                     WW       = "Est (SE)\np value",
                                     Hosp     = "Est (SE)\np value")
  
  # --- 4. Merge top header labels where blank --------------------------------
  ft <- flextable::merge_v(ft,  j = c("Model", "Variable"), part = "header")
  ft <- flextable::merge_h(ft,  part = "header")
  
  # --- 5. Merge Model column cells vertically per group ----------------------
  ft <- flextable::merge_v(ft, j = "Model", part = "body")
  
  # --- 6. Typography ----------------------------------------------------------
  ft <- flextable::font(ft,     fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 10,                    part = "all")
  ft <- flextable::bold(ft,     part = "header")
  
  # --- 7. Alignment -----------------------------------------------------------
  ft <- flextable::align(ft, j = "Model",    align = "left",   part = "all")
  ft <- flextable::align(ft, j = "Variable", align = "left",   part = "body")
  ft <- flextable::align(ft, j = "Variable", align = "center", part = "header")
  ft <- flextable::align(ft, j = c("Both", "WW", "Hosp"),
                         align = "center", part = "all")
  
  # --- 8. Borders -------------------------------------------------------------
  thin_border  <- officer::fp_border(color = "black", width = 0.75)
  thick_border <- officer::fp_border(color = "black", width = 1.5)
  
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft,    border = thick_border, part = "header")
  ft <- flextable::hline_bottom(ft, border = thick_border, part = "body")
  ft <- flextable::vline_left(ft,   border = thick_border, part = "all")
  ft <- flextable::vline_right(ft,  border = thick_border, part = "all")
  ft <- flextable::border_inner_h(ft, border = thin_border, part = "all")
  ft <- flextable::border_inner_v(ft, border = thin_border, part = "all")
  
  # --- 9. Line height — allow multi-line cells --------------------------------
  ft <- flextable::line_spacing(ft, space = 1.2, part = "all")
  
  # --- 10. Padding ------------------------------------------------------------
  ft <- flextable::padding(ft,
                           padding.top = 3, padding.bottom = 3,
                           padding.left = 4, padding.right = 4, part = "all")
  
  # --- 11. Caption ------------------------------------------------------------
  ft <- flextable::set_caption(ft,
                               caption = flextable::as_paragraph(flextable::as_chunk(title)))
  
  # --- 12. Valign -------------------------------------------------------------
  ft <- flextable::valign(ft, j = "Model",    valign = "top", part = "body")
  ft <- flextable::valign(ft, j = "Variable", valign = "center", part = "body")
  ft <- flextable::valign(ft, valign = "center", part = "header")
  
  return(ft)
}

# Helper to fit ww-only or hosp-only glmmTMB model
fit_single_predictor <- function(formula, data) {
  glmmTMB::glmmTMB(formula,
                   family  = gaussian(),
                   data    = data,
                   control = glmmTMBControl(optimizer = optim,
                                            optArgs   = list(method = "BFGS")))
}

# ------------
# ggplot theme
# ------------

source("RSV - plot themes.R")

# --------------------------
# Load combined data file
# --------------------------

load(file = "../Data/rsv_combined_data.Rdata")


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
dat_county$date_Factor <- cut(ymd(dat_county$week), breaks = "week")

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
         rsv_hosp_10_100_lag_1 = lag(rsv_hosp_10_100, 1,
                                     default = NA),
         rsv_hosp_10_100_lag_2 = lag(rsv_hosp_10_100, 2,
                                     default = NA)) %>%
  ungroup()

# remove nas
dat_county_na <- dat_county %>%
  filter(!is.na(rsv_hosp_10_100)) %>%
  filter(!is.na(rsv_hosp_50_100)) %>%
  filter(!is.na(rsv_ww_county_lag_1)) %>%
  filter(!is.na(date_Factor))

# --- No time lag --------------------------------------------------------------
model_county_0   <- fit_single_predictor(
  rsv_hosp_50_100 ~ + scale(log1p(rsv_hosp_10_100)) 
  + scale(log1p(rsv_ww_county)) + ar1(date_Factor + 0|county_factor),
  dat_county_na)

model_county_0_ww   <- fit_single_predictor(
  rsv_hosp_50_100 ~ scale(log1p(rsv_ww_county)) + ar1(date_Factor + 0|county_factor),
  dat_county_na)

model_county_0_hosp <- fit_single_predictor(
  rsv_hosp_50_100 ~ scale(log1p(rsv_hosp_10_100)) + ar1(date_Factor + 0|county_factor),
  dat_county_na)


# --- 1 week lag ---------------------------------------------------------------
model_county_1   <- fit_single_predictor(
  rsv_hosp_50_100 ~ + scale(log1p(rsv_hosp_10_100_lag_1)) 
  + scale(log1p(rsv_ww_county_lag_1)) + ar1(date_Factor + 0|county_factor),
  dat_county_na)

model_county_1_ww   <- fit_single_predictor(
  rsv_hosp_50_100 ~ scale(log1p(rsv_ww_county_lag_1)) + ar1(date_Factor + 0|county_factor),
  dat_county_na)

model_county_1_hosp <- fit_single_predictor(
  rsv_hosp_50_100 ~ scale(log1p(rsv_hosp_10_100_lag_1)) + ar1(date_Factor + 0|county_factor),
  dat_county_na)

# --- 2 week lag ---------------------------------------------------------------
model_county_2 <- fit_single_predictor(
  rsv_hosp_50_100 ~ + scale(log1p(rsv_hosp_10_100_lag_2)) 
  + scale(log1p(rsv_ww_county_lag_2)) + ar1(date_Factor + 0|county_factor),
  dat_county_na
)

model_county_2_ww   <- fit_single_predictor(
  rsv_hosp_50_100 ~ scale(log1p(rsv_ww_county_lag_2)) + ar1(date_Factor + 0|county_factor),
  dat_county_na)

model_county_2_hosp <- fit_single_predictor(
  rsv_hosp_50_100 ~ scale(log1p(rsv_hosp_10_100_lag_2)) + ar1(date_Factor + 0|county_factor),
  dat_county_na)

# ==============================================================================
# EXTRACT MODEL RESULTS
# ==============================================================================

extract <- function(model, grp) {
  model_summary_function(dataframe    = dat_county_na,
                         outcome      = dat_county_na$rsv_hosp_50_100,
                         model        = model,
                         glmer_option = "glmmtmb") %>%
    dplyr::mutate(group = grp) %>%
    dplyr::select(group, everything()) %>%
    dplyr::select(-dplyr::any_of("t value2"))
}

# Both predictors
both_0 <- extract(model_county_0, "No time lag")
both_1 <- extract(model_county_1, "1 week lag")
both_2 <- extract(model_county_2, "2 week lag")

# WW only
ww_0 <- extract(model_county_0_ww, "No time lag")
ww_1 <- extract(model_county_1_ww, "1 week lag")
ww_2 <- extract(model_county_2_ww, "2 week lag")

# Hosp only
hosp_0 <- extract(model_county_0_hosp, "No time lag")
hosp_1 <- extract(model_county_1_hosp, "1 week lag")
hosp_2 <- extract(model_county_2_hosp, "2 week lag")

# Combine into lists
prep_list <- function(...) {
  dplyr::bind_rows(...) %>%
    dplyr::rename_with(~ dplyr::case_when(
      . == "Std. Error" ~ "SE",
      . == "Pr(>|z|)"   ~ "Pval",
      TRUE              ~ .
    )) %>%
    dplyr::mutate(
      variable = clean_variable_names(variable),
      Est      = as.numeric(Est),
      Pval     = as.numeric(Pval)
    )
}

both_combined <- prep_list(both_0, both_1, both_2)
ww_combined   <- prep_list(ww_0,   ww_1,   ww_2)
hosp_combined <- prep_list(hosp_0, hosp_1, hosp_2)

lag_groups <- c("No time lag", "1 week lag", "2 week lag")


# ==============================================================================
# BUILD AND SAVE TABLES
# ==============================================================================

caption_combined <- paste0(
  "Table 2: Generalized linear mixed model results for predicting RSV ",
  "hospitalizations among over 50-year-olds from wastewater and under 10 ",
  "hospitalizations. Coefficients are standardized, outcome is on original ",
  "scale of hospitalizations per 100k."
)

# --- Combined table (all lag groups) -----------------------------------------
model_table_ft_combined <- model_table_as_flex_function(
  both_list  = both_combined,
  ww_list    = ww_combined,
  hosp_list  = hosp_combined,
  lag_groups = lag_groups,
  title      = caption_combined
)

model_table_ft_combined

save_as_docx(FitFlextableToPage(model_table_ft_combined),
             path = "figures and tables/Table 2 Mixed model results.docx")
