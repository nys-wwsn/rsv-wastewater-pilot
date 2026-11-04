# library(gt)
# library(gtExtras)
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
library(osmdata)
library(ggspatial)
library(patchwork)
library(glmmTMB)
library(rcompanion)

pal <- met.brewer("Hiroshige", n = 4)

map_function <- function(county_name){
  map <- 
    ggplot()+
    annotation_map_tile(zoom = 9,
                        type = "osm") +
    geom_sf(data = ny_counties %>%
              filter(NAME == county_name),
            fill = "NA", color = "black",
            lwd = 1.2)+
    geom_sf(data = sewershed_summary %>%
              filter(county == county_name), 
            aes(color = `Detection rate`,  size = `Sewershed size`),
            alpha = 0.7) +
    geom_sf(data = sewershed_summary %>%
              filter(county == county_name),
            aes(size = `Sewershed size`),
            shape = 1,
            stroke = 2)+
    scale_color_manual(name = "Detection rate (%)",
                       values = c("< 30%" = pal[4],
                                  "30% to 40%" = pal[3],
                                  "40% to 50%" = pal[2],
                                  "> 50%" = pal[1])
    )+
    scale_size_discrete(range = c(6, 14)) +
    theme_dth_maps+
    labs(title = paste(county_name, " County", sep = ""),
         size = "Sewershed size\n(mgd)")+
    theme(legend.position = "bottom",
          legend.background = element_blank())
  
  # mini ny map
  ggm1 <- ggplot() + 
    annotation_map_tile(zoom = 5,
                        type = "hotstyle") +
    geom_sf(data = ny_counties, fill = "dodgerblue", alpha = 0.5) + 
    geom_sf(data = ny_counties %>%
              filter(NAME == county_name), fill = "orange", 
            color = "white", 
            size = 0.25) +
    theme_dth_maps+
    #annotation_scale(location = "bl", width_hint = 0.4)+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
  
  # inset map
  gg_inset_map1 <- 
    map+theme(legend.position = "none")+ 
    inset_element(ggm1, left = 0.6, bottom = 0.6, right = 1, top = 1)
  
  # gg_inset_map1 <- ggdraw() +
  #   draw_plot(map) +
  #   draw_plot(ggm1, x = 0, y = 0.74, width = 0.3, height = 0.3)
  # 
  return(gg_inset_map1)
  
}

# get legend function
# from this link
# https://stackoverflow.com/questions/78163631/r-get-legend-from-cowplot-package-no-longer-work-for-ggplot2-version-3-5-0
# get legend function
get_legend2 <- function(plot, legend = NULL) {
  if (is.ggplot(plot)) {
    gt <- ggplotGrob(plot)
  } else {
    if (is.grob(plot)) {
      gt <- plot
    } else {
      stop("Plot object is neither a ggplot nor a grob.")
    }
  }
  pattern <- "guide-box"
  if (!is.null(legend)) {
    pattern <- paste0(pattern, "-", legend)
  }
  indices <- grep(pattern, gt$layout$name)
  not_empty <- !vapply(
    gt$grobs[indices], 
    inherits, what = "zeroGrob", 
    FUN.VALUE = logical(1)
  )
  indices <- indices[not_empty]
  if (length(indices) > 0) {
    return(gt$grobs[[indices[1]]])
  }
  return(NULL)
}

# Renoir was old palette (purples)
# calculate sensitivity, spec, ppv, npv


sensitivity_function <- function(dataframe, 
                                 aggregation, 
                                 outcome, 
                                 outcome_var_name,
                                 ww_var){
  
  
  dataframe$true_positive <- ifelse(dataframe[[ww_var]] > 0 & 
                                      dataframe[[outcome_var_name]] > 0, 1, 0)
  
  dataframe$false_positive <- ifelse(dataframe[[ww_var]] > 0 & 
                                       dataframe[[outcome_var_name]] == 0, 1, 0)
  
  dataframe$true_negative <- ifelse(dataframe[[ww_var]] == 0 & 
                                      dataframe[[outcome_var_name]] == 0, 1, 0)
  
  dataframe$false_negative <- ifelse(dataframe[[ww_var]] == 0 & 
                                       dataframe[[outcome_var_name]] > 0, 1, 0)
  
  # make a two by two table
  sens_df <- dataframe %>%
    summarize(true_positive = sum(true_positive, na.rm = TRUE),
              true_negative = sum(true_negative, na.rm = TRUE),
              false_positive = sum(false_positive, na.rm = TRUE),
              false_negative = sum(false_negative, na.rm = TRUE)
    )
  
  sensitivity <- (sens_df$true_positive /(sens_df$true_positive + sens_df$false_negative))
  value <- sensitivity
  SE <- sqrt((value * (1 - sensitivity)) / (sens_df$true_positive + sens_df$false_negative) )
  `95% LL` <- sensitivity - 1.96 * SE
  `95% UL` <- sensitivity + 1.96 * SE
  group <- "sensitivity"
  sens_row <- as.data.frame(cbind(group, value, SE, `95% LL`, `95% UL`))
  
  
  specificity <- (sens_df$true_negative / (sens_df$true_negative + sens_df$false_positive))
  value <- specificity
  SE <- sqrt( (specificity * (1 - specificity)) / (sens_df$true_negative + sens_df$false_positive) )
  `95% LL` <- specificity - 1.96 * SE
  `95% UL` <- specificity + 1.96 * SE
  group <- "specificity"
  spec_row <- as.data.frame(cbind(group, value, SE, `95% LL`, `95% UL`))
  
  ppv <- (sens_df$true_positive / (sens_df$true_positive + sens_df$false_positive))    
  value <- ppv
  SE <- sqrt( ppv * (1 - ppv) / (sens_df$true_positive + sens_df$false_positive) )
  `95% LL` <- ppv - 1.96 * SE
  `95% UL` <- ppv + 1.96 * SE
  group <- "ppv"
  ppv_row <- as.data.frame(cbind(group, value, SE, `95% LL`, `95% UL`))
  
  npv <- (sens_df$true_negative / (sens_df$true_negative + sens_df$false_negative))
  value <- npv
  SE <- sqrt( npv * (1 - npv) / (sens_df$false_negative + sens_df$true_negative) )
  `95% LL` <- npv - 1.96 * SE
  `95% UL` <- npv + 1.96 * SE
  group <- "npv"
  npv_row <- as.data.frame(cbind(group, value, SE, `95% LL`, `95% UL`))
  
  df <- bind_rows(sens_row, spec_row, ppv_row, npv_row) %>%
    mutate_at(c("value", "SE", "95% LL", "95% UL"), as.numeric) %>%
    mutate_if(is.numeric, ~round(., 2))
  
  aggregation <- aggregation
  outcome <- outcome
  
  # table
  row_number <- as.data.frame(cbind(aggregation, outcome, df))
  return(row_number)
}

# Sens function wrapper for seasons
sens_output_table_function <- function(season_select,
                                       season_name){
  # Hospitalizations (sewershed and county) (under 10, over 50, all)
  
  # sewershed, all hospital admissions
  row_1 <- sensitivity_function(dataframe = dat_sewershed %>% ungroup() %>%
                                  filter(season %in% season_select),
                                aggregation = "sewershed",
                                outcome = "All hospitalizations",
                                outcome_var_name = "rsv_hosp_all_100",
                                ww_var = "rsv_ww_mean")
  
  season_name <- season_name
  row_1 <- as.data.frame(cbind(season_name, row_1))
  
  # sewershed under 10 hospitalizations
  row_2 <- sensitivity_function(dataframe = dat_sewershed %>% ungroup() %>%
                                  filter(season %in% season_select),
                                aggregation = "sewershed",
                                outcome = "Under 10",
                                outcome_var_name = "rsv_hosp_10_100",
                                ww_var = "rsv_ww_mean")
  
  season_name <- season_name
  row_2 <- as.data.frame(cbind(season_name, row_2))
  
  # sewershed over 50 hospitalizations
  row_3 <- sensitivity_function(dataframe = dat_sewershed %>% ungroup() %>%
                                  filter(season %in% season_select),
                                aggregation = "sewershed",
                                outcome = "Over 50",
                                outcome_var_name = "rsv_hosp_50_100",
                                ww_var = "rsv_ww_mean")
  
  season_name <- season_name
  row_3 <- as.data.frame(cbind(season_name, row_3))
  
  ###
  # county case sensitivity
  ###
  
  # table
  row_4 <- sensitivity_function(dataframe = dat_county %>%ungroup() %>%
                                  filter(county == "Onondaga") %>%
                                  filter(season %in% season_select), 
                                aggregation = "county",
                                outcome = "cases", 
                                outcome_var_name = "rsv_cases_total_100",
                                ww_var = "rsv_ww_county")
  season_name <- season_name
  row_4 <- as.data.frame(cbind(season_name, row_4))
  
  ###
  # county hosp sensitivity
  ###
  
  # county hosp sensitivity - all
  row_5 <- sensitivity_function(dataframe = dat_county %>%ungroup()  %>%
                                  filter(season %in% season_select), 
                                aggregation = "county",
                                outcome = "All hospitalizations", 
                                outcome_var_name = "rsv_hosp_all",
                                ww_var = "rsv_ww_county")
  season_name <- season_name
  row_5 <- as.data.frame(cbind(season_name, row_5))
  
  # county hosp sensitivity - under 10
  row_6 <- sensitivity_function(dataframe = dat_county %>%ungroup()  %>%
                                  filter(season %in% season_select), 
                                aggregation = "county",
                                outcome = "Under 10", 
                                outcome_var_name = "rsv_hosp_10",
                                ww_var = "rsv_ww_county")
  season_name <- season_name
  row_6 <- as.data.frame(cbind(season_name, row_6))
  
  # county hosp sensitivity - over 50
  row_7 <- sensitivity_function(dataframe = dat_county %>%ungroup() %>%
                                  filter(season %in% season_select) , 
                                aggregation = "county",
                                outcome = "Over 50", 
                                outcome_var_name = "rsv_hosp_50",
                                ww_var = "rsv_ww_county")
  season_name <- season_name
  row_7 <- as.data.frame(cbind(season_name, row_7))
  
  ## Bind all the results together and add season indicator
  sens_df <- bind_rows(row_1,
                       row_2,
                       row_3,
                       row_4,
                       row_5,
                       row_6,
                       row_7)
  
  return(sens_df)
}

# lag and lead time functions - note designed for county level aggregation
# daily data
lag_lead_function_daily <- function(dataframe, 
                                    steps, 
                                    method,
                                    ww_var, 
                                    clin_var,
                                    plot,
                                    clin_name){
  
  dataframe$ww_var <- dataframe[[ww_var]]
  dataframe$clin_var <- dataframe[[clin_var]]
  
  # empty list
  datalist_lags <- list()
  
  # number of lags to test
  c <- 0:steps
  
  # for loop for daily lags
  for(i in as.character(c)){
    # create one lag for daily data and test correlation
    df3 <- dataframe %>%
      group_by(county) %>%
      arrange(-desc(date)) %>%
      mutate(
        ww_lag = lag(ww_var, as.integer(i), default = NA)
        
      )%>%
      ungroup()
    
    # correlation
    cor_result <- df3 %>%
      group_by(county) %>%
      summarize(
        cor = cor(log1p(ww_lag), log1p(clin_var) , use = "na.or.complete", method = method)
      ) %>%
      ungroup()
    cor_result$lag <- i
    
    datalist_lags[[i]] <- cor_result
  }
  weekly_lags <- do.call(rbind, datalist_lags)
  
  # id highest lag correlation
  lags <- weekly_lags %>%
    group_by(county) %>%
    mutate(highest_lag = ifelse(cor == max(cor, na.rm = TRUE),
                                1, 0)
    )
  
  # lag plot only
  levels <- seq(from = -steps, to = steps, by = 1)
  
  # make it a factor
  lags$lag <- factor(lags$lag,
                     levels = levels)
  # plot
  plot_lags<-
    ggplot(data = lags %>%
             filter(!is.na(cor)), 
           aes(x = lag, y = cor, color = county, group = county))+
    geom_line(lwd = 1)+
    geom_point(aes(shape = as.character(highest_lag)), size = 4)+
    theme_bw()+
    theme(legend.position = "bottom")+
    labs(title = paste(clin_name, " (7-day avg) ~ RSV wastewater conc (7-day avg)", sep = ""),
         color = "",
         x = "Lag",
         y = "Spearman Correlation")+
    guides(shape = "none")+
    scale_shape_manual(values = c("1" = 17,
                                  "0" = 1))+
    scale_color_manual(values = MetBrewer::met.brewer("Hiroshige", 4))+
    ylim(0,1)
  
  plot_lags
  
  
  # lead time
  # empty list
  datalist_lags <- list()
  
  # number of lags to test
  c <- 0:steps
  
  # for loop for daily lags
  for(i in as.character(c)){
    # create one lag for daily data and test correlation
    df3 <- dataframe %>%
      group_by(county) %>%
      arrange(-desc(date)) %>%
      mutate(
        ww_lead = lead(ww_var, as.integer(i), default = NA)
        
      )%>%
      ungroup()
    
    # correlation
    cor_result <- df3 %>%
      group_by(county) %>%
      summarize(
        cor = cor(log1p(ww_lead), log1p(clin_var) , use = "na.or.complete", method = method)
      ) %>%
      ungroup()
    cor_result$lead <- i
    
    datalist_lags[[i]] <- cor_result
  }
  weekly_leads <- do.call(rbind, datalist_lags)
  
  # id highest lag correlation
  leads <- weekly_leads %>%
    group_by(county) %>%
    mutate(highest_lead = ifelse(cor == max(cor, na.rm = TRUE),
                                 1, 0)
    )
  
  leads$lag <- as.numeric(leads$lead) * -1
  leads$lag <- as.character(leads$lag)
  
  # combine dfs
  df <- bind_rows(lags, leads)
  
  levels <- seq(from = -steps, to = steps, by = 1)
  
  # make it a factor
  df$lag <- factor(df$lag,
                   levels = levels)
  
  # indicate highest value
  df <- df %>%
    group_by(county) %>%
    mutate(highest_lag = ifelse(cor == max(cor, na.rm = TRUE),
                                1, 0)
    )
  
  
  # plot
  plot_lags_and_leads<-
    ggplot(data = df %>%
             filter(!is.na(cor)), 
           aes(x = lag, y = cor, color = county, group = county))+
    geom_line(lwd = 1)+
    geom_point(aes(shape = as.character(highest_lag)), size = 4)+
    theme_bw()+
    theme(legend.position = "bottom")+
    labs(title = "RSV hospitalizations (7-day avg) ~ RSV wastewater conc (7-day avg)",
         color = "",
         x = "Lag",
         y = "Spearman Correlation")+
    guides(shape = "none")+
    scale_shape_manual(values = c("1" = 17,
                                  "0" = 1))+
    scale_color_manual(values = MetBrewer::met.brewer("Hiroshige", 4))+
    ylim(0,1)
  
  plot_lags_and_leads
  
  if(plot == "lags only"){
    return(plot_lags)
  } else(
    return(plot_lags_and_leads)
  )
  
}

# weekly data

# weekly data
lag_lead_function_weekly <- function(
    dataframe, 
    steps, 
    method,
    ww_var, 
    clin_var,
    plot,
    clin_name){
  
  dataframe$ww_var <- dataframe[[ww_var]]
  dataframe$clin_var <- dataframe[[clin_var]]
  
  # empty list
  datalist_lags <- list()
  
  # number of lags to test
  c <- 0:steps
  
  # for loop for daily lags
  for(i in as.character(c)){
    # create one lag for daily data and test correlation
    df3 <- dataframe %>%
      group_by(county) %>%
      arrange(-desc(week)) %>%
      mutate(
        ww_lag = lag(ww_var, as.integer(i), default = NA)
        
      )%>%
      ungroup()
    
    # correlation
    cor_result <- df3 %>%
      group_by(county) %>%
      summarize(
        cor = cor(log1p(ww_lag), log1p(clin_var) , use = "na.or.complete", method = method)
      ) %>%
      ungroup()
    cor_result$lag <- i
    
    datalist_lags[[i]] <- cor_result
  }
  weekly_lags <- do.call(rbind, datalist_lags)
  
  # id highest lag correlation
  lags <- weekly_lags %>%
    group_by(county) %>%
    mutate(highest_lag = ifelse(cor == max(cor, na.rm = TRUE),
                                1, 0)
    )
  
  # lag plot only
  levels <- seq(from = -steps, to = steps, by = 1)
  
  # make it a factor
  lags$lag <- factor(lags$lag,
                     levels = levels)
  # plot
  plot_lags<-
    ggplot(data = lags %>%
             filter(!is.na(cor)), 
           aes(x = lag, y = cor, color = county, group = county))+
    geom_line(lwd = 1)+
    geom_point(aes(shape = as.character(highest_lag)), size = 4)+
    theme_bw()+
    theme(legend.position = "bottom")+
    labs(title = paste(clin_name, " (weekly) ~ RSV wastewater conc (weekly)", sep = ""),
         color = "",
         x = "Lag",
         y = "Spearman Correlation")+
    guides(shape = "none")+
    scale_shape_manual(values = c("1" = 17,
                                  "0" = 1))+
    scale_color_manual(values = MetBrewer::met.brewer("Hiroshige", 4))+
    ylim(0,1)
  
  plot_lags
  
  
  # lead time
  # empty list
  datalist_lags <- list()
  
  # number of lags to test
  c <- 0:steps
  
  # for loop for daily lags
  for(i in as.character(c)){
    # create one lag for daily data and test correlation
    df3 <- dataframe %>%
      group_by(county) %>%
      arrange(-desc(week)) %>%
      mutate(
        ww_lead = lead(ww_var, as.integer(i), default = NA)
        
      )%>%
      ungroup()
    
    # correlation
    cor_result <- df3 %>%
      group_by(county) %>%
      summarize(
        cor = cor(log1p(ww_lead), log1p(clin_var) , use = "na.or.complete", method = method)
      ) %>%
      ungroup()
    cor_result$lead <- i
    
    datalist_lags[[i]] <- cor_result
  }
  weekly_leads <- do.call(rbind, datalist_lags)
  
  # id highest lag correlation
  leads <- weekly_leads %>%
    group_by(county) %>%
    mutate(highest_lead = ifelse(cor == max(cor, na.rm = TRUE),
                                 1, 0)
    )
  
  leads$lag <- as.numeric(leads$lead) * -1
  leads$lag <- as.character(leads$lag)
  
  # combine dfs
  df <- bind_rows(lags, leads)
  
  levels <- seq(from = -steps, to = steps, by = 1)
  
  # make it a factor
  df$lag <- factor(df$lag,
                   levels = levels)
  
  # indicate highest value
  df <- df %>%
    group_by(county) %>%
    mutate(highest_lag = ifelse(cor == max(cor, na.rm = TRUE),
                                1, 0)
    )
  
  
  # plot
  plot_lags_and_leads<-
    ggplot(data = df %>%
             filter(!is.na(cor)), 
           aes(x = lag, y = cor, color = county, group = county))+
    geom_line(lwd = 1)+
    geom_point(aes(shape = as.character(highest_lag)), size = 4)+
    theme_bw()+
    theme(legend.position = "bottom")+
    labs(title = paste(clin_name, " (weekly) ~ RSV wastewater conc (weekly)", sep = ""),
         color = "",
         x = "Lag",
         y = "Spearman Correlation")+
    guides(shape = "none")+
    scale_shape_manual(values = c("1" = 17,
                                  "0" = 1))+
    scale_color_manual(values = MetBrewer::met.brewer("Hiroshige", 4))+
    ylim(0,1)
  
  plot_lags_and_leads
  
  if(plot == "lags only"){
    return(plot_lags)
  } else(
    return(plot_lags_and_leads)
  )
}

# make table a flex table
table_as_flex_function <- function(dataframe,
                                   title){
  ft <- flextable(dataframe)
  ft <- theme_vanilla(ft)
  ft <- color(ft, part = "footer", color = "black")
  ft <- set_caption(ft, caption = title)
  ft <- ft %>%
    fontsize(size = 10, part = "all") %>%
    vline(part = "all") %>%
    padding(padding = 0, part = "all") %>%
    align(align = "center") %>%
    align(align = "center", part = "header")
}

# updated function
table_as_flex_rsv_function <- function(dataframe, title) {
  #' Change a dataframe to a flextable for RSV descriptive statistics
  #'
  #' @param dataframe dataframe. Object to change to flextable.
  #' @param title character string. Title of the table.
  #' @return Flextable object.
  
  library(flextable)
  library(officer)
  
  # ── Identify key row indices BEFORE modifying dataframe ──────────────────────
  # Convert is_header to logical in case it came in as character
  dataframe <- dataframe %>%
    mutate(is_header = as.logical(is_header))
  
  header_rows  <- which(dataframe$is_header == TRUE)
  section_ends <- c(header_rows[-1] - 1, nrow(dataframe))
  
  # Sub_label groups for vertical merging (skip header rows)
  non_header   <- dataframe %>% mutate(row_num = row_number()) %>% filter(!is_header)
  sl_rle       <- rle(paste(non_header$sub_label))
  sl_ends_nh   <- cumsum(sl_rle$lengths)
  sl_starts_nh <- c(1, head(sl_ends_nh, -1) + 1)
  sl_starts    <- non_header$row_num[sl_starts_nh]
  sl_ends      <- non_header$row_num[sl_ends_nh]
  needs_merge  <- which(sl_rle$lengths > 1)
  
  # ── Clean up dataframe for display ───────────────────────────────────────────
  dataframe <- dataframe %>%
    mutate(across(everything(), ~ifelse(is.na(.), "", as.character(.)))) %>%
    select(-is_header)
  
  # ── Rename columns ────────────────────────────────────────────────────────────
  colnames(dataframe) <- c("sub_label", "County", "col3", "col4",
                           "Mean", "Min", "Median", "Max")
  
  # ── Build flextable ───────────────────────────────────────────────────────────
  ft <- flextable(dataframe)
  
  # ── Remove default header ─────────────────────────────────────────────────────
  ft <- delete_part(ft, part = "header")
  
  # ── Font: Times New Roman throughout ─────────────────────────────────────────
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::color(ft, color = "black", part = "all")
  
  # ── Background: white everywhere ─────────────────────────────────────────────
  ft <- flextable::bg(ft, bg = "white", part = "all")
  
  # ── Header rows: bold ────────────────────────────────────────────────────────
  for (i in header_rows) {
    ft <- flextable::bold(ft, i = i, bold = TRUE, part = "body")
  }
  
  # ── Merge sub_label (col 1) vertically within each sub-group ─────────────────
  for (k in needs_merge) {
    sr <- sl_starts[k]
    er <- sl_ends[k]
    if (er > sr) {
      ft <- merge_at(ft, i = sr:er, j = 1, part = "body")
    }
  }
  
  # ── Borders: full grid ────────────────────────────────────────────────────────
  border_style <- fp_border(color = "black", width = 0.5)
  ft <- border_outer(ft, part = "all", border = border_style)
  ft <- border_inner_h(ft, part = "all", border = border_style)
  ft <- border_inner_v(ft, part = "all", border = border_style)
  ft <- fix_border_issues(ft)
  
  # ── Alignment ────────────────────────────────────────────────────────────────
  ft <- flextable::align(ft, align = "center", part = "all")
  ft <- flextable::valign(ft, valign = "center", part = "all")
  
  # ── Padding ──────────────────────────────────────────────────────────────────
  ft <- flextable::padding(ft, padding = 2, part = "all")
  
  # ── Column widths ─────────────────────────────────────────────────────────────
  ft <- width(ft, j = 1, width = 1.3)   # section/sub_label
  ft <- width(ft, j = 2, width = 0.9)   # county
  ft <- width(ft, j = 3, width = 0.9)   # col3
  ft <- width(ft, j = 4, width = 0.9)   # col4
  ft <- width(ft, j = 5, width = 0.5)   # mean
  ft <- width(ft, j = 6, width = 0.4)   # min
  ft <- width(ft, j = 7, width = 0.5)   # median
  ft <- width(ft, j = 8, width = 0.4)   # max
  
  # ── Caption ───────────────────────────────────────────────────────────────────
  ft <- set_caption(ft, caption = title)
  
  return(ft)
}


# fit to page function
FitFlextableToPage <- function(ft, pgwidth = 6){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}

# correlation function
cor_hosp_function <- function(dataframe,
                              county_name,
                              comparison_variable,
                              comparison_name,
                              ww_variable,
                              season_name){
  
  dataframe$comparison_variable <- dataframe[[comparison_variable]]
  dataframe$ww_variable <- dataframe[[ww_variable]]
  
  d_county <- dataframe %>%
    filter(season %in% season_name) %>%
    filter(county == county_name) 
  set.seed(100)
  case_cor <-  ci_cor(
    x = as.data.frame(cbind(log1p(d_county$ww_variable), 
                            log1p(d_county$comparison_variable))),
    probs = c(0.025, 0.95),
    method = "spearman",
    type = "bootstrap",
    R = 1000
  )
  spearman_statistic <- case_cor$estimate
  ll <- case_cor$interval[1]
  ul <- case_cor$interval[2]
  observations <- length(d_county$comparison_variable)
  county <- county_name
  comparison <- comparison_name
  row_df <- as.data.frame(cbind(paste(list(season_name)), county, comparison, 
                                signif(spearman_statistic, 3), 
                                signif(ll, 3), signif(ul, 3), observations))
  return(row_df)
}

# calculate correlations for each hospitalizations group
hosp_correlations_stack_function <- function(comparison_variable,
                                             hosp_group,
                                             ww_variable){
  
  # erie all months hosp data
  row_5 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Erie",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 21 to 39", "Weeks 40 to 20")
  ) %>%
    mutate(hosp_group = hosp_group)
  
  # jefferson all months hosp data
  row_6 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Jefferson",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 21 to 39", "Weeks 40 to 20")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # onondaga all months hosp data
  row_7 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Onondaga",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 21 to 39", "Weeks 40 to 20")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # westchester all months hosp data
  row_8 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Westchester",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 21 to 39", "Weeks 40 to 20")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # erie oct to may hosp data
  row_9 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Erie",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 40 to 20")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # jefferson oct to may hosp data
  row_10 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Jefferson",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 40 to 20")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # onondaga oct to may hosp data
  row_11 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Onondaga",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 40 to 20")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # westchester oct to may hosp data
  row_12 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Westchester",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 40 to 20")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # erie jun to sep hosp data
  row_13 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Erie",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 21 to 39")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # jefferson jun to sep hosp data
  row_14 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Erie",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 21 to 39")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # onondaga jun to sep hosp data
  row_15 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Onondaga",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 21 to 39")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # westchester jun to sep hosp data
  row_16 <- cor_hosp_function(
    dataframe = dat_county,
    county_name = "Westchester",
    comparison_variable = comparison_variable,
    ww_variable = ww_variable,
    comparison_name = "log+1(Hospitalizations ~ concentration)",
    season_name = c("Weeks 21 to 39")
  )%>%
    mutate(hosp_group = hosp_group)
  
  # bind rows
  df <- bind_rows(
    row_5, row_6, row_7, row_8,
    row_9, row_10, row_11, row_12, row_13, row_14,
    row_15, row_16
  )
  return(df)
}

# roc plot function
roc_plot_function <- function(data_input,
                              aggregation,
                              outcome,
                              outcome_name,
                              ww_variable,
                              grouping_var,
                              county_list,
                              season_include,
                              pop_var){
  
  # weekly data
  data_input$outcome <- data_input[[outcome]]
  data_input$ww_variable <- data_input[[ww_variable]]
  data_input$grouping_var <- data_input[[grouping_var]]
  data_input$pop_var <- data_input[[pop_var]]
  
  dat_sewershed_pilot_weekly <- 
    data_input %>%
    filter(county %in% county_list) %>%
    filter(season %in% season_include) %>%
    group_by(grouping_var, pop_var, week) %>%
    summarize(rsv_conc_weekly = mean(ww_variable, na.rm = TRUE),
              outcome_weekly = sum(outcome, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(!is.na(rsv_conc_weekly))
  
  dat_sewershed_pilot_weekly$outcome_10 <- ifelse(
    dat_sewershed_pilot_weekly$outcome_weekly > 1, 1, 0)
  # simple glm
  model_hosp <- glm(outcome_10 ~ rsv_conc_weekly, 
                    data = dat_sewershed_pilot_weekly,
                    family = binomial)
  
  # probabilities
  probs_mod1 <- predict(model_hosp, type = 'response')
  
  # roc curve
  roc_mod1 <- roc(response = dat_sewershed_pilot_weekly$outcome_10, 
                  predictor = probs_mod1)
  showtext_auto()
  mod1_plot <- ggroc(roc_mod1, legacy.axes = TRUE) +
    labs(x = 'False-positive rate', y = 'True-positive rate',
         title = 'ROC curve\nsurvived ~ pclass') +
    annotate('text', x = .5, y = .5, label = paste0('AUC: ', round(auc(roc_mod1), 
                                                                   digits = 2)))+
    theme_dth_1+
    labs(x = "1 - specificity (false-positive rate)",
         y = "Sensitivity (true-positive rate)",
         title = paste("ROC curve - ", aggregation, " level", sep = ""),
         subtitle = 
           paste("at least 1 weekly ", outcome_name," ~ RSV ww concentration", 
                 sep = "")
    )
  mod1_plot
  
  
  return(mod1_plot)
  showtext_end()
  
}

# by season roc plot
roc_plot_function_by_season <- function(data_input,
                                        aggregation,
                                        outcome,
                                        outcome_name,
                                        ww_variable,
                                        grouping_var,
                                        county_list,
                                        pop_var){
  
  # weekly data
  data_input$outcome <- data_input[[outcome]]
  data_input$ww_variable <- data_input[[ww_variable]]
  data_input$grouping_var <- data_input[[grouping_var]]
  data_input$pop_var <- data_input[[pop_var]]
  
  dat_sewershed_pilot_weekly <- 
    data_input %>%
    filter(county %in% county_list) %>%
    filter(season == "Weeks 40 to 20") %>%
    group_by(grouping_var, pop_var, week) %>%
    summarize(rsv_conc_weekly = mean(ww_variable, na.rm = TRUE),
              outcome_weekly = sum(outcome, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(!is.na(rsv_conc_weekly))
  
  dat_sewershed_pilot_weekly$outcome_10 <- ifelse(
    dat_sewershed_pilot_weekly$outcome_weekly > 1, 1, 0)
  # simple glm
  model_hosp <- glm(outcome_10 ~ rsv_conc_weekly, 
                    data = dat_sewershed_pilot_weekly,
                    family = binomial)
  
  # probabilities
  probs_mod1 <- predict(model_hosp, type = 'response')
  
  # roc curve
  roc_mod1 <- roc(response = dat_sewershed_pilot_weekly$outcome_10, 
                  predictor = probs_mod1)
  
  # plot all three seasons by color
  group <- "Weeks 40 to 20"
  AUC_winter <- round(auc(roc_mod1), digits = 2)
  winter <- cbind(coords(roc_mod1, "all"), group)
  
  # summer
  dat_sewershed_pilot_weekly <- 
    data_input %>%
    filter(county %in% county_list) %>%
    filter(season == "Weeks 21 to 39") %>%
    group_by(grouping_var, pop_var, week) %>%
    summarize(rsv_conc_weekly = mean(ww_variable, na.rm = TRUE),
              outcome_weekly = sum(outcome, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(!is.na(rsv_conc_weekly))
  
  dat_sewershed_pilot_weekly$outcome_10 <- ifelse(
    dat_sewershed_pilot_weekly$outcome_weekly > 1, 1, 0)
  # simple glm
  model_hosp <- glm(outcome_10 ~ rsv_conc_weekly, 
                    data = dat_sewershed_pilot_weekly,
                    family = binomial)
  
  # probabilities
  probs_mod1 <- predict(model_hosp, type = 'response')
  
  # roc curve
  roc_mod1 <- roc(response = dat_sewershed_pilot_weekly$outcome_10, 
                  predictor = probs_mod1)
  group <- "Weeks 21 to 39"
  AUC_summer <- round(auc(roc_mod1), digits = 2)
  summer <- cbind(coords(roc_mod1, "all"), group)
  
  # all weeks
  dat_sewershed_pilot_weekly <- 
    data_input %>%
    filter(county %in% county_list) %>%
    group_by(grouping_var, pop_var, week) %>%
    summarize(rsv_conc_weekly = mean(ww_variable, na.rm = TRUE),
              outcome_weekly = sum(outcome, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(!is.na(rsv_conc_weekly))
  
  dat_sewershed_pilot_weekly$outcome_10 <- ifelse(
    dat_sewershed_pilot_weekly$outcome_weekly > 1, 1, 0)
  # simple glm
  model_hosp <- glm(outcome_10 ~ rsv_conc_weekly, 
                    data = dat_sewershed_pilot_weekly,
                    family = binomial)
  
  # probabilities
  probs_mod1 <- predict(model_hosp, type = 'response')
  
  # roc curve
  roc_mod1 <- roc(response = dat_sewershed_pilot_weekly$outcome_10, 
                  predictor = probs_mod1)
  group <- "All weeks"
  AUC_all <- round(auc(roc_mod1), digits = 2)
  all <- cbind(coords(roc_mod1, "all"), group)
  
  # combine
  roc_df <- bind_rows(all, winter, summer)
  
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
         title = paste("ROC curve - ", aggregation, " level", sep = ""),
         subtitle = 
           paste("at least 1 weekly ", outcome_name," ~ RSV ww concentration", 
                 sep = "")
    )+
    scale_color_manual(values=met.brewer("Hiroshige", 3), name = "Season") +
    annotate('text', x = .75, y = .5,
             label = paste("AUC: ", AUC_all, sep = ""),
             color = pal[1]) +
    annotate('text', x = .75, y = .425,
             label = paste("AUC: ", AUC_summer, sep = ""),
             color = pal[2]) +
    annotate('text', x = .75, y = .35,
             label = paste("AUC: ", AUC_winter, sep = ""),
             color = pal[3])
  
  mod1_plot
  
  
  return(mod1_plot)
  showtext_end()
  
}
# extract legend function
g_legend<-function(a.gplot){
  # docstring information
  #' Extract legend from ggplot object
  #' 
  #' @description This function will pull the legend from a ggplot as a grob
  #' and let the user add the legend to a panel figure.
  #' 
  #' @param a.gplot ggplot object.
  #' 
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


# weekly data
lag_lead_function_df <- function(
    dataframe, 
    steps, 
    method,
    ww_var, 
    clin_var,
    plot,
    clin_name,
    county_group){
  
  if(county_group == "Yes"){
    
    dataframe$ww_var <- dataframe[[ww_var]]
    dataframe$clin_var <- dataframe[[clin_var]]
    
    # empty list
    datalist_lags <- list()
    
    # number of lags to test
    c <- 0:steps
    
    # for loop for daily lags
    for(i in as.character(c)){
      # create one lag for daily data and test correlation
      df3 <- dataframe %>%
        group_by(county) %>%
        arrange(-desc(week)) %>%
        mutate(
          ww_lag = lag(ww_var, as.integer(i), default = NA)
          
        )%>%
        ungroup()
      
      # correlation
      cor_result <- df3 %>%
        group_by(county) %>%
        summarize(
          cor = cor(log1p(ww_lag), log1p(clin_var) , use = "na.or.complete", method = method)
        ) %>%
        ungroup()
      cor_result$lag <- i
      
      datalist_lags[[i]] <- cor_result
    }
    weekly_lags <- do.call(rbind, datalist_lags)
    
    # id highest lag correlation
    lags <- weekly_lags %>%
      group_by(county) %>%
      mutate(highest_lag = ifelse(cor == max(cor, na.rm = TRUE),
                                  1, 0)
      )
    
    # lag plot only
    levels <- seq(from = -steps, to = steps, by = 1)
    
    # make it a factor
    lags$lag <- factor(lags$lag,
                       levels = levels)
    
    
    # lead time
    # empty list
    datalist_lags <- list()
    
    # number of lags to test
    c <- 0:steps
    
    # for loop for daily lags
    for(i in as.character(c)){
      # create one lag for daily data and test correlation
      df3 <- dataframe %>%
        group_by(county) %>%
        arrange(-desc(week)) %>%
        mutate(
          ww_lead = lead(ww_var, as.integer(i), default = NA)
          
        )%>%
        ungroup()
      
      # correlation
      cor_result <- df3 %>%
        group_by(county) %>%
        summarize(
          cor = cor(log1p(ww_lead), log1p(clin_var) , use = "na.or.complete", method = method)
        ) %>%
        ungroup()
      cor_result$lead <- i
      
      datalist_lags[[i]] <- cor_result
    }
    weekly_leads <- do.call(rbind, datalist_lags)
    
    # id highest lag correlation
    leads <- weekly_leads %>%
      group_by(county) %>%
      mutate(highest_lead = ifelse(cor == max(cor, na.rm = TRUE),
                                   1, 0)
      )
    
    leads$lag <- as.numeric(leads$lead) * -1
    leads$lag <- as.character(leads$lag)
    
    # combine dfs
    df <- bind_rows(lags, leads)
    
    levels <- seq(from = -steps, to = steps, by = 1)
    
    # make it a factor
    df$lag <- factor(df$lag,
                     levels = levels)
    
    # indicate highest value
    df <- df %>%
      group_by(county) %>%
      mutate(highest_lag = ifelse(cor == max(cor, na.rm = TRUE),
                                  1, 0)
      )
    return(df)
  } else if(county_group == "No"){
    
    dataframe$ww_var <- dataframe[[ww_var]]
    dataframe$clin_var <- dataframe[[clin_var]]
    
    # empty list
    datalist_lags <- list()
    
    # number of lags to test
    c <- 0:steps
    
    # for loop for daily lags
    for(i in as.character(c)){
      # create one lag for daily data and test correlation
      df3 <- dataframe %>%
        group_by(county) %>%
        arrange(-desc(week)) %>%
        mutate(
          ww_lag = lag(ww_var, as.integer(i), default = NA)
          
        )%>%
        ungroup()
      
      # correlation
      cor_result <- df3 %>%
        summarize(
          cor = cor(log1p(ww_lag), log1p(clin_var) , use = "na.or.complete", method = method)
        ) %>%
        ungroup()
      cor_result$lag <- i
      
      datalist_lags[[i]] <- cor_result
    }
    weekly_lags <- do.call(rbind, datalist_lags)
    
    # id highest lag correlation
    lags <- weekly_lags %>%
      mutate(highest_lag = ifelse(cor == max(cor, na.rm = TRUE),
                                  1, 0)
      )
    
    # lag plot only
    levels <- seq(from = -steps, to = steps, by = 1)
    
    # make it a factor
    lags$lag <- factor(lags$lag,
                       levels = levels)
    
    
    # lead time
    # empty list
    datalist_lags <- list()
    
    # number of lags to test
    c <- 0:steps
    
    # for loop for daily lags
    for(i in as.character(c)){
      # create one lag for daily data and test correlation
      df3 <- dataframe %>%
        group_by(county) %>%
        arrange(-desc(week)) %>%
        mutate(
          ww_lead = lead(ww_var, as.integer(i), default = NA)
          
        )%>%
        ungroup()
      
      # correlation
      cor_result <- df3 %>%
        summarize(
          cor = cor(log1p(ww_lead), log1p(clin_var) , use = "na.or.complete", method = method)
        ) %>%
        ungroup()
      cor_result$lead <- i
      
      datalist_lags[[i]] <- cor_result
    }
    weekly_leads <- do.call(rbind, datalist_lags)
    
    # id highest lag correlation
    leads <- weekly_leads %>%
      mutate(highest_lead = ifelse(cor == max(cor, na.rm = TRUE),
                                   1, 0)
      )
    
    leads$lag <- as.numeric(leads$lead) * -1
    leads$lag <- as.character(leads$lag)
    
    # combine dfs
    df <- bind_rows(lags, leads)
    
    levels <- seq(from = -steps, to = steps, by = 1)
    
    # make it a factor
    df$lag <- factor(df$lag,
                     levels = levels)
    
    # indicate highest value
    df <- df %>%
      mutate(highest_lag = ifelse(cor == max(cor, na.rm = TRUE),
                                  1, 0)
      )
    return(df)
  }
  
}


# model summary function
model_summary_function <- function(dataframe,
                                   outcome,
                                   model,
                                   glmer_option){
  # docstring information
  #' Output model summary information to a table
  #' 
  #' @description This function will take a model object in R and output a
  #' dataframe with the coefficients and model metrics.
  #' 
  #' @param dataframe dataframe. Dataframe containing the data used to fit the 
  #' model.
  #' @param outcome character vector. Outcome variable name from the model.
  #' @param model model object. Model fit to the data.
  #' @param glemer_option character string. Whether the model was fit using the
  #' glmer function.
  #' 
  #' @details This function will take a model fit using the data and output
  #' the coefficients, intercept, standard errors, t values, and p values
  #' for the model. Further, essential model metrics including the number of 
  #' observation, AIC, and Efron R2 are returned.
  
  #' @return Dataframe 
  
  if(glmer_option == "No"){
    
    dataframe$outcome_name <- dataframe[[outcome]]
    # Effron R2 - compares the actual to the predicted values 
    # against the residuals
    Actual <- dataframe$case_incidence
    Predicted = predict(model, type="response")
    Residuals = residuals(model)
    
    effron_r <- efronRSquared(residual = Residuals, 
                              predicted = Predicted, 
                              statistic = "EfronRSquared")
    effron_r <- as.data.frame(cbind("Efron R2", as.numeric(effron_r)))
    colnames(effron_r) <- c("variable", "Est")
    aic <- AIC(model)
    aic <- as.data.frame(cbind("AIC", aic))
    colnames(aic) <- c("variable", "Est")
    
    # model coefficients, se, and p values
    cc <- coef(summary(model))
    cc <- within(as.data.frame(cc),
                 {   `Std. Error` <- `Std. Error`
                 `t value` <- Estimate/`Std. Error`
                 `Pr(>|t|)` <- 2*pt(-abs(`t value`), df=length(dataframe)-1)
                 })
    
    #add ci
    #calculate se from dataframe
    se <- (cc$`Std. Error`)
    # table of estimates with 95% CI
    (tab <- cbind(Est = cc$Estimate, LL = cc$Estimate - 1.96 * se, 
                  UL = cc$Estimate + 1.96 *
                    se))
    tab2 <- cbind(cc, tab)
    
    # add stars based on the p value 
    tab2$`t value2` <- ifelse(tab2$`Pr(>|t|)` < 0.1, 
                              paste(round(tab2$`t value`, 2), "*", sep = ""), 
                              round(tab2$`t value`, 2))
    tab2$`t value2` <- ifelse(tab2$`Pr(>|t|)` < 0.05, 
                              paste(round(tab2$`t value`, 2), "**", sep = ""), 
                              round(tab2$`t value`, 2))
    tab2$`t value2` <- ifelse(tab2$`Pr(>|t|)` < 0.01, 
                              paste(round(tab2$`t value`, 2), "***", sep = ""), 
                              round(tab2$`t value`, 2))
    # add variable names
    tab2$variable <- row.names(tab2)
    # grab columns and export table
    tab_export <- tab2 %>% dplyr::select(variable, Est, `Std. Error`, `t value2`, 
                                         `Pr(>|t|)`)
    # add number of observations
    n <- nobs(model)
    n <- as.data.frame(cbind("n", n))
    colnames(n) <- c("variable", "Est")
    
    tab_export$Est <- as.character(tab_export$Est)
    tab_export_final <- bind_rows(tab_export, n, effron_r, aic)
    
    tab_export_final$Est <- round_signifi_function(
      as.numeric(tab_export_final$Est)
    )
    tab_export_final$`Std. Error` <- round_signifi_function(
      as.numeric(tab_export_final$`Std. Error`)
    )
    tab_export_final$`Pr(>|t|)` <- round_signifi_function(
      as.numeric(tab_export_final$`Pr(>|t|)`)
    )
    
    # round values
    tab_export_final <- tab_export_final %>%
      mutate_if(., is.numeric, round_signifi_function)
    
    return(tab_export_final)
  } else if(glmer_option == "Yes"){
    
    dataframe$outcome_name <- dataframe[[outcome]]
    
    # Effron R2 - compares the actual to the predicted values 
    # against the residuals
    Actual <- dataframe$case_incidence
    Predicted = predict(model, type="response")
    Residuals = residuals(model)
    
    effron_r <- efronRSquared(residual = Residuals, 
                              predicted = Predicted, 
                              statistic = "EfronRSquared")
    effron_r <- as.data.frame(cbind("Efron R2", as.numeric(effron_r)))
    colnames(effron_r) <- c("variable", "Est")
    aic <- AIC(model)
    aic <- as.data.frame(cbind("AIC", aic))
    colnames(aic) <- c("variable", "Est")
    
    # model coefficients, se, and p values
    cc <- coef(summary(model))
    cc <- within(as.data.frame(cc),
                 {   `Std. Error` <- `Std. Error`
                 `t value` <- Estimate/`Std. Error`
                 `Pr(>|t|)` <- 2*pt(-abs(`t value`), df=length(dataframe)-1)
                 })
    
    #add ci
    #calculate se from dataframe
    se <- (cc$`Std. Error`)
    # table of estimates with 95% CI
    (tab <- cbind(Est = cc$Estimate, LL = cc$Estimate - 1.96 * se, UL = cc$Estimate + 1.96 *
                    se))
    tab2 <- cbind(cc, tab)
    
    # add stars based on the p value 
    tab2$`t value2` <- ifelse(tab2$`Pr(>|t|)` < 0.1, 
                              paste(round(tab2$`t value`, 2), "*", sep = ""), 
                              round(tab2$`t value`, 2))
    tab2$`t value2` <- ifelse(tab2$`Pr(>|t|)` < 0.05, 
                              paste(round(tab2$`t value`, 2), "**", sep = ""), 
                              round(tab2$`t value`, 2))
    tab2$`t value2` <- ifelse(tab2$`Pr(>|t|)` < 0.01, 
                              paste(round(tab2$`t value`, 2), "***", sep = ""), 
                              round(tab2$`t value`, 2))
    
    # add variable names
    tab2$variable <- row.names(tab2)
    # grab columns and export table
    tab_export <- tab2 %>% dplyr::select(variable, Est, `Std. Error`, `t value2`, 
                                         `Pr(>|t|)`)
    # add number of observations
    n <- nobs(model)
    n <- as.data.frame(cbind("n", n))
    colnames(n) <- c("variable", "Est")
    
    tab_export$Est <- as.character(tab_export$Est)
    tab_export_final <- bind_rows(tab_export, n, effron_r, aic)
    
    tab_export_final$Est <- as.numeric(tab_export_final$Est)
    
    # round values
    tab_export_final <- tab_export_final %>%
      mutate_if(., is.numeric, round_signifi_function)
    
    return(tab_export_final)
  } else if(glmer_option == "gls"){
    
    dataframe$outcome_name <- dataframe[[outcome]]
    
    # Effron R2 - compares the actual to the predicted values 
    # against the residuals
    Actual <- dataframe$case_incidence
    Predicted = predict(model, type="response")
    Residuals = residuals(model)
    
    effron_r <- efronRSquared(residual = Residuals, 
                              predicted = Predicted, 
                              statistic = "EfronRSquared")
    effron_r <- as.data.frame(cbind("Efron R2", as.numeric(effron_r)))
    colnames(effron_r) <- c("variable", "Est")
    aic <- AIC(model)
    aic <- as.data.frame(cbind("AIC", aic))
    colnames(aic) <- c("variable", "Est")
    
    # model coefficients, se, and p values
    cs <- as.data.frame(summary(model)$tTable)
    cc <- within(as.data.frame(cs),
                 {   `Std.Error` <- `Std.Error`
                 `t-value` <- Value/`Std.Error`
                 `p-value` <- 2*pt(-abs(`t-value`), df=length(dataframe)-1)
                 })
    
    #add ci
    #calculate se from dataframe
    se <- (cc$`Std.Error`)
    # table of estimates with 95% CI
    (tab <- cbind(Est = cc$Value, LL = cc$Value - 1.96 * se, UL = cc$Value + 1.96 *
                    se))
    tab2 <- cbind(cc, tab)
    
    # add stars based on the p value 
    tab2$`t value2` <- ifelse(tab2$`p-value` < 0.1, 
                              paste(round(tab2$`t-value`, 2), "*", sep = ""), 
                              round(tab2$`t-value`, 2))
    tab2$`t value2` <- ifelse(tab2$`p-value` < 0.05, 
                              paste(round(tab2$`t-value`, 2), "**", sep = ""), 
                              round(tab2$`t-value`, 2))
    tab2$`t value2` <- ifelse(tab2$`p-value` < 0.01, 
                              paste(round(tab2$`t-value`, 2), "***", sep = ""), 
                              round(tab2$`t-value`, 2))
    
    # add variable names
    tab2$variable <- row.names(tab2)
    # grab columns and export table
    tab_export <- tab2 %>% dplyr::select(variable, Est, `Std.Error`, `t value2`, 
                                         `p-value`)
    # add number of observations
    n <- nobs(model)
    n <- as.data.frame(cbind("n", n))
    colnames(n) <- c("variable", "Est")
    
    tab_export$Est <- as.character(tab_export$Est)
    tab_export_final <- bind_rows(tab_export, n, effron_r, aic)
    
    tab_export_final$Est <- as.numeric(tab_export_final$Est)
    
    # round values
    tab_export_final <- tab_export_final %>%
      mutate_if(., is.numeric, round_signifi_function)
    
    return(tab_export_final)
  } else if(glmer_option == "glmmtmb"){
    # Effron R2 - compares the actual to the predicted values 
    # against the residuals
    dataframe$outcome_name <- dataframe$rsv_hosp_50_100
    
    Actual <- dataframe$outcome_name
    Predicted = predict(model , type="response")
    Residuals = residuals(model )
    
    effron_r <- efronRSquared(residual = Residuals, 
                              predicted = Predicted, 
                              statistic = "EfronRSquared")
    effron_r
    
    r2 <- effron_r
    
    
    r2 <- as.data.frame(cbind(names(r2), r2))
    colnames(r2) <- c("variable", "Est")
    aic <- AIC(model)
    aic <- as.data.frame(cbind("AIC", aic))
    colnames(aic) <- c("variable", "Est")
    
    # model coefficients, se, and p values
    cc <- coef(summary(model))
    cc <- within(as.data.frame(cc$cond),
                 {   `Std. Error` <- `Std. Error`
                 `t value` <- Estimate/`Std. Error`
                 `Pr(>|t|)` <- 2*pt(-abs(`t value`), df=length(dataframe)-1)
                 })
    
    #add ci
    #calculate se from dataframe
    se <- (cc$`Std. Error`)
    # table of estimates with 95% CI
    (tab <- cbind(Est = cc$Estimate, LL = cc$Estimate - 1.96 * se, UL = cc$Estimate + 1.96 *
                    se))
    tab2 <- cbind(cc, tab)
    
    # add stars based on the p value 
    tab2$`t value2` <- ifelse(tab2$`Pr(>|t|)` < 0.1, 
                              paste(round(tab2$`t value`, 2), "*", sep = ""), 
                              round(tab2$`t value`, 2))
    tab2$`t value2` <- ifelse(tab2$`Pr(>|t|)` < 0.05, 
                              paste(round(tab2$`t value`, 2), "**", sep = ""), 
                              round(tab2$`t value`, 2))
    tab2$`t value2` <- ifelse(tab2$`Pr(>|t|)` < 0.01, 
                              paste(round(tab2$`t value`, 2), "***", sep = ""), 
                              round(tab2$`t value`, 2))
    
    # add variable names
    tab2$variable <- row.names(tab2)
    # grab columns and export table
    tab_export <- tab2 %>% dplyr::select(variable, Est, `Std. Error`, `t value2`, 
                                         `Pr(>|z|)`)
    # add number of observations
    n <- nobs(model)
    n <- as.data.frame(cbind("n", n))
    colnames(n) <- c("variable", "Est")
    
    tab_export$Est <- as.character(tab_export$Est)
    tab_export_final <- bind_rows(tab_export, n, r2, aic)
    
    tab_export_final$Est <- as.numeric(tab_export_final$Est)
    
    # round values
    tab_export_final <- tab_export_final %>%
      mutate_if(., is.numeric, round_signifi_function)
    
    return(tab_export_final)
  }
  
}

# for results, round values >1, if <1, report 3 significant figures
round_signifi_function <- function(x){
  # docstring information
  #' Round numeric data
  #' 
  #' @description This function will round values greater than 1 to 3 places and
  #' report 3 significant digits for values less than 1.
  #' 
  #' @param x vector. Numeric vector.
  
  ifelse(x >1,
         round(x, 3),
         signif(x, 4)
  )
}


# ------------------------------------------------------------------------------
# Helper: format p-values as "<0.001" or 3 decimal places
# ------------------------------------------------------------------------------
format_pvalue <- function(x) {
  ifelse(is.na(x), "",
         ifelse(as.numeric(x) < 0.001, "<0.001",
                formatC(as.numeric(x), digits = 3, format = "f")))
}
