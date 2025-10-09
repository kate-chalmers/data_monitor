break_wrap <- function (string, width = 80, indent = 0, exdent = 0, whitespace_only = TRUE) 
{
  out <- stringi::stri_wrap(string, width = width, indent = indent, 
                   exdent = exdent, whitespace_only = whitespace_only, simplify = FALSE)
  vapply(out, str_c, collapse = "<br>", character(1))
}

areaPlotter <- function(df, country_name) {
  
  # df <- mats_ts[["1_1"]]
  # country_name <- "Denmark"
  
  min_val <- min(df$obs_value) - (sd(df$obs_value)*2)
  max_val <- max(df$obs_value) + (sd(df$obs_value)*2)
  
  df <- df %>% mutate(time_period = as.integer(time_period),
                      obs_value = round(obs_value, round_val)) %>%
    rename(" " = "obs_value")
  
  df |>
    e_charts(time_period, height = 100) |>
    e_line(
      serie = ` `,
      areaStyle = list(opacity = 0.2),
      lineStyle = list(opacity = 100),
      itemStyle = list(opacity = 0)
    ) |>
    e_color(df$perf_val) |>
    e_x_axis(min = min(df$time_period),
             max = max(df$time_period),
             axisLabel = list(NULL), axisLine = list(NULL)) |>
    e_y_axis(min = min_val, max = max_val, axisLabel = list(NULL),
             axisLine = list(NULL)) |>
    e_tooltip(
      trigger = "axis",
      # In case country name in tooltip
      # <b style='font-size:10px!important;margin-bottom:0px;'>Country: </b>", country_name,"
      # <br>
      formatter = htmlwidgets::JS(paste0("
    function(params){

      // Start tooltip with red font color span
      let rez = `<span style='font-size:10px!important;line-height:0px;'>` +
                `<b style='font-size:10px!important;margin-bottom:0px;'>Year: </b>` + params[0].value[0] +
                `<br>`;

      params.forEach(function(item){
        rez += `<b style='font-size:10px!important'>Value:</b> ` + item.value[1];
      });

      // Close the red font span
      rez += '</span>';

      return rez;
    }
  "))
    )|>
    e_legend(show = F) |>
    e_grid(top=0,right=0,bottom=0,left=0) |>
    e_hide_grid_lines()
  
  
}

indicatorText <- function(avg_df, cluster_df) {
  
  cluster_text <- avg_df %>%
    filter(measure %in% cluster_df) %>%
    mutate(perf_val = ifelse(is.na(perf_val), "#999999", perf_val)) %>%
    count(perf_val) %>%
    # complete(perf_val = c("#0F8554", "goldenrod", "#CF597E", "#999999")) %>%
    mutate(n = ifelse(is.na(n), 0, n),
           cluster_text = case_when(
             perf_val == "#0F8554" ~ paste0("<b style='font-size:1.5rem'>", n,"</b> indicators have <b style='color:", perf_val,";font-size:1.5rem'>improved</b>."),
             perf_val == "goldenrod" ~ paste0("<b style='font-size:1.5rem'>", n,"</b> indicators have <b style='color:", perf_val,";font-size:1.5rem'>not seen significant change</b>."),
             perf_val == "#CF597E" ~ paste0("<b style='font-size:1.5rem'>", n,"</b> indicators have <b style='color:", perf_val,";font-size:1.5rem'>deteriorated</b>."),
             perf_val == "#999999" ~ paste0("<b style='font-size:1.5rem'>", n,"</b> indicators <b style='color:", perf_val,";font-size:1.5rem'>do not have enough data for evaluation</b>.")
           )) %>%
    arrange(-n) %>%
    pull(cluster_text) 
  
  cluster_text <- fluidRow(
    HTML("<span style='font-size:14px; display:inline-block;'>Since 2010 or the earliest available year, <br><br>"), 
    div(
             column(3, HTML(cluster_text[1])),
             column(3, HTML(cluster_text[2])),
             column(3, HTML(cluster_text[3])),
             column(3, HTML(cluster_text[4]))
        
    ),
    HTML("</span>")
  )
  
  return(cluster_text)
  
}


pillBox <- function(avg_df, cluster_filter) {
  
  # avg_df <- avg_vals %>% filter(ref_area == "DNK")
  # cluster_filter <- cwb_indicator_text_filter
  
  priority <- c("Improving", "No significant change", "Deteriorating", "Not enough data")
  
  plot_df <- avg_df %>% 
    filter(measure %in% cluster_filter) %>% 
    mutate(
      perf_val = ifelse(is.na(perf_val), "#999999", perf_val),
      perf_val_name = case_when(
        perf_val == "#0F8554"   ~ "Improving",
        perf_val == "goldenrod" ~ "No significant change",
        perf_val == "#CF597E"   ~ "Deteriorating",
        TRUE                    ~ "Not enough data"
      ),
      perf_val_light = case_when(
        perf_val == "#0F8554"   ~ "#d6e8e0",
        perf_val == "goldenrod" ~ "#f8edd8",
        perf_val == "#CF597E"   ~ "#f5e1e6",
        TRUE                    ~ "#ececec"
      )
    ) %>%
    count(perf_val, perf_val_light, perf_val_name) %>%
    mutate(
      n   = ifelse(is.na(n), 0, n),
      pct = 100 * n / sum(n)
    ) %>%
    arrange(match(perf_val_name, rev(priority))) %>%
    mutate(
      first_seg = row_number() == 1L,
      last_seg  = row_number() == n(),
      standard_style = "display:inline-block;height:30px;line-height:25px;"
    ) %>%
    mutate(
      html = case_when(
        first_seg ~ paste0("<span style='", standard_style,
                           "border: solid 1.5px ", perf_val, ";border-right:0px;",
                           "border-radius: 25px 0px 0px 25px; width:", pct, "%;",
                           "background:", perf_val_light,";'><b>", n, "</b></span>"),
        last_seg ~ paste0("<span style='", standard_style,
                          "border: solid 1.5px ", perf_val, ";border-left:0px;",
                          "border-radius: 0px 25px 25px 0px; width:", pct, "%;",
                          "background:", perf_val_light,";'><b>", n, "</b></span>"),
        TRUE ~ paste0("<span style='", standard_style,
                      "border: solid 1.5px ", perf_val, ";border-right:0px;border-left:0px;",
                      "width:", pct, "%;background:", perf_val_light,";'><b>", n, "</b></span>")
      )
    )
  
  HTML(paste0(plot_df$html, collapse = ""))
  
}

summaryBarPlot <- function(avg_df, cluster_filter) {
  
  # cluster_filter <- fwb_indicator_text_filter
  # avg_df <- avg_vals
  
  plot_df <- avg_df %>% 
    filter(measure %in% cluster_filter) %>% 
    mutate(perf_val = ifelse(is.na(perf_val), "#999999", perf_val),
           perf_val_name = case_when(
             perf_val == "#0F8554" ~ "Improving",
             perf_val == "goldenrod" ~ "No significant change",
             perf_val == "#CF597E" ~ "Deteriorating",
             perf_val == "#999999" ~ "Not enough data",
           )) %>%
    count(perf_val_name) %>%
    complete(perf_val_name = c("Not enough data", "Deteriorating", "No significant change", "Improving")) %>%
    mutate(n = ifelse(is.na(n), 0, n))
  
  plot_df %>%
    mutate(y = " ") %>%
    pivot_wider(names_from = "perf_val_name", values_from = "n") %>%
    e_charts(y) %>%
    e_bar(`Not enough data`, stack = "stack") |>
    e_bar(Deteriorating, stack = "stack") |>
    e_bar(`No significant change`, stack = "stack") |>
    e_bar(Improving, stack = "stack") |>
    e_color(scales::alpha(c("#999999", "#CF597E", "goldenrod", "#0F8554"), 0.8)) |>
    e_x_axis(axisLabel = list(NULL), axisLine = list(NULL)) |>
    e_y_axis(min = 0, max = sum(plot_df$n),
             axisLabel = list(NULL),
             axisLine = list(NULL)) |>
    e_labels(position = "inside") |>
    e_legend(orient = "horizontal") |>
    e_hide_grid_lines() |>
    e_flip_coords() |>
    e_tooltip() |>
    e_grid(top=15,right=0,bottom=0,left=0) 
  
}

timeSeriesAverage <- function(full_df, dimension_name) {
  
  # dimension_name <- c("F", "M")
  # full_df <- full_dat
  
  full_df <- full_df %>% filter(ref_area %in% oecd_countries)
  
  # Remove any years that are not complete enough to be considered
  tidy_df <- full_df %>%
    mutate(unit_measure = str_remove_all(unit_measure, "_SUB")) %>%
    # Calculate sample size
    group_by(measure, unit_measure, time_period, dimension) %>%
    mutate(n=n()) %>%
    ungroup() %>%
    # Remove any years that have < 10 sample (removes data from request reporting in non-SILC years or spotty recent data)
    filter(!n < 10)
  
  # OECD time series by dimension -----------------------------------------
  
  prepped_df <- tidy_df %>% filter(dimension %in% dimension_name)
  
  # List measures available in dimensions specified
  measures_avail <- prepped_df %>% distinct(measure) %>% pull()
  
  cleaned_df <- c()
  for(measure_name in measures_avail) {
    
    # List unit measures available in measure specified
    unit_measures_avail <- prepped_df %>% filter(measure == measure_name) %>% distinct(unit_measure) %>% pull()
    
    for(unit_measure_name in unit_measures_avail) {
      
      # Filter by each measure, unit_measure and dimension from the FULL data available for indicator
      short_df <- tidy_df %>% filter(measure == measure_name,
                                     unit_measure == unit_measure_name,
                                     dimension %in% c(dimension_name, "_T"))
      
      # List full set of countries available for indicator for full dimension
      ref_area_full <- short_df %>% distinct(ref_area) %>% pull()
      
      # Remove any countries that are missing from at least one of the three time periods
      removals <- short_df %>%
        group_by(time_period, dimension) %>%
        complete(ref_area = ref_area_full) %>%
        ungroup() %>%
        filter(is.na(obs_value)) %>%
        distinct(ref_area) %>%
        pull()
      
      # Loop over dimension values
      for(dimension_name_short in dimension_name) {
        
        short_df <- prepped_df %>%
          filter(measure == measure_name,
                 unit_measure == unit_measure_name,
                 dimension == dimension_name_short) %>%
          # Remove countries that have missing values/aren't available for all dimension
          filter(!ref_area %in% removals) %>%
          group_by(measure, unit_measure, dimension, time_period) %>%
          mutate(n = n()) %>%
          ungroup()
        
        cleaned_df <- rbind(cleaned_df, short_df)
        
      }
    }
  }
  
  # Calculate the average by measure, dimension, time period
  horizontal_ts <- cleaned_df %>%
    group_by(measure, unit_measure, dimension, time_period) %>%
    mutate(obs_value = mean(obs_value, na.rm=T),
           countries = paste0(ref_area, collapse = ", "),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD")) %>%
    slice(1) %>%
    ungroup()
  
  # Check same value is used across age bands
  nonMatchHorizontal <- horizontal_ts %>%
    group_by(measure, unit_measure, dimension) %>%
    mutate(var = var(n)) %>%
    ungroup() %>%
    filter(!var == 0) %>%
    nrow()
  
  # Stop function if any number mismatches exist
  if(nonMatchHorizontal != 0L) {
    stop(print("Horizontal averages do not match! Check script"))
  }
  
  # Create list of countries used
  match_list <- horizontal_ts %>%
    select(measure, unit_measure, ref_area = countries) %>%
    separate_rows(ref_area) %>%
    distinct()
  
  # Match countries to create overall OECD average --------------------------
  # This is redundant if dimension is set to _T but useful for horizontal comparisons
  
  oecd_ts <- tidy_df %>%
    filter(dimension == "_T") %>%
    # Drop any countries not in match list by merging
    merge(match_list, by = c("measure", "unit_measure", "ref_area")) %>%
    group_by(measure, unit_measure, dimension, time_period) %>%
    mutate(n = n(),
           obs_value = mean(obs_value, na.rm=T),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD")) %>%
    slice(1) %>%
    ungroup()
  
  # Check same value is used across age bands
  nonMatchAverage <- oecd_ts %>%
    group_by(measure, unit_measure, dimension) %>%
    mutate(var = var(n)) %>%
    ungroup() %>%
    filter(!var == 0) %>%
    nrow()
  
  # Stop function if any number mismatches exist
  if(nonMatchAverage != 0L) {
    stop(print("OECD averages do not match! Check script"))
  }
  
  # Test if both horizontal and OECD averages match
  test_horiz <- horizontal_ts %>% select(measure, unit_measure, horizon = ref_area) %>% distinct()
  test_oecd <- oecd_ts %>% select(measure, unit_measure, average = ref_area) %>% distinct()
  
  # Count number of mismatches
  testMatch <- test_horiz %>% merge(test_oecd) %>% filter(!horizon == average) %>% nrow()
  
  # Stop function if any number mismatches exist
  if(testMatch != 0L) {
    stop(print("OECD and horizontal averages do not match! Check script"))
  }
  
  # Return oecd average only if _T otherwise horizontal averages + oecd average
  if(all(dimension_name == "_T")) {
    full_average <- oecd_ts %>% select(-n)
  } else {
    full_average <- oecd_ts %>%
      select(-n) %>%
      rbind(horizontal_ts %>% select(-n, -countries))
  }
  
  return(full_average)
  
}

threePointAverage <- function(full_df, dimension_name) {
  
  # dimension_name <- c("_T")
  # full_df <- tidy_dat
  
  full_df <- full_df %>% filter(ref_area %in% oecd_countries)
  
  # Remove any years that are not complete enough to be considered
  filtered_df <- full_df %>%
    group_by(measure, unit_measure, time_period, dimension) %>%
    mutate(n=n()) %>%
    ungroup() %>%
    filter(!n < 5)
  
  # Set earliest value band
  earliest_df <- filtered_df %>%
    filter(time_period <= 2015) %>%
    group_by(ref_area, measure, unit_measure, dimension) %>%
    filter(time_period == min(time_period)) %>%
    ungroup() %>%
    mutate(label = "earliest")
  
  # Set precovid value band
  precovid_df <- filtered_df %>%
    filter(time_period %in% 2016:2019) %>%
    group_by(ref_area, measure, unit_measure, dimension) %>%
    filter(time_period == max(time_period)) %>%
    ungroup() %>%
    mutate(label = "precovid")
  
  # Set latest value band
  latest_df <- filtered_df %>%
    filter(time_period > 2019) %>%
    group_by(ref_area, measure, unit_measure, dimension) %>%
    filter(time_period == max(time_period)) %>%
    ungroup() %>%
    mutate(label = "latest")
  
  # Combine groups
  tidy_df <- rbind(earliest_df, precovid_df, latest_df) %>%
    mutate(unit_measure = str_remove_all(unit_measure, "_SUB"))
  
  # OECD average by dimension -----------------------------------------------
  
  # Filter dimensions wanted and remove _SUB in unit_measure (for matching)
  prepped_df <- tidy_df %>% filter(dimension %in% dimension_name)
  
  # List measures available in dimensions specified
  measures_avail <- prepped_df %>% distinct(measure) %>% pull()
  
  cleaned_df <- c()
  for(measure_name in measures_avail) {
    
    # List unit measures available in measure specified
    unit_measures_avail <- prepped_df %>% filter(measure == measure_name) %>% distinct(unit_measure) %>% pull()
    
    for(unit_measure_name in unit_measures_avail) {
      
      # Filter by each measure, unit_measure and dimension from the FULL data available for indicator
      short_df <- tidy_df %>% filter(measure == measure_name,
                                     unit_measure == unit_measure_name,
                                     dimension %in% c(dimension_name, "_T"))
      
      # List full set of countries available for indicator for full dimension
      ref_area_full <- short_df %>% distinct(ref_area) %>% pull()
      
      # Remove any countries that are missing from at least one of the three time periods
      removals <- short_df %>%
        group_by(label, dimension) %>%
        complete(ref_area = ref_area_full) %>%
        ungroup() %>%
        filter(is.na(obs_value)) %>%
        distinct(ref_area) %>%
        pull()
      
      # Loop over dimension values
      for(dimension_name_short in dimension_name) {
        
        short_df <- prepped_df %>%
          filter(measure == measure_name,
                 unit_measure == unit_measure_name,
                 dimension == dimension_name_short) %>%
          # Remove countries that have missing values/aren't available for all dimension
          filter(!ref_area %in% removals) %>%
          group_by(measure, unit_measure, dimension, label) %>%
          mutate(n = n()) %>%
          ungroup()
        
        cleaned_df <- rbind(cleaned_df, short_df)
        
      }
      
    }
  }
  
  # Calculate the average by measure, dimension, time period
  horizontal_average <- cleaned_df %>%
    group_by(measure, unit_measure, dimension, label) %>%
    mutate(obs_value = mean(obs_value, na.rm=T),
           countries = paste0(ref_area, collapse = ", "),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD"),
           time_period = case_when(label %in% c("precovid", "latest") ~ max(time_period), TRUE ~ min(time_period))) %>%
    slice(1) %>%
    ungroup()
  
  # Check same value is used across age bands
  nonMatchHorizontal <- horizontal_average %>%
    group_by(measure, unit_measure, dimension) %>%
    mutate(var = var(n)) %>%
    ungroup() %>%
    filter(!var == 0) %>%
    nrow()
  
  # Stop function if any number mismatches exist
  if(nonMatchHorizontal != 0L) {
    stop(print("Horizontal averages do not match! Check script"))
  }
  
  # Create list of countries used
  match_list <- horizontal_average %>%
    select(measure, unit_measure, ref_area = countries) %>%
    separate_rows(ref_area) %>%
    distinct()
  
  # Match countries to create overall OECD average --------------------------
  # This is redundant if dimension is set to _T but useful for horizontal comparisons
  
  oecd_average <- tidy_df %>%
    filter(dimension == "_T") %>%
    # Drop any countries not in match list by merging
    merge(match_list, by = c("measure", "unit_measure", "ref_area")) %>%
    group_by(measure, unit_measure, dimension, label) %>%
    mutate(n = n(),
           obs_value = mean(obs_value, na.rm=T),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD"),
           time_period = case_when(label %in% c("precovid", "latest") ~ max(time_period),
                                   TRUE ~ min(time_period))) %>%
    slice(1) %>%
    ungroup()
  
  # Check same value is used across age bands
  nonMatchAverage <- oecd_average %>%
    group_by(measure, unit_measure, dimension) %>%
    mutate(var = var(n)) %>%
    ungroup() %>%
    filter(!var == 0) %>%
    nrow()
  
  # Stop function if any number mismatches exist
  if(nonMatchAverage != 0L) {
    stop(print("OECD averages do not match! Check script"))
  }
  
  # Test if both horizontal and OECD averages match
  test_horiz <- horizontal_average %>% select(measure, unit_measure, horizon = ref_area) %>% distinct()
  test_oecd <- oecd_average %>% select(measure, unit_measure, average = ref_area) %>% distinct()
  
  # Count number of mismatches
  testMatch <- test_horiz %>% merge(test_oecd) %>% filter(!horizon == average) %>% nrow()
  
  # Stop function if any number mismatches exist
  if(testMatch != 0L) {
    stop(print("OECD and horizontal averages do not match! Check script"))
  }
  
  # Return oecd average only if _T otherwise horizontal averages + oecd average
  if(all(dimension_name == "_T")) {
    full_average <- oecd_average %>% select(-n)
  } else {
    full_average <- oecd_average %>%
      select(-n, -time_period) %>%
      rbind(horizontal_average %>% select(-countries, -n, -time_period))
  }
  
  return(full_average)
  
}

latestPointAverage <- function(full_df, dimension_name) {
  
  # dimension_name <- c("_T")
  # full_df <- tidy_dat
  
  full_df <- full_df %>% filter(ref_area %in% oecd_countries)
  
  # Remove any years that are not complete enough to be considered
  filtered_df <- full_df %>%
    group_by(measure, unit_measure, time_period, dimension) %>%
    mutate(n=n()) %>%
    ungroup() %>%
    filter(!n < 5)
  
  # Set latest value band
  latest_df <- filtered_df %>%
    filter(time_period >= 2010) %>%
    group_by(ref_area, measure, unit_measure, dimension) %>%
    filter(time_period == max(time_period)) %>%
    ungroup() %>%
    mutate(label = "latest")
  
  # Combine groups
  tidy_df <- rbind(latest_df) 
  
  # OECD average by dimension -----------------------------------------------
  
  # Filter dimensions wanted and remove _SUB in unit_measure (for matching)
  prepped_df <- tidy_df %>% filter(dimension %in% dimension_name)
  
  # List measures available in dimensions specified
  measures_avail <- prepped_df %>% distinct(measure) %>% pull()
  
  cleaned_df <- c()
  for(measure_name in measures_avail) {
    
    # List unit measures available in measure specified
    unit_measures_avail <- prepped_df %>% filter(measure == measure_name) %>% distinct(unit_measure) %>% pull()
    
    for(unit_measure_name in unit_measures_avail) {
      
      # Filter by each measure, unit_measure and dimension from the FULL data available for indicator
      short_df <- tidy_df %>% filter(measure == measure_name,
                                     unit_measure == unit_measure_name,
                                     dimension %in% c(dimension_name, "_T"))
      
      # List full set of countries available for indicator for full dimension
      ref_area_full <- short_df %>% distinct(ref_area) %>% pull()
      
      # Remove any countries that are missing from at least one of the three time periods
      removals <- short_df %>%
        group_by(label, dimension) %>%
        complete(ref_area = ref_area_full) %>%
        ungroup() %>%
        filter(is.na(obs_value)) %>%
        distinct(ref_area) %>%
        pull()
      
      # Loop over dimension values
      for(dimension_name_short in dimension_name) {
        
        short_df <- prepped_df %>%
          filter(measure == measure_name,
                 unit_measure == unit_measure_name,
                 dimension == dimension_name_short) %>%
          # Remove countries that have missing values/aren't available for all dimension
          filter(!ref_area %in% removals) %>%
          group_by(measure, unit_measure, dimension, label) %>%
          mutate(n = n()) %>%
          ungroup()
        
        cleaned_df <- rbind(cleaned_df, short_df)
        
      }
      
    }
  }
  
  # Calculate most common year for latest year label
  latest_year <- cleaned_df %>% 
    select(measure, time_period) %>%
    group_by(measure) %>%
    count(time_period) %>%
    filter(n == max(n)) %>%
    ungroup() %>%
    select(-n)
  
  # Calculate the average by measure, dimension, time period
  horizontal_average <- cleaned_df %>%
    group_by(measure, unit_measure, dimension, label) %>%
    mutate(obs_value = mean(obs_value, na.rm=T),
           countries = paste0(ref_area, collapse = ", "),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD")) %>%
    slice(1) %>%
    ungroup() %>%
    left_join(latest_year, by = "measure") 

  # Check same value is used across age bands
  nonMatchHorizontal <- horizontal_average %>%
    group_by(measure, unit_measure, dimension) %>%
    mutate(var = var(n)) %>%
    ungroup() %>%
    filter(!var == 0) %>%
    nrow()
  
  # Stop function if any number mismatches exist
  if(nonMatchHorizontal != 0L) {
    stop(print("Horizontal averages do not match! Check script"))
  }
  
  # Create list of countries used
  match_list <- horizontal_average %>%
    select(measure, unit_measure, ref_area = countries) %>%
    separate_rows(ref_area) %>%
    distinct()
  
  # Match countries to create overall OECD average --------------------------
  # This is redundant if dimension is set to _T but useful for horizontal comparisons
  
  oecd_average <- tidy_df %>%
    filter(dimension == "_T") %>%
    # Drop any countries not in match list by merging
    merge(match_list, by = c("measure", "unit_measure", "ref_area")) %>%
    group_by(measure, unit_measure, dimension, label) %>%
    mutate(n = n(),
           obs_value = mean(obs_value, na.rm=T),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD"),
           time_period = case_when(label %in% c("precovid", "latest") ~ max(time_period),
                                   TRUE ~ min(time_period))) %>%
    slice(1) %>%
    ungroup()
  
  # Check same value is used across age bands
  nonMatchAverage <- oecd_average %>%
    group_by(measure, unit_measure, dimension) %>%
    mutate(var = var(n)) %>%
    ungroup() %>%
    filter(!var == 0) %>%
    nrow()
  
  # Stop function if any number mismatches exist
  if(nonMatchAverage != 0L) {
    stop(print("OECD averages do not match! Check script"))
  }
  
  # Test if both horizontal and OECD averages match
  test_horiz <- horizontal_average %>% select(measure, unit_measure, horizon = ref_area) %>% distinct()
  test_oecd <- oecd_average %>% select(measure, unit_measure, average = ref_area) %>% distinct()
  
  # Count number of mismatches
  testMatch <- test_horiz %>% merge(test_oecd) %>% filter(!horizon == average) %>% nrow()
  
  # Stop function if any number mismatches exist
  if(testMatch != 0L) {
    stop(print("OECD and horizontal averages do not match! Check script"))
  }
  
  # Return oecd average only if _T otherwise horizontal averages + oecd average
  if(all(dimension_name == "_T")) {
    full_average <- oecd_average %>% select(-n)
  } else {
    full_average <- oecd_average %>%
      select(-n, -time_period) %>%
      rbind(horizontal_average %>% select(-countries, -n, -time_period))
  }
  
  return(full_average)
  
}
