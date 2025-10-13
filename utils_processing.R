
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

twoPointAverage <- function(full_df, dimension_name) {
  
  # full_df <- full_dat
  # dimension_name <- group_list
  
  full_df <- full_df %>% filter(ref_area %in% oecd_countries)
  
  # Remove any years that are not complete enough to be considered
  filtered_df <- full_df %>%
    group_by(measure, unit_measure, time_period, dimension) %>%
    mutate(n=n()) %>%
    ungroup() %>%
    filter(!n < 5)
  
  # Set earliest value band
  earliest_df <- filtered_df %>%
    filter(time_period <= 2019) %>%
    group_by(ref_area, measure, unit_measure, dimension) %>%
    filter(time_period == min(time_period)) %>%
    ungroup() %>%
    mutate(label = "earliest")
  
  # Set latest value band
  latest_df <- filtered_df %>%
    filter(time_period > 2019) %>%
    group_by(ref_area, measure, unit_measure, dimension) %>%
    filter(time_period == max(time_period)) %>%
    ungroup() %>%
    mutate(label = "latest")
  
  # Combine groups
  tidy_df <- rbind(earliest_df, latest_df) 
  
  # OECD average by dimension -----------------------------------------------
  
  # Filter dimensions wanted and remove _SUB in unit_measure (for matching)
  prepped_df <- tidy_df %>% filter(dimension %in% dimension_name, ref_area %in% oecd_countries)
  
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
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD")) %>%
    slice(1) %>%
    ungroup() %>%
    select(-time_period)
  
  horizontal_average <- cleaned_df %>%
    group_by(measure, unit_measure, dimension, label) %>%
    count(time_period) %>%
    filter(n == max(n)) %>%
    ungroup() %>%
    select(measure, dimension, label, time_period) %>%
    merge(horizontal_average, ., by = c("measure", "dimension", "label"))
  
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
      select(-n) %>%
      rbind(horizontal_average %>% select(-countries, -n))
  }
  
  return(full_average)
  
}

break_wrap <- function (string, width = 80, indent = 0, exdent = 0, whitespace_only = TRUE) {
  out <- stringi::stri_wrap(string, width = width, indent = indent,
                            exdent = exdent, whitespace_only = whitespace_only, simplify = FALSE)
  vapply(out, str_c, collapse = "<br>", character(1))
}

