source("./global.R")

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
    # filter(time_period <= 2015) %>%
    group_by(ref_area, measure, unit_measure, dimension) %>%
    filter(time_period == min(time_period)) %>%
    ungroup() %>%
    mutate(label = "earliest")

  # # Set precovid value band
  # precovid_df <- filtered_df %>%
  #   filter(time_period %in% 2016:2019) %>%
  #   group_by(ref_area, measure, unit_measure, dimension) %>%
  #   filter(time_period == max(time_period)) %>%
  #   mutate(label = "precovid")

  # Set latest value band
  latest_df <- filtered_df %>%
    # filter(time_period > 2019) %>%
    group_by(ref_area, measure, unit_measure, dimension) %>%
    filter(time_period == max(time_period)) %>%
    ungroup() %>%
    mutate(label = "latest")

  # Combine groups
  tidy_df <- rbind(earliest_df, latest_df) %>%
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

latestPointAverage <- function(full_df, dimension_name) {

  # dimension_name <- group_list
  # full_df <- full_dat

  full_df <- full_df %>%
    filter(ref_area %in% oecd_countries) %>%
    mutate(time_period = as.numeric(time_period))

  # Remove any years that are not complete enough to be considered
  filtered_df <- full_df %>%
    group_by(measure, unit_measure, time_period, dimension) %>%
    mutate(n=n()) %>%
    ungroup() %>%
    filter(!n < 5)

  # Set latest value band
  latest_df <- filtered_df %>%
    filter(time_period >= 2010) %>%
    group_by(ref_area, measure, dimension) %>%
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
    select(-time_period) %>%
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
      select(-n) %>%
      rbind(horizontal_average %>% select(-countries, -n))
  }

  return(full_average)

}

earliestPointAverage <- function(full_df, dimension_name) {

  # dimension_name <- group_list
  # full_df <- full_dat

  full_df <- full_df %>%
    filter(ref_area %in% oecd_countries) %>%
    mutate(time_period = as.numeric(time_period))

  # Remove any years that are not complete enough to be considered
  filtered_df <- full_df %>%
    group_by(measure, unit_measure, time_period, dimension) %>%
    mutate(n=n()) %>%
    ungroup() %>%
    filter(!n < 5)

  # Set latest value band
  earliest_df <- filtered_df %>%
    filter(time_period >= 2010) %>%
    group_by(ref_area, measure, dimension) %>%
    filter(time_period == min(time_period)) %>%
    ungroup() %>%
    mutate(label = "earliest")

  # Combine groups
  tidy_df <- rbind(earliest_df)

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
  earliest_year <- cleaned_df %>%
    select(measure, time_period) %>%
    group_by(measure) %>%
    count(time_period) %>%
    filter(n == max(n)) %>%
    filter(time_period == min(time_period)) %>%
    ungroup() %>%
    select(-n)

  # Calculate the average by measure, dimension, time period
  horizontal_average <- cleaned_df %>%
    select(-time_period) %>%
    group_by(measure, unit_measure, dimension, label) %>%
    mutate(obs_value = mean(obs_value, na.rm=T),
           countries = paste0(ref_area, collapse = ", "),
           ref_area = case_when(!n==38 ~ paste0("OECD ", n), TRUE ~ "OECD")) %>%
    slice(1) %>%
    ungroup() %>%
    left_join(earliest_year, by = "measure")

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
           time_period = case_when(label %in% c("earliest") ~ min(time_period),
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

earliest_timeframe <- c(2010:2014)
medium_timeframe <- c(2015:2019)
latest_time_frame <- c(2019:2023)

baseline_timeframe <- earliest_timeframe

full_dat <- readRDS("/Users/Kate/OneDrive/WISE/hsl_dashboard/data/final dataset.RDS") %>%
  select(-obs_status, -base_per) %>%
  rownames_to_column() %>%
  pivot_longer(!c(rowname, measure, unit_measure, ref_area, time_period, obs_value),
               values_to="dimension") %>%
  group_by(rowname) %>%
  mutate(
    dimension = case_when(
      all(dimension == "_T") ~ "_T",
      dimension == "_T" ~ NA,
      TRUE ~ dimension
    ),
    time_period = as.numeric(time_period)
  ) %>%
  ungroup() %>%
  drop_na(dimension) %>%
  select(-name,-rowname) %>%
  distinct() %>%
  mutate(unit_measure = str_remove_all(unit_measure, "_SUB")) %>%
  filter(!measure %in% c(""),
         !dimension == "ISCED11_1",
         time_period >= 2010)


dict <- readxl::read_excel("./data/dictionary.xlsx") %>%
  mutate(label = case_when(
    is.na(label) & !is.na(indic) ~ indic,
    TRUE ~ label
  ),
  unit_tag = case_when(
    position == "before" & !is.na(unit_tag) ~ paste0("<span style='font-size:24px'>", unit_tag, "</span> "),
    position == "after" & !unit_tag %in% c("%") & !is.na(unit_tag) ~ paste0("<br><span style='font-size:12px'>", unit_tag, "</span>"),
    is.na(unit_tag) ~ "",
    TRUE ~ unit_tag
  )
  ) %>%
  distinct()

group_one <- "F"
group_two <- "M"
group_three <- "ISCED11_2_3"
group_four <- "ISCED11_5T8"
group_five <- "YOUNG"
group_six <- "MID"
group_seven <- "OLD"
group_list <- c(group_one, group_two, group_three, group_four, group_five, group_six, group_seven)


gap_filler <- full_dat %>%
  select(measure, dimension) %>%
  distinct() %>%
  filter(!dimension == "_T") %>%
  merge(., data.frame(ref_area = country_name_vector)) %>%
  group_by(ref_area, measure) %>%
  complete(dimension = group_list) %>%
  ungroup()

oecd_dat <- threePointAverage(full_dat, group_list) %>%
  pivot_wider(names_from = "label", values_from = "obs_value") %>%
  # mutate(latest = ifelse(is.na(latest) & !is.na(precovid), precovid, latest)) %>%
  pivot_longer(!c(measure, unit_measure, ref_area, dimension), names_to = "label", values_to = "obs_value") %>%
  filter(!label == "precovid") %>%
  mutate(ref_area = ifelse(grepl("OECD", ref_area), "OECD", ref_area))

tidy_dat <- full_dat %>%
  filter(dimension %in% c(group_list, "_T")) %>%
  group_by(ref_area, measure, unit_measure, dimension) %>%
  filter(time_period == max(time_period) | time_period == min(time_period)) %>%
  mutate(label = ifelse(time_period == max(time_period), "latest", "earliest")) %>%
  ungroup() %>%
  select(-time_period) %>%
  rbind(oecd_dat) %>%
  merge(dict %>% select(measure, unit_measure, direction)) %>%
  pivot_wider(names_from = "dimension", values_from = "obs_value") %>%
  mutate(test = case_when(
    is.na(`M`) & is.na(`ISCED11_2_3`) & is.na(`YOUNG`) ~ "drop",
    TRUE ~ "keep"
  )) %>%
  filter(test == "keep") %>%
  select(-test) %>%
  mutate(
    across(all_of(c(!!group_one, !!group_two, !!group_three, !!group_four,
                    !!group_five, !!group_six, !!group_seven)),
           ~ case_when(
             direction == "positive" ~ .x / `_T`,
             TRUE ~ `_T` / .x
           ), .names = "{.col}_ratio")

  )


# Lollipop
lollipop_dat <- tidy_dat %>%
  filter(label == "latest") %>%
  select(measure, ref_area, contains("ratio")) %>%
  pivot_longer(!c(measure, ref_area), names_to = "dimension", values_to = "ratio") %>%
  mutate(dimension = str_remove_all(dimension, "_ratio"),
         dimension_color = case_when(
           dimension == "YOUNG" ~ "#88CCEE",
           dimension == "MID" ~ "#CC6677",
           dimension == "OLD" ~ "#DDCC77",
           dimension == "M" ~ "#117733",
           dimension == "F" ~ "#332288",
           dimension == "ISCED11_2_3" ~ "#AA4499",
           dimension == "ISCED11_5T8" ~ "#44AA99"
         )) %>%
  distinct() %>%
  drop_na()

# # Growing fastest
# underline_dat <- tidy_dat %>%
#   filter(label == "latest") %>%
#   select(-unit_measure, -label, -contains("ratio")) %>%
#   pivot_longer(!c(measure, ref_area, direction), names_to = "dimension", values_to = "obs_value") %>%
#   drop_na(obs_value) %>%
#   filter(!dimension == "_T") %>%
#   mutate(
#     dimension_group = case_when(
#       dimension %in% c("F", "M") ~ "gender",
#       dimension %in% c("YOUNG", "MID", "OLD") ~ "age",
#       dimension %in% c("ISCED11_5T8", "ISCED11_2_3") ~ "educ",
#       dimension == "_T" ~ "average"
#     )
#   ) %>%
#   group_by(measure, ref_area, dimension_group) %>%
#   mutate(underline = case_when(
#     obs_value == max(obs_value) & direction == "positive" ~ "u",
#     obs_value == min(obs_value) & direction == "negative" ~ "u",
#     TRUE ~ "span"
#   )) %>%
#   select(-direction, -obs_value)

# Performance
time_series_color_int <- tidy_dat %>%
  select(ref_area, measure, label, all_of(group_list)) %>%
  pivot_longer(!c(ref_area, measure, label)) %>%
  drop_na(value) %>%
  pivot_wider(names_from = "label") %>%
  mutate(change = latest - earliest) %>%
  select(ref_area, measure, group = name, change)


time_series_color <- time_series_color_int %>%
  pivot_wider(names_from = "group", values_from = "change") %>%
  left_join(dict %>% select(measure, threshold, direction)) %>%
  mutate(
    # Assign significance if surpassing threshold
    across(
      .cols = all_of(c(group_one, group_two, group_three, group_four, group_five, group_six, group_seven)),
      .fns = ~ case_when(
        direction == "positive" & . > threshold ~ "#0F8554",
        direction == "negative" & . < threshold * -1 ~ "#0F8554",
        direction == "positive" & . < threshold * -1 ~ "#CF597E",
        direction == "negative" & . > threshold ~ "#CF597E",
        direction == "positive" & (!. > threshold | !. < threshold * -1) ~ "goldenrod",
        direction == "negative" & (!. < threshold * -1 | !. > threshold ) ~ "goldenrod",
        TRUE ~ "#999999"
      ),
      .names = "{.col}"
    )
  ) %>%
  select(ref_area, measure, all_of(group_list)) %>%
  pivot_longer(!c(ref_area, measure), names_to = "dimension", values_to = "perf_val") %>%
  mutate(
    perf_val_name = case_when(
      perf_val == "#0F8554" ~ "Improving",
      perf_val == "goldenrod" ~ "No significant change",
      perf_val == "#CF597E" ~ "Deteriorating",
      perf_val == "#999999" ~ "Not enough data",
    )
  )


# Pull indicators where all horizontal values exist
measure_list <- time_series_color %>% distinct(measure) %>% pull

tiers_dat <- full_dat %>%
  filter(measure %in% measure_list,
         !dimension %in% "_T",
         ref_area %in% oecd_countries) %>%
  group_by(ref_area, measure, dimension) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  select(-time_period) %>%
  left_join(dict %>% select(measure, direction)) %>%
  group_by(measure, dimension) %>%
  arrange(measure, dimension, obs_value) %>%
  mutate(rank =
           case_when(
             direction == "positive" ~ rank(obs_value, ties.method = "random"),
             TRUE ~ rank(-obs_value, ties.method = "random")
           ),
         rank_max = max(rank)
  ) %>%
  ungroup() %>%
  mutate(share = rank/rank_max,
         tiers = case_when(
           share < 0.33 ~ 3,
           share > 0.66 ~ 1,
           TRUE ~ 2
         )
  ) %>%
  ungroup() %>%
  select(measure, dimension, ref_area, tiers) %>%
  group_by(measure, dimension) %>%
  complete(ref_area = unique(country_name_vector)) %>%
  ungroup() %>%
  mutate(icon = case_when(
    tiers == 1 ~ "1-circle-fill",
    tiers == 2 ~ "2-circle-fill",
    tiers == 3 ~ "3-circle-fill",
    TRUE ~ "three-dots"
  )) %>%
  drop_na(icon) %>%
  select(-tiers)


gap_dat <- full_dat %>%
  filter(measure %in% measure_list) %>%
  group_by(ref_area, measure, dimension) %>%
  filter(time_period == max(time_period) | time_period == min(time_period)) %>%
  mutate(label = ifelse(time_period == max(time_period), "latest", "earliest")) %>%
  ungroup() %>%
  select(-time_period) %>%
  rbind(oecd_dat) %>%
  merge(dict %>% select(measure, direction)) %>%
  pivot_wider(names_from = "dimension", values_from = "obs_value") %>%
  mutate(
    F_gap_ratio = case_when(
      direction == "positive" ~ `F`/`_T`,
      direction == "negative" ~ `_T`/`F`
    ),
    M_gap_ratio = case_when(
      direction == "positive" ~ `M`/`_T`,
      direction == "negative" ~ `_T`/`M`
    ),
    YOUNG_gap_ratio = case_when(
      direction == "positive" ~ `YOUNG`/`_T`,
      direction == "negative" ~ `_T`/`YOUNG`
    ),
    MID_gap_ratio = case_when(
      direction == "positive" ~ `MID`/`_T`,
      direction == "negative" ~ `_T`/`MID`
    ),
    OLD_gap_ratio = case_when(
      direction == "positive" ~ `OLD`/`_T`,
      direction == "negative" ~ `_T`/`OLD`
    ),
    ISCED11_2_3_gap_ratio = case_when(
      direction == "positive" ~ `ISCED11_2_3`/`_T`,
      direction == "negative" ~ `_T`/`ISCED11_2_3`
    ),
    ISCED11_5T8_gap_ratio = case_when(
      direction == "positive" ~ `ISCED11_5T8`/`_T`,
      direction == "negative" ~ `_T`/`ISCED11_5T8`
    )
  ) %>%
  select(ref_area, measure, label, contains("ratio")) %>%
  pivot_longer(!c(measure, label, ref_area)) %>%
  pivot_wider(names_from = "label", values_from = "value") %>%
  drop_na(earliest, latest) %>%
  mutate(
    dimension = case_when(
      grepl("F_gap", name) ~ "F",
      grepl("M_gap", name) ~ "M",
      grepl("YOUNG_gap", name)  ~ "YOUNG",
      grepl("MID_gap", name) ~ "MID",
      grepl("OLD_gap", name) ~ "OLD",
      grepl("ISCED11_2_3", name) ~ "ISCED11_2_3",
      grepl("ISCED11_5T8", name) ~ "ISCED11_5T8"
    ),
    # See if gap is moving away/towards parity (1)
    gap_value = case_when(
      earliest > 1 & latest > 1 & earliest < latest ~ "widening",
      earliest > 1 & latest > 1 & earliest > latest ~ "narrowing",
      earliest < 1 & latest < 1 & earliest < latest ~ "narrowing",
      earliest < 1 & latest < 1 & earliest > latest ~ "widening",
      # This doesn't work great with education since we don't have primary
      earliest < 1 & latest > 1 ~ "Flip to better off than the population",
      earliest > 1 & latest < 1 ~ "Flip to worse off than the population",
      abs(earliest - latest) < 0.05 ~ "no change",
      TRUE ~ "no change"
    ),
    # Assign significance based on size of change
    gap = latest - earliest,
    gap_value = case_when(
      gap > 0.01 ~ gap_value,
      gap < -0.01 ~ gap_value,
      TRUE ~ "no change"
    )
  ) %>%
  select(ref_area, dimension, measure, gap_value, gap) %>%
  group_by(ref_area) %>%
  mutate(gap = scales::rescale(abs(gap), to = c(50, 100))) %>%
  ungroup()

oecd_latest <- latestPointAverage(full_dat %>% filter(ref_area %in% oecd_countries), group_list) %>%
  mutate(ref_area = "OECD") %>%
  select(measure, ref_area, dimension, latest_year = time_period, latest = obs_value)

latest_dat <- full_dat %>%
  group_by(measure, ref_area, dimension) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  select(measure, ref_area, dimension, latest_year = time_period, latest = obs_value) %>%
  rbind(oecd_latest) %>%
  filter(measure %in% unique(gap_filler$measure), !dimension == "_T")

oecd_earliest <- earliestPointAverage(full_dat %>% filter(ref_area %in% oecd_countries), group_list) %>%
  mutate(ref_area = "OECD") %>%
  select(measure, ref_area, dimension, earliest_year = time_period)

earliest_dat <- full_dat %>%
  group_by(measure, ref_area, dimension) %>%
  filter(time_period == min(time_period)) %>%
  ungroup() %>%
  select(measure, ref_area, dimension, earliest_year = time_period) %>%
  rbind(oecd_earliest) %>%
  filter(measure %in% unique(gap_filler$measure), !dimension == "_T")

time_series_dat <- full_dat %>%
  rbind(timeSeriesAverage(full_dat, group_list)) %>%
  filter(!dimension == "_T") %>%
  mutate(ref_area = ifelse(grepl("OECD", ref_area), "OECD", ref_area)) %>%
  merge(gap_filler, by = c("ref_area", "measure", "dimension"), all = T) %>%
  left_join(tiers_dat) %>%
  left_join(time_series_color) %>%
  left_join(gap_dat) %>%
  left_join(dict %>% select(measure, label, unit, unit_tag, round_val, direction, position)) %>%
  # left_join(underline_dat) %>%
  left_join(lollipop_dat) %>%
  left_join(latest_dat) %>%
  left_join(earliest_dat) %>%
  left_join(time_series_color_int %>% rename(dimension = group)) %>%
  arrange(measure, dimension, ref_area, time_period) %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into = "cat") %>%
  mutate(
    gap_value = ifelse(is.na(gap_value), "no data", gap_value),
    # underline = ifelse(is.na(underline), "span", "span"),
    icon = ifelse(is.na(icon), "three-dots", icon),
    value_tidy = case_when(!is.na(latest) ~ prettyNum(round(latest, round_val), big.mark = " ")),
    value_tidy = case_when(
      is.na(unit_tag) & !is.na(value_tidy) ~ paste0("<span style='font-size:24px;line-height:25px;'>", value_tidy, "</span>"),
      position == "before" & !is.na(unit_tag) & !is.na(value_tidy) ~ paste0(unit_tag, "<span style='font-size:24px;line-height:25px;'>", value_tidy, "</span>"),
      position == "after" & !is.na(unit_tag) & !is.na(value_tidy) ~ paste0("<span style='font-size:24px;line-height:25px;'>", value_tidy,"</span>", unit_tag),
      is.na(position) & !is.na(unit_tag) & !is.na(value_tidy) ~ paste0("<span style='font-size:24px;line-height:25px;'>", value_tidy,"</span>"),
      TRUE ~ paste("<span style='font-size:24px;'>No data</span><br>")
    ),
    image = case_when(
      cat == "1" ~ "income and wealth.png",
      cat == "3" ~ "housing.png",
      cat == "2" ~ "work and job quality.png",
      cat == "5" ~ "health.png",
      cat == "6" ~ "knowledge and skills.png",
      cat == "9" ~ "environmental quality.png",
      cat == "11" ~ "subjective wellbeing.png",
      cat == "10" ~ "safety.png",
      cat == "4" ~ "worklife balance.png",
      cat == "7" ~ "social connections.png",
      cat == "8" ~ "civic engagement.png",
      cat == "12" ~ "natural capital.png",
      cat == "13" ~ "human capital.png",
      cat == "14" ~ "social capital.png",
      cat == "15" ~ "economic capital.png"
    ),
    image_caption = str_remove_all(image, "\\.png"),
    image_caption = tools::toTitleCase(image_caption),
    dim_color = case_when(
      cat == "1" ~ "#3597d6",
      cat == "3" ~ "#2fa894",
      cat == "2" ~ "#1b7fba",
      cat == "5" ~ "#843773",
      cat == "6" ~ "#7eac3c",
      cat == "9" ~ "#1ba750",
      cat == "11" ~ "#f1612e",
      cat == "10" ~ "#606261",
      cat == "4" ~ "#912b22",
      cat == "7" ~ "#db4d5f",
      cat == "8" ~ "#daa923",
      cat == "12" ~ "darkblue",
      cat == "13" ~ "darkblue",
      cat == "14" ~ "darkblue",
      cat == "15" ~ "darkblue"
    ),
    dimension_long = case_when(
      dimension == "F" ~ "Women",
      dimension == "M" ~ "Men",
      dimension == "ISCED11_2_3" ~ "Secondary educated",
      dimension == "ISCED11_5T8" ~ "Tertiary educated",
      dimension == "YOUNG" ~ "Young adults",
      dimension == "MID" ~ "Middle-aged",
      dimension == "OLD" ~ "Older adults"
    )
  ) %>%
  drop_na(dimension_long) %>%
  mutate(
    dimension_tidy = paste0("<br><br><b style='font-size:12px;margin-bottom:5px;color:", dimension_color,";'>", break_wrap(dimension_long, 30), "</b><br><span style='font-size:1px'></span>"),
    dimension_group = case_when(
      dimension %in% c("F", "M") ~ "gender",
      dimension %in% c("YOUNG", "MID", "OLD") ~ "age",
      dimension %in% c("ISCED11_5T8", "ISCED11_2_3") ~ "educ",
      dimension == "_T" ~ "average"
     ),
    gap_image =  case_when(
      grepl("worse off", gap_value) ~ "flip to population.png",
      grepl("better off", gap_value) & dimension == "F" ~ "flip to group.png",
      grepl("better off", gap_value) & dimension == "M" ~ "flip to group.png",
      grepl("better off", gap_value) & dimension == "YOUNG" ~ "flip to group.png",
      grepl("better off", gap_value) & dimension == "MID" ~ "flip to group.png",
      grepl("better off", gap_value) & dimension == "OLD" ~ "flip to group.png",
      grepl("better off", gap_value) & dimension == "ISCED11_5T8" ~ "flip to group.png",
      grepl("better off", gap_value) & dimension == "ISCED11_2_3" ~ "flip to group.png",
      TRUE ~ paste0(tolower(gap_value), ".png")
    ),
    gap_value = tools::toTitleCase(gap_value),
    gap_value = case_when(
      gap_value %in% c("Narrowing") ~ paste0("Moving toward the population"),
      gap_value %in% c("Widening") ~ paste0("Moving away from the population"),
      gap_value == "No Change" ~ "No Change",
      TRUE ~ gap_value
    ),
    gap_value = str_wrap(gap_value, 15),
    opacity = ifelse(grepl("No data", value_tidy), 0.6, 1)
  ) %>%
  select(-cat)

saveRDS(time_series_dat, "./data/full inequalities data.RDS")

latest_total <- full_dat %>%
  filter(dimension == "_T", measure %in% measure_list) %>%
  merge(dict %>% select(unit_tag, round_val, position)) %>%
  mutate(
    value_tidy = case_when(!is.na(obs_value) ~ prettyNum(round(obs_value, round_val), big.mark = " ")),
    value_tidy = case_when(
      is.na(unit_tag) & !is.na(value_tidy) ~ paste0("<span style='font-size:24px;line-height:25px;'>", value_tidy, "</span>"),
      position == "before" & !is.na(unit_tag) & !is.na(value_tidy) ~ paste0(unit_tag, "<span style='font-size:24px;line-height:25px;'>", value_tidy, "</span>"),
      position == "after" & !is.na(unit_tag) & !is.na(value_tidy) ~ paste0("<span style='font-size:24px;line-height:25px;'>", value_tidy,"</span>", unit_tag),
      is.na(position) & !is.na(unit_tag) & !is.na(value_tidy) ~ paste0("<span style='font-size:24px;line-height:25px;'>", value_tidy,"</span>"),
      TRUE ~ paste("<span style='font-size:24px;'>No data</span><br>")
    )
  ) %>%
  group_by(ref_area, measure) %>%
  filter(time_period == max(time_period)) %>%
  ungroup()

saveRDS(latest_total, "./data/latest total values.RDS")



