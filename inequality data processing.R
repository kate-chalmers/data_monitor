source("./global_processing.R")

# Pull break treated data and clean for analysis
full_dat <- readRDS("S:/Data/WDP/Well being database/Automated database/output/break_treated_final_dataset.RDS") %>%
  filter(!dimension == "ISCED11_1", time_period >= 2010, !measure %in% dropped_indics) %>%
  select(ref_area, time_period, measure, dimension, unit_measure, obs_value) %>%
  distinct() %>%
  mutate(unit_measure = str_remove_all(unit_measure, "_SUB")) 

# group_one <- "F"
# group_two <- "M"
# group_three <- "ISCED11_2_3"
# group_four <- "ISCED11_5T8"
# group_five <- "YOUNG"
# group_six <- "MID"
# group_seven <- "OLD"
group_list <- c("F", "M", "ISCED11_2_3", "ISCED11_5T8", "YOUNG", "MID", "OLD")

gap_filler <- full_dat %>%
  select(measure, dimension) %>%
  distinct() %>%
  filter(!dimension == "_T") %>%
  merge(., data.frame(ref_area = c(all_countries, "OECD"))) %>%
  group_by(ref_area, measure) %>%
  complete(dimension = group_list) %>%
  ungroup() 

oecd_dat <- twoPointAverage(full_dat, group_list) %>%
  mutate(ref_area = ifelse(grepl("OECD", ref_area), "OECD", ref_area)) 

tidy_dat <- full_dat %>%
  filter(dimension %in% c(group_list, "_T")) %>%
  group_by(ref_area, measure, unit_measure, dimension) %>%
  filter(time_period == max(time_period) | time_period == min(time_period)) %>%
  mutate(label = ifelse(time_period == max(time_period), "latest", "earliest")) %>%
  ungroup() %>%
  rbind(oecd_dat) 


# Ratio -------------------------------------------------------------------

ratio_dat <- tidy_dat %>%
  select(-time_period) %>%
  pivot_wider(names_from = "dimension", values_from = "obs_value") %>%
  mutate(
    test = case_when(
      is.na(`M`) & is.na(`ISCED11_2_3`) & is.na(`YOUNG`) ~ "drop",
      TRUE ~ "keep")
  ) %>%
  filter(test == "keep") %>%
  select(-test) %>%
  merge(dict %>% select(measure, direction)) %>%
  mutate(
    across(all_of(!!group_list),
           ~ case_when(
             direction == "positive" ~ .x / `_T`,
             TRUE ~ `_T` / .x
           ), .names = "{.col}_ratio")

  )

# Pull indicators where all horizontal values exist
measure_list <- ratio_dat %>% distinct(measure) %>% pull


# Lollipop parity plots ---------------------------------------------------

lollipop_dat <- ratio_dat %>%
  filter(label == "latest") %>%
  select(measure, ref_area, contains("ratio")) %>%
  pivot_longer(!c(measure, ref_area), names_to = "dimension", values_to = "ratio") %>%
  mutate(dimension = str_remove_all(dimension, "_ratio"),
         dimension_color = case_when(
           dimension == "YOUNG" ~ "#88CCEE",
           dimension == "MID" ~ "#661100",
           dimension == "OLD" ~ "#DDCC77",
           dimension == "M" ~ "#117733",
           dimension == "F" ~ "#332288",
           dimension == "ISCED11_2_3" ~ "#AA4499",
           dimension == "ISCED11_5T8" ~ "#44AA99"
         )) %>%
  distinct() %>%
  drop_na()



# Cumulative change performance -------------------------------------------

time_series_color_int <- ratio_dat %>%
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
      .cols = all_of(group_list),
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


# Tiers  ------------------------------------------------------------------

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
  complete(ref_area = unique(all_countries)) %>%
  ungroup() %>%
  mutate(icon = case_when(
    tiers == 1 ~ "1-circle-fill",
    tiers == 2 ~ "2-circle-fill",
    tiers == 3 ~ "3-circle-fill",
    TRUE ~ "three-dots"
  )) %>%
  drop_na(icon) %>%
  select(-tiers)


# Gap icons ---------------------------------------------------------------

gap_dat <- tidy_dat %>%
  select(-time_period) %>%
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


# earliest and latest values ----------------------------------------------

latest_dat <- tidy_dat %>%
  filter(label == "latest", measure %in% measure_list, !dimension == "_T") %>%
  select(measure, ref_area, dimension, latest_year = time_period, latest = obs_value)
  
earliest_dat <- tidy_dat %>%
  filter(label == "earliest", measure %in% measure_list, !dimension == "_T") %>%
  select(measure, ref_area, dimension, earliest_year = time_period, earliest = obs_value)


# Time series and final cleaning ------------------------------------------

time_series_dat <- full_dat %>%
  rbind(timeSeriesAverage(full_dat, group_list)) %>%
  filter(!dimension == "_T") %>%
  mutate(ref_area = ifelse(grepl("OECD", ref_area), "OECD", ref_area)) %>%
  merge(gap_filler, by = c("ref_area", "measure", "dimension"), all = T) %>%
  left_join(tiers_dat) %>%
  left_join(time_series_color) %>%
  left_join(gap_dat) %>%
  left_join(dict %>% select(measure, label, unit, unit_tag, round_val, direction, position)) %>%
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
      (is.na(position) | position == "none") & !is.na(unit_tag) & !is.na(value_tidy) ~ paste0("<span style='font-size:24px;line-height:25px;'>", value_tidy,"</span>"),
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


saveRDS(time_series_dat, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/gap_page/data/full inequalities data.RDS")

latest_total <- tidy_dat %>%
  filter(dimension == "_T", measure %in% measure_list, label == "latest") %>%
  merge(dict %>% select(measure, unit_tag = unit_tag_clean, round_val, position), by = "measure") %>%
  mutate(
    value_tidy = case_when(!is.na(obs_value) ~ prettyNum(round(obs_value, round_val), big.mark = " ")),
    value_tidy = case_when(
      is.na(unit_tag) & !is.na(value_tidy) ~ paste0("", value_tidy, ""),
      position == "before" & !is.na(unit_tag) & !is.na(value_tidy) ~ paste0(unit_tag, "", value_tidy, ""),
      position == "after" & !is.na(unit_tag) & !is.na(value_tidy) & unit_tag == "%" ~ paste0("", value_tidy,"<span style='font-size:2rem'>", unit_tag, "</span>"),
      position == "after" & !is.na(unit_tag) & !is.na(value_tidy) & unit_tag == "%" ~ paste0("", value_tidy, " ", unit_tag),
      is.na(position) & !is.na(unit_tag) & !is.na(value_tidy) ~ paste0("", value_tidy,""),
      TRUE ~ paste("No data")
    )
  ) %>%
  select(ref_area, measure, value_tidy)

saveRDS(latest_total, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/gap_page/data/latest total values.RDS")



