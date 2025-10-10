source("./global.R")
library(tidymodels)

dict <- readxl::read_excel("S:/Data/WDP/Well being database/Automated database/output/dictionary.xlsx") %>%
  mutate(
    unit_tag_clean = unit_tag,
    unit_tag = case_when(
      position == "before" & !is.na(unit_tag) ~ paste0("<span style='font-size:24px'>", unit_tag, "</span> "),
      position == "after" & !unit_tag %in% c("%") & !is.na(unit_tag) ~ paste0("<br><span style='font-size:12px'>", unit_tag, "</span>"),
      is.na(unit_tag) ~ "",
      TRUE ~ unit_tag
    ),
    position = ifelse(is.na(position), "none", position),
    round_val = ifelse(is.na(round_val), 1, round_val)
  )




tidy_dat <- readRDS("./data/final dataset.RDS") %>%
  filter(age == "_T", education_lev == "_T", sex == "_T",
         time_period >= 2010,
         !measure %in% c("11_3_Sadness", "11_3_Anger", "11_3_Worry",
                         "11_3_Sadness_DEP", "11_3_Anger_DEP", "11_3_Worry_DEP",
                         "11_3_Wellrest", "11_3_Enjoy", "11_3_Laugh",
                         "11_3_Wellrest_DEP", "11_3_Enjoy_DEP", "11_3_Laugh_DEP",
                         "6_5_DEP")) %>%
  select(ref_area, time_period, measure, unit_measure, obs_value) %>%
  mutate(dimension = "_T") %>%
  left_join(dict %>% select(measure, threshold, direction), by=c("measure")) %>%
  distinct()


# Latest point data
gap_filler <- tidy_dat %>%
  distinct(measure, unit_measure) %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into=c("cat", "subcat")) %>%
  mutate(cat = as.numeric(cat),
         subcat = as.numeric(subcat)) %>%
  arrange(cat, subcat, measure) %>%
  select(-cat, -subcat) %>%
  mutate(measure = fct_infreq(measure)) %>%
  merge(., data.frame(ref_area = country_name_vector))

avg_vals <- threePointAverage(tidy_dat %>% filter(ref_area %in% oecd_countries), "_T")

avg_vals <- tidy_dat %>% 
  rbind(avg_vals %>% select(-label)) %>%
  group_by(ref_area, measure) %>%
  filter(time_period == max(time_period) | time_period == min(time_period)) %>%
  mutate(label = case_when(
    time_period == max(time_period) ~ "latest", 
    TRUE ~ "earliest")
    ) %>%
  ungroup() 

tiers_dat <- readRDS("./data/final dataset.RDS") %>%
  filter(age == "_T", education_lev == "_T", sex == "_T", time_period >= 2010,
         ref_area %in% oecd_countries) %>%
  select(ref_area, time_period, obs_value, measure) %>%
  group_by(measure, ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  left_join(dict %>% select(measure, direction)) %>%
  group_by(measure) %>%
  arrange(measure, obs_value) %>%
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
  select(measure, ref_area, tiers) %>%
  distinct() %>%
  complete(measure = unique(tidy_dat$measure)) %>%
  group_by(measure) %>%
  complete(ref_area = unique(country_name_vector)) %>%
  ungroup() %>%
  mutate(icon = case_when(
    tiers == 1 ~ "1-circle-fill.png",
    tiers == 2 ~ "2-circle-fill.png",
    tiers == 3 ~ "3-circle-fill.png",
    TRUE ~ "three-dots.png"
  )) %>%
  drop_na(icon)


cum_change_dat <- avg_vals %>%
  # This preserves the latest year 
  group_by(ref_area, measure) %>%
  mutate(time_period = max(time_period)) %>%
  ungroup() %>%
  pivot_wider(names_from = "label", values_from = "obs_value") %>%
  mutate(cum_change = latest - earliest,
         perf_val = case_when(
           direction == "positive" & cum_change > threshold ~ "good",
           direction == "negative" & cum_change < threshold * -1 ~ "good",
           direction == "positive" & cum_change < threshold * -1 ~ "bad",
           direction == "negative" & cum_change > threshold ~ "bad",
           direction == "positive" & (!cum_change > threshold | !cum_change < threshold * -1) ~ "neutral",
           direction == "negative" & (!cum_change < threshold * -1 | !cum_change > threshold ) ~ "neutral"
         ),
         perf_val = case_when(
           perf_val == "good" ~ "#0F8554",
           perf_val == "neutral" ~ "goldenrod",
           perf_val == "bad" ~ "#CF597E",
           TRUE ~ "#999999"
         ),
         explanation = case_when(
           is.na(cum_change) & !is.na(threshold) ~ paste0("Not enough data to calculate cumulative change"),
           is.na(cum_change) & is.na(threshold) ~ "Not enough data to calculate threshold",
           !is.na(cum_change) & is.na(threshold) ~ "We can prob calculate a threshold!"
         ),
         explanation = ifelse(!is.na(explanation), paste0("<span style='font-size:8px'>", explanation, "</span>"), explanation),
         ref_area = ifelse(grepl("OECD", ref_area), "OECD", ref_area)
  ) 

gap_filler <- gap_filler %>%
  anti_join(., cum_change_dat %>% select(ref_area, measure, unit_measure))

cum_change_dat <- cum_change_dat %>% merge(gap_filler, by = c("ref_area", "measure", "unit_measure"), all = T) 

oecd_latest <- latestPointAverage(tidy_dat %>% filter(ref_area %in% oecd_countries), "_T") %>%
  mutate(ref_area = "OECD") %>%
  select(measure, ref_area, time_period, latest = obs_value) 

latest_dat <- tidy_dat %>%
  group_by(measure, ref_area) %>%
  filter(time_period == max(time_period)) %>%
  ungroup() %>%
  select(measure, ref_area, time_period, latest = obs_value) %>%
  rbind(oecd_latest)

avg_vals <- cum_change_dat %>%
  mutate(measure2 = measure,
         ref_area = ifelse(grepl("OECD", ref_area), "OECD", ref_area)) %>%
  separate(measure2, into="cat") %>%
  distinct() %>% 
  select(-time_period, -latest) %>%
  left_join(tiers_dat %>% select(-tiers), by = c("measure", "ref_area")) %>%
  left_join(latest_dat, by = c("measure", "ref_area")) %>% 
  merge(dict %>% select(measure, label_name = label, unit_tag, round_val, position), by = "measure") %>%
  ungroup() %>%
  mutate(
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
    arrangement = case_when(
      icon == "1-circle-fill.png" & perf_val == "#0F8554" ~ 1,
      icon == "1-circle-fill.png" & perf_val == "goldenrod" ~ 2,
      icon == "1-circle-fill.png" & perf_val == "#CF597E" ~ 3,
      icon == "1-circle-fill.png" & perf_val == "#999999" ~ 4,
      icon == "2-circle-fill.png" & perf_val == "#0F8554" ~ 5,
      icon == "2-circle-fill.png" & perf_val == "goldenrod" ~ 6,
      icon == "2-circle-fill.png" & perf_val == "#CF597E" ~ 7,
      icon == "2-circle-fill.png" & perf_val == "#999999" ~ 8,
      icon == "3-circle-fill.png" & perf_val == "#0F8554" ~ 9,
      icon == "3-circle-fill.png" & perf_val == "goldenrod" ~ 10,
      icon == "3-circle-fill.png" & perf_val == "#CF597E" ~ 11,
      icon == "3-circle-fill.png" & perf_val == "#999999" ~ 12,
      icon == "three-dots.png" & perf_val == "#0F8554" ~ 13,
      icon == "three-dots.png" & perf_val == "goldenrod" ~ 14,
      icon == "three-dots.png" & perf_val == "#CF597E" ~ 15,
      icon == "three-dots.png" & perf_val == "#999999" ~ 16,
      TRUE ~ 17
    ),
    arrangement = case_when(
      grepl("OECD", ref_area) & perf_val == "#0F8554" ~ 1,
      grepl("OECD", ref_area) & perf_val == "goldenrod" ~ 2,
      grepl("OECD", ref_area) & perf_val == "#CF597E" ~ 3,
      grepl("OECD", ref_area) & perf_val == "#999999" ~ 4,
      TRUE ~ arrangement
    ),
    opacity = case_when(
      is.na(latest) ~ 0.6,
      TRUE ~ 1
    ),
    time_period = case_when(
      is.na(time_period) ~ " ",
      TRUE ~ as.character(time_period)
    ),
    perf_val = case_when(
      is.na(perf_val) ~ "#999999",
      TRUE ~ perf_val
    ),
    perf_val_name = case_when(
      perf_val == "#0F8554" ~ "Improving",
      perf_val == "goldenrod" ~ "No significant change",
      perf_val == "#CF597E" ~ "Deteriorating",
      perf_val == "#999999" ~ "Not enough data",
    )
  )


# Time series data
ts_vals <- timeSeriesAverage(tidy_dat %>% filter(ref_area %in% oecd_countries), "_T")

ts_vals <- tidy_dat %>% rbind(ts_vals)

cum_change_dat <- cum_change_dat %>% 
  select(ref_area, measure, unit_measure, perf_val) %>%
  mutate(ref_area = ifelse(grepl("OECD", ref_area), "OECD", ref_area))

ts_vals <- ts_vals %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into="cat") %>%
  distinct() %>%
  mutate(ref_area = ifelse(grepl("OECD", ref_area), "OECD", ref_area)) %>%
  select(-unit_measure) %>%
  merge(cum_change_dat, by = c("ref_area", "measure")) %>%
  left_join(dict %>% select(measure, round_val, unit_tag_clean, position), by = "measure") %>%
  mutate(
    perf_val = case_when(
      is.na(perf_val) ~ "#999999",
      TRUE ~ perf_val
    ),
    perf_val_name = case_when(
      perf_val == "#0F8554" ~ "Improving",
      perf_val == "goldenrod" ~ "No significant change",
      perf_val == "#CF597E" ~ "Deteriorating",
      perf_val == "#999999" ~ "Not enough data",
    ),
    obs_value_tidy = prettyNum(round(obs_value, round_val), big.mark = " "),
    obs_value_tidy = case_when(
      position == "after" & unit_tag_clean == "%" ~ paste0(obs_value_tidy, "%"),
      position == "before" ~ paste0("USD ", obs_value_tidy),
      position == "after" & unit_tag_clean == "%" ~ paste0(obs_value_tidy, " ", unit_tag_clean),
    )
  )


saveRDS(ts_vals, "./data/time series.RDS")
saveRDS(avg_vals, "./data/latest point data.RDS")

gap_filler <- avg_vals %>%
  distinct(measure) %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into=c("cat", "subcat")) %>%
  mutate(cat = as.numeric(cat),
         subcat = as.numeric(subcat)) %>%
  arrange(cat, subcat, measure) %>%
  select(-subcat) %>%
  mutate(measure = fct_infreq(measure))

saveRDS(gap_filler, "./data/gap filler.RDS")
