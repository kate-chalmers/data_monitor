source("./global_processing.R")

comparison_versions <- paste0("./versioning/") %>%
  list.files(., full.names=T) %>%
  file.info() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(grepl("cwb_latest point", rowname)) %>%
  arrange(desc(mtime)) %>%
  slice_head(n = 2) %>%
  mutate(
    min_or_max = case_when(mtime == min(mtime) ~ "min", 
                        mtime == max(mtime) ~ "max", 
                        TRUE ~ "0")
  ) %>%
  filter(!min_or_max == "0") %>%
  select(rowname, min_or_max)

latest_version <- comparison_versions %>% filter(min_or_max == "max") %>% pull(rowname)
previous_version <- comparison_versions %>% filter(!min_or_max == "max") %>% pull(rowname)

latest_dat <- readRDS(latest_version) 

latest_dat %>% filter(measure == "1_1", ref_area == "AUT") %>% select(ref_area, latest)
  
latest_dat <- latest_dat %>%
  select(measure, ref_area, perf_val_name, icon) %>%
  rename(new_perf = perf_val_name, new_icon = icon) 

previous_dat <- readRDS(previous_version) 

previous_dat %>% filter(measure == "1_1", ref_area == "AUT") %>% select(ref_area, latest)

previous_dat <- previous_dat %>%
  select(measure, ref_area, perf_val_name, icon) %>%
  rename(old_perf = perf_val_name, old_icon = icon)


latest_dat %>% anti_join(previous_dat)
previous_dat %>% anti_join(latest_dat)

  
# Check changes
version_differences <- latest_dat %>% 
  merge(previous_dat) %>%
  mutate(
    improvements_in_ts = case_when(
      old_perf %in% c("No significant change", "Deteriorating") & new_perf %in% c("Improving") | old_perf == "Deteriorating" & new_perf == c("No significant change") ~ "Improved",
      old_perf == "Improving" & new_perf %in% c("Deteriorating", "No significant change") | old_perf == "No significant change" & new_perf == "Deteriorating" ~ "Worsened",
      old_perf == "Not enough data" & !new_perf == "Not enough data" ~ "Performance evaluation now available",
      new_perf == "Not enough data" & !old_perf == "Not enough data" ~ "Performance evaluation no longer available (likely due to break)",
      old_perf == new_perf ~ "no change",
      TRUE ~ "CHECK"
    ),
    improvement_in_position = case_when(
      old_icon %in% c("2-circle-fill.png", "3-circle-fill.png") & new_icon %in% c("1-circle-fill.png") | old_icon == "3-circle-fill.png" & new_icon == "2-circle-fill.png" ~ "Improved",
      old_icon %in% c("1-circle-fill.png") & new_icon %in% c("2-circle-fill.png", "3-circle-fill.png") | old_icon == "2-circle-fill.png" & new_icon == "3-circle-fill.png" ~ "Worsened",
      !old_icon == "three-dots.png" & new_icon == "three-dots.png" ~ "Ranking no longer available (likely data removed)",
      !new_icon == "three-dots.png" & old_icon == "three-dots.png" ~ "Ranking now available",
      old_icon == new_icon | is.na(old_icon) & is.na(new_icon) ~ "no change",
      TRUE ~ "CHECK"
    ),
    new_icon = case_when(
      new_icon == "1-circle-fill.png" ~ "Tier 1",
      new_icon == "2-circle-fill.png" ~ "Tier 2",
      new_icon == "3-circle-fill.png" ~ "Tier 3",
      TRUE ~ "No ranking"
    ),
    old_icon = case_when(
      old_icon == "1-circle-fill.png" ~ "Tier 1",
      old_icon == "2-circle-fill.png" ~ "Tier 2",
      old_icon == "3-circle-fill.png" ~ "Tier 3",
      TRUE ~ "No ranking"
    )
  ) %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into = c("cat", "subcat")) %>%
  mutate(cat = as.numeric(cat), subcat = as.numeric(subcat)) %>%
  arrange(cat, subcat) %>%
  mutate(measure = fct_inorder(measure)) %>%
  # Remove next update
  filter(!grepl("2_9", measure))


ranking_change <- version_differences %>%
  filter(!improvement_in_position == "no change") %>% 
  select(measure,ref_area, performance = new_perf, ranking_old = old_icon, ranking_new = new_icon, ranking_change = improvement_in_position) %>%
  merge(dict %>% select(measure, label)) %>%
  relocate(label, .before = everything()) %>%
  arrange(measure, ranking_change)


performance_change <- version_differences %>%
  filter(!improvements_in_ts == "no change") %>%
  # Remove next update
  filter(!measure == "6_1_DEP") %>%
  select(measure,ref_area, ranking = new_icon, performance_prev = old_perf, performance_new = new_perf, performance_change = improvements_in_ts) %>%
  merge(dict %>% select(measure, label)) %>%
  relocate(label, .before = everything()) %>%
  arrange(measure, performance_change)

performance_change %>% 
  count(label, performance_change) %>%
  arrange(label, -n)


# Check any losses
latest_dat %>%
  select(measure, ref_area) %>%
  anti_join(previous_dat %>% select(measure, ref_area))

previous_dat %>%
  select(measure, ref_area) %>%
  anti_join(latest_dat %>% select(measure, ref_area))


getwd()
require(openxlsx)
list_of_datasets <- list("Performance changes" = performance_change, "Tier changes" = ranking_change)
write.xlsx(list_of_datasets, file = "S:/Data/WDP/Well being database/Data Monitor/data_monitor/versioning/version differences/data monitor changes_feb2026.xlsx")

