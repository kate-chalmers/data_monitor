source("./utils_processing.R")
library(tidyverse)

oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", 
                    "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", 
                    "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", 
                    "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

partner_countries <- c("BRA", "ARG", "BGR", "HRV", "PER", "ROU", "IDN", "THA", "ZAF")

all_countries <- c(oecd_countries, partner_countries)

dropped_indics <- c(# Ad hoc indicators not on DE
                    "11_3_Sadness", "11_3_Anger", "11_3_Worry",
                    "11_3_Sadness_DEP", "11_3_Anger_DEP", "11_3_Worry_DEP",
                    "11_3_Wellrest", "11_3_Enjoy", "11_3_Laugh",
                    "11_3_Wellrest_DEP", "11_3_Enjoy_DEP", "11_3_Laugh_DEP",
                    # Duplicates to be removed from database
                    "6_2_DEP", "6_3_DEP",
                    # Yes/no indicators where mirror is included
                    "5_2_DEP", "10_2", "11_3_DEP", "7_1", "8_1", "14_3_DEP", "14_7_DEP")

# Not published data
inequality_dropped <- c("11_1_DEP", "2_9_DEP", "4_4_DEP", "7_3_DEP")

dict <- readxl::read_excel("S:/Data/WDP/Well being database/Automated database/output/dictionary.xlsx") %>%
  mutate(
    label = case_when(
      is.na(label) & !is.na(indic) ~ indic,
      TRUE ~ label
    ),
    unit_tag_clean = unit_tag,
    unit_tag_clean_fr = unit_tag_fr,
    unit_tag = case_when(
      position == "before" & !is.na(unit_tag) ~ paste0("<span style='font-size:24px'>", unit_tag, "</span> "),
      position == "after" & !unit_tag %in% c("%", "km2") & !is.na(unit_tag) ~ paste0("<br><span style='font-size:12px'>", unit_tag, "</span>"),
      position == "after" & unit_tag == " km2" & !is.na(unit_tag) ~ paste0("&nbsp;", unit_tag),
      is.na(unit_tag) ~ "",
      TRUE ~ unit_tag
    ),
    unit_tag_fr = case_when(
      position == "before" & !is.na(unit_tag_fr) ~ paste0("<span style='font-size:24px'>", unit_tag_fr, "</span> "),
      position == "after" & !unit_tag_fr %in% c("%", "km2") & !is.na(unit_tag_fr) ~ paste0("<br><span style='font-size:12px'>", unit_tag_fr, "</span>"),
      position == "after" & unit_tag == " km2" & !is.na(unit_tag) ~ paste0("&nbsp;", unit_tag),
      is.na(unit_tag_fr) ~ "",
      TRUE ~ unit_tag_fr
    ),
    position = ifelse(is.na(position), "none", position),
    round_val = ifelse(is.na(round_val), 1, round_val)
  )


skills_sig <- data.frame(ref_area = c(all_countries, "OECD")) %>%
  mutate(
    # reading
    sig2012.6_1 = case_when(
      ref_area %in% c("AUT", "CHL", "COL", "CZE", "DNK", "EST", "IRL", "ISR", "ITA", 
                      "LTU", "MEX", "NZL", "PRT", "SWE", "GBR", "USA", "ARG", "BRA",
                      "HRV", "KAZ", "MYS", "ROU", "SRB", "SGP") ~ "0",
      TRUE ~ "1"
    ),
    sig2015.6_1 = case_when(
      ref_area %in% c("AUS", "AUT", "CZE", "EST", "HUN", "IRL", "ISR", "ITA", "JPN", 
                      "KOR", "LTU", "MEX", "NZL", "SVK", "CHE", "GBR", "USA", "BRA",
                      "DOM", "MLT", "MDA", "MNE", "ROU", "SGP", "URY") ~ "0",
      TRUE ~ "1"
    ),
    # math
    sig2012.6_2 = case_when(
      ref_area %in% c("COL", "HUN", "ISR", "JPN", "LVA", "LTU", "SWE", "TUR", "GBR",
                      "HRV", "IDN", "MNE", "KAZ", "SRB", "SGP", "ARE", "URY") ~ "0",
      TRUE ~ "1"
    ),
    sig2015.6_2 = case_when(
      ref_area %in% c("AUS", "COL", "CZE", "HUN", "JPN", "KOR", "LVA", "LTU", "GBR", "USA",
                      "BRA", "HRV", "MDA", "PER", "ARE") ~ "0",
      TRUE ~ "1"
    ),
    # science
    sig2012.6_3 = case_when(
      ref_area %in% c("CAN", "CHL", "COL", "CZE", "DNK", "FRA", "HUN", "ISR", "JPN",
                      "KOR", "LVA", "LTU", "MEX", "NZL", "PRT", "SVK", "SWE", "TUR",
                      "USA", "ARG", "BRA", "HRV", "IDN", "KAZ", "MYS", "MNE", "ROU",
                      "SRB", "SGP") ~ "0",
      TRUE ~ "1"
    ),
    sig2015.6_3 = case_when(
      ref_area %in% c("AUS", "AUT", "CHL", "COL", "CZE", "IRL", "ISR", "ITA", "JPN",
                      "LVA", "MEX", "POL", "SVK", "SWE", "CHE", "USA", "BRA", "MLT",
                      "MNE", "ROU", "ARE", "URY") ~ "0",
      TRUE ~ "1"
    )
  ) %>%
  pivot_longer(!ref_area) %>%
  separate(name, into = c("sig", "measure"), sep = "\\.") %>%
  filter(sig == "sig2012") %>%
  select(-sig) %>%
  rename(sig = value)

oecd_avg_dat <- readRDS("//main.oecd.org/sdataWIS/Data/WDP/Well being database/Automated database/output/final dataset.RDS") %>%
  filter(grepl("OECD", ref_area)) %>%
  arrange(measure, time_period) %>%
  group_by(measure, obs_status) %>%
  mutate(time_period = as.numeric(time_period),
         drop = case_when(
           obs_status == "B" & time_period == max(time_period) ~ 1
         )
  ) %>%
  group_by(measure) %>%
  mutate(drop = zoo::na.locf(drop, fromLast = T, na.rm = F),
         drop = case_when(is.na(drop) ~ 0, TRUE ~ drop)) %>%
  ungroup() %>%
  filter(!drop == 1) %>%
  select(-drop, -base_per) %>%
  rownames_to_column() %>%
  pivot_longer(!c(rowname, measure, unit_measure, ref_area, time_period, obs_value, obs_status),
               values_to="dimension") %>%
  group_by(rowname) %>%
  mutate(
    dimension = case_when(
      all(dimension == "_T") ~ "_T",
      dimension == "_T" ~ NA,
      TRUE ~ dimension
    )
  ) %>%
  ungroup() %>%
  drop_na(dimension) %>%
  select(-name,-rowname) %>%
  distinct() %>%
  mutate(unit_measure = str_remove_all(unit_measure, "_SUB")) %>%
  filter(!measure %in% c(""))

oecd_avg_measures <- oecd_avg_dat %>% filter(dimension == "_T") %>% distinct(measure) %>%pull(measure)
oecd_avg_measures_gap <- oecd_avg_dat %>% filter(!dimension == "_T") %>% distinct(measure) %>%pull(measure)

