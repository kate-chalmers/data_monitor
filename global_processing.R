source("./utils_processing.R")
library(tidyverse)
# library(tidymodels)

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
                    "5_2_DEP", "10_2", "11_3_DEP", "7_1", "8_1")

dict <- readxl::read_excel("S:/Data/WDP/Well being database/Automated database/output/dictionary.xlsx") %>%
  mutate(
    label = case_when(
      is.na(label) & !is.na(indic) ~ indic,
      TRUE ~ label
    ),
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


