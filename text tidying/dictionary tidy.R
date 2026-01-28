library(tidyverse)

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
    # Reading
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
    # Math
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
    # Science
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
