library(tidyverse)

dict <- readxl::read_excel("S:/Data/WDP/Well being database/Automated database/output/dictionary.xlsx")

material_headline <- c("1_1", "1_2", "1_3", "2_1", "2_2", "2_7", "3_1", "3_2")
quality_headline <- c("5_1", "5_3", "6_2", "6_1_DEP", "9_2", "9_3", "10_1", "10_2_DEP", "11_1", "11_2")
community_headline <- c("4_1", "4_3", "7_1_DEP", "7_2", "8_1_DEP", "8_2")

dat <- data.frame(measure = c(material_headline, quality_headline, community_headline)) %>%
  merge(dict %>% select(measure, label), by = "measure") %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into = "cat") %>%
  mutate(
    indic_label = case_when(
      cat == "1" ~ "income",
      cat == "2" ~ "jobs",
      cat == "3" ~ "housing",
      cat == "4" ~ "balance",
      cat == "5" ~ "health",
      cat == "6" ~ "education",
      cat == "7" ~ "community",
      cat == "8" ~ "civic",
      cat == "9" ~ "environment",
      cat == "10" ~ "safety",
      cat == "11" ~ "satisfaction"
    )
  ) %>%
  select(indic_label, label) %>%
  group_by(indic_label) %>%
  mutate(headline_indic = paste0(label, collapse = ", ")) %>%
  slice(1)

# Possible: https://raw.githubusercontent.com/wise-oecd/bli-test/refs/heads/main/info.tsv

read.table("./dictionary tidying/info_original.tsv", sep = "\t", header = T) %>%
  merge(dat, by = "indic_label") %>% 
  select(order, indic_label, indic_name, description, headline_indic) %>%
  arrange(order) %>%
  write.table(., file = "./dictionary tidying/info.tsv", quote = F, sep = "\t", col.names = NA) 
  
# Translation addition

translate_dat <- data.frame(measure = c(material_headline, quality_headline, community_headline)) %>%
  merge(dict %>% select(measure, label, label_fr), by = "measure") %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into = "cat") %>%
  mutate(
    indic_label = case_when(
      cat == "1" ~ "income",
      cat == "2" ~ "jobs",
      cat == "3" ~ "housing",
      cat == "4" ~ "balance",
      cat == "5" ~ "health",
      cat == "6" ~ "education",
      cat == "7" ~ "community",
      cat == "8" ~ "civic",
      cat == "9" ~ "environment",
      cat == "10" ~ "safety",
      cat == "11" ~ "satisfaction"
    )
  ) %>%
  select(indic_label, label, label_fr) %>%
  group_by(indic_label) %>%
  mutate(
    headline_indic = paste0(label, collapse = ", "),
    headline_indic_fr = paste0(label_fr, collapse = ", ")) %>%
  slice(1) %>%
  ungroup() %>%
  select(key = indic_label, en = headline_indic, fr=headline_indic_fr) %>%
  mutate(key = paste0(key, "_headline_indics"))


data.table::fread("https://raw.githubusercontent.com/wise-oecd/bli-test/refs/heads/main/translation.tsv") %>% 
  add_row(V1 = 298, 
          V2 = "metadata_note", 
          V3 = 'Learn more about the underlying indicators <a href="https://www.oecd.org/content/dam/oecd/en/topics/policy-sub-issues/measuring-well-being-and-progress/oecd-well-being-database-definitions.pdf">here.</a>', 
          V4 = 'Pour en savoir plus sur les indicateurs utilisés, cliquez <a href="https://www.oecd.org/content/dam/oecd/en/topics/policy-sub-issues/measuring-well-being-and-progress/oecd-well-being-database-definitions.pdf">ici.</a>') %>%
  # rbind(translate_dat) %>%
  write.table(., file = "./text tidying/translation.tsv", quote = F, sep = "\t", col.names = NA) 


data.table::fread("https://raw.githubusercontent.com/wise-oecd/bli-test/refs/heads/main/translation.tsv") %>% 
  select(-V1) %>%
  mutate(V2 = ifelse(V2 == "metadata-note", "metadata_note", V2)) %>%
  janitor::row_to_names(1) %>%
  write.table(., file = "./text tidying/translation.tsv", quote = F, sep = "\t", col.names = NA) 

  
data.table::fread("https://raw.githubusercontent.com/wise-oecd/bli-test/refs/heads/main/translation.tsv") %>% View()
  select(-V1) %>%
  slice(-nrow(.)) %>%
  add_row(V2 = "metadata_note", 
          V3 = 'Learn more about the underlying indicators', 
          V4 = 'Pour en savoir plus sur les indicateurs utilisés, cliquez ') %>%
  add_row(V2 = "metadata_note_click", 
          V3 = 'here.', 
          V4 = 'ici.') %>%
  janitor::row_to_names(1) %>%
  write.table(., file = "./text tidying/translation.tsv", quote = F, sep = "\t", col.names = NA) 


data.table::fread("https://raw.githubusercontent.com/wise-oecd/bli-test/refs/heads/main/translation.tsv") %>%
  select(-V1) %>%
  mutate(V4 = ifelse(V4 == "Pour en savoir plus sur les indicateurs utilisés, cliquez&nbsp;", "Pour en savoir plus sur les indicateurs utilisés, cliquez ", V4)) %>% 
  janitor::row_to_names(1) %>%
  write.table(., file = "./text tidying/translation.tsv", quote = F, sep = "\t", col.names = NA) 





