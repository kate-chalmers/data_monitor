library(wiser)
library(tidyverse)

# dat <- readxl::read_excel("S:/Data/WDP/Well being database/Data Monitor/data_monitor/hows_life_dictionary_french.xlsx")
# 
# agency_identifier <- "OECD.WISE.WDP"
# dataflow_identifier <- "DSD_HSL@DF_HSL_CWB"
# 
# # Obtain labeled data
# df <- get_labeled_dataset(
#   agency_identifier = agency_identifier,
#   dataflow_identifier = dataflow_identifier
# )
# 
# cwb_indics <- df %>%
#   janitor::clean_names() %>%
#   select(measure, measure_label_fr) %>%
#   distinct()
# 
# agency_identifier <- "OECD.WISE.WDP"
# dataflow_identifier <- "DSD_HSL@DF_HSL_FWB"
# 
# # Obtain labeled data
# df <- get_labeled_dataset(
#   agency_identifier = agency_identifier,
#   dataflow_identifier = dataflow_identifier
# )
# 
# fwb_indics <- df %>%
#   janitor::clean_names() %>%
#   select(measure, measure_label_fr) %>%
#   distinct()
# 
# dict <- rbind(cwb_indics, fwb_indics) %>%
#   rename(indicator = measure_label_fr) 
# 
# dict <- dat %>%
#   merge(dict, all = T) %>% 
#   mutate(indicator = ifelse(!is.na(label), label, indicator)) 
# 
# openxlsx::write.xlsx(dict, "S:/Data/WDP/Well being database/Data Monitor/data_monitor/hows_life_dictionary_french.xlsx")

dat <- readxl::read_excel("S:/Data/WDP/Well being database/Data Monitor/data_monitor/hows_life_dictionary_french.xlsx")

dict_full <- readxl::read_excel("S:/Data/WDP/Well being database/Automated database/output/dictionary.xlsx")

dict_full %>%
  merge(dat %>% select(measure, label_fr = label, unit_fr = unit, indicator_fr = indicator), by = "measure", all = T) %>%
  openxlsx::write.xlsx("S:/Data/WDP/Well being database/Automated database/output/dictionary.xlsx")
