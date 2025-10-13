# remotes::install_github("ebailey78/shinyBS")
library(shiny)
library(tidyverse)
library(echarts4r)
library(shinyjs)

# Non-essential
# library(bslib)
library(bsicons)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)

# Memory boosting
# library(memoise)
# library(cachem)
source("./utils.R")

last_updated <- file.info("./data/full inequalities data.RDS") %>% pull(mtime) %>% format(., "%d %B, %Y")


oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST",
                    "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN",
                    "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT",
                    "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

oecd_names <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czechia", "Denmark",
                "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan",
                "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Türkiye", "United Kingdom",
                "United States")

the_thes <- c("United States", "Netherlands", "Slovak Republic", "United Kingdom", "OECD average")

partner_countries <- c("BRA", "ARG", "BGR", "HRV", "PER", "ROU", "IDN", "THA", "ZAF")

partner_country_names <- countrycode(partner_countries, "iso3c", "country.name")

country_name_vector <- setNames(c("OECD", oecd_countries, partner_countries), c("OECD Average", oecd_names, partner_country_names))

average_vector <- country_name_vector[1]
oecd_vector <- country_name_vector[2:(length(oecd_countries) +1)]
accession_vector <- country_name_vector[(length(oecd_countries) + 2):length(country_name_vector)]


clusters <- readxl::read_excel("./data/dictionary.xlsx") %>%
  select(measure) %>%
  mutate(measure2 = measure) %>%
  separate(measure2, into = c("cat")) %>%
  mutate(
    cluster = case_when(
      cat %in% c("1", "2", "3") ~ "mats",
      cat %in% c("5", "6", "9", "10", "11") ~ "qualts",
      cat %in% c("4", "7", "8", "14") ~ "coms",
      cat == "12" ~ "nat",
      cat == "13" ~ "human",
      # cat == "14" ~ "social",
      cat == "15" ~ "econ"
    ),
    cat = as.numeric(cat)
  ) %>%
  arrange(cat, measure)

dim_colors <- data.frame(
  cat = as.character(1:15),
  color = c("rgb(37,160,219, 0.2)",   # #25a0db
            "rgb(35,129,196, 0.2)",   # #2381c4
            "rgb(49,171,156, 0.2)",   # #31ab9c
            "rgb(148,47,41, 0.2)",    # #942f29
            "rgb(124,64,126, 0.2)",   # #7c407e
            "rgb(122,177,83, 0.2)",   # #7ab153
            "rgb(223,86,104, 0.2)",   # #df5668
            "rgb(217,167,44, 0.2)",   # #d9a72c
            "rgb(20,178,104, 0.2)",   # #14b268
            "rgb(96,97,101, 0.2)",    # #606165
            "rgb(233,107,59, 0.2)",   # #e96b3b
            "rgb(29,23,82, 0.2)",     # #1d1752
            "rgb(29,23,82, 0.2)",     # #1d1752
            "rgb(29,23,82, 0.2)",     # #1d1752
            "rgb(29,23,82, 0.2)")     # #1d1752
)

mat_cluster <- clusters %>% filter(cluster == "mats") %>% pull(measure)
qualts_cluster <- clusters %>% filter(cluster == "qualts") %>% pull(measure)
coms_cluster <- clusters %>% filter(cluster == "coms") %>% pull(measure)
nature_cluster <- clusters %>% filter(cluster == "nat") %>% pull(measure)
human_cluster <- clusters %>% filter(cluster == "human") %>% pull(measure)
social_cluster <- clusters %>% filter(cluster == "social") %>% pull(measure)
econ_cluster <- clusters %>% filter(cluster == "econ") %>% pull(measure)

full_dat <- readRDS("./data/full inequalities data.RDS") %>%
  filter(!grepl("11_3_", measure),
         !measure %in% c("10_2", "11_1_DEP", "14_3_DEP", "14_7_DEP", "7_1_DEP",
                         "4_4_DEP", "2_9_DEP", "7_3_DEP",
                         "8_1", "4_1", "7_2", "2_6", "2_5", "5_2_DEP", "4_2", "4_3", "8_2")) %>%
  arrange(ref_area, measure, dimension, time_period) %>%
  mutate(time_period = as.numeric(time_period))

latest_idx <- full_dat |>
  group_by(ref_area, measure, dimension) |>
  filter(time_period == max(time_period) | is.na(obs_value)) |>
  mutate(time_period = ifelse(gap_value == "No Data", NA, time_period)) |>
  ungroup()

latest_tot <- readRDS("./data/latest total values.RDS")

inequalitySeriesPlotter <- inequalitySeriesPlotter_fun
gapPlotter              <- gapPlotter_fun
lollipopPlotter         <- lollipopPlotter_fun

