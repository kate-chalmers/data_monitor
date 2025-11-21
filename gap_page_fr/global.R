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

oecd_names <- c("Australie", "Autriche", "Belgique", "Canada", "Chili", "Colombie", "Costa Rica", "Tchéquie", "Danemark",
                "Estonie", "Finlande", "France", "Allemagne", "Grèce", "Hongrie", "Islande", "Irlande", "Israël", "Italie", "Japon",
                "Corée", "Lettonie", "Lituanie", "Luxembourg", "Mexique", "Pays-Bas", "Nouvelle-Zélande", "Norvège", "Pologne",
                "Portugal", "Slovaquie", "Slovénie", "Espagne", "Suède", "Suisse", "Turquie", "Royaume-Uni",
                "États-Unis")

article_en <- c("Australie", "Autriche", "Belgique", "Colombie", "Tchéquie", "Estonie",
                "Finlande", "France", "Allemagne", "Grèce", "Hongrie", "Islande",
                "Irlande", "Israël", "Italie", "Corée", "Lettonie", "Lituanie",
                "Norvège", "Pologne", "Slovaquie", "Slovénie", "Espagne",
                "Suède", "Suisse", "Turquie", "Nouvelle-Zélande", "Argentine", "Croatie",
                "Roumanie", "Indonésie", "Thaïlande", "Afrique du Sud", "Brésil", "Pérou", "Bulgarie")

article_au <- c("Canada", "Chili", "Costa Rica", "Danemark", "Japon", "Luxembourg",
                "Mexique", "Portugal", "Royaume-Uni", "Brésil", "Pérou")

article_aux <- c("Pays-Bas", "États-Unis")


partner_countries <- c("BRA", "ARG", "BGR", "HRV", "PER", "ROU", "IDN", "THA", "ZAF")

partner_country_vector <- c("Brésil"  = "BRA",
                            "Argentine"= "ARG",
                            "Bulgarie"= "BGR",
                            "Croatie" = "HRV",
                            "Pérou" = "PER",
                            "Roumanie" = "ROU",
                            "Indonésie" = "IDN",
                            "Thaïlande" = "THA",
                            "Afrique du Sud" = "ZAF")


country_name_vector <- setNames(c("OECD", oecd_countries), c("OCDE", oecd_names))
country_name_vector <- c(country_name_vector, partner_country_vector)

average_vector <- country_name_vector[1]
oecd_vector <- country_name_vector[2:(length(oecd_countries) +1)]
accession_vector <- country_name_vector[(length(oecd_countries) + 2):length(country_name_vector)]

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

clusters <- readRDS("./data/clusters_df.RDS")

mat_cluster <- clusters %>% filter(cluster == "mats") %>% pull(measure)
qualts_cluster <- clusters %>% filter(cluster == "qualts") %>% pull(measure)
coms_cluster <- clusters %>% filter(cluster == "coms") %>% pull(measure)
nature_cluster <- clusters %>% filter(cluster == "nat") %>% pull(measure)
human_cluster <- clusters %>% filter(cluster == "human") %>% pull(measure)
social_cluster <- clusters %>% filter(cluster == "social") %>% pull(measure)
econ_cluster <- clusters %>% filter(cluster == "econ") %>% pull(measure)

full_dat <- readRDS("./data/full inequalities data.RDS") %>%
  arrange(ref_area, measure, dimension, time_period) %>%
  mutate(time_period = as.numeric(time_period)) %>%
  select(-dimension_long, -dimension_tidy, -image_caption, -gap_value) %>%
  rename(dimension_long = dimension_long_fr, dimension_tidy = dimension_tidy_fr, image_caption = image_caption_fr,
         gap_value = gap_value_fr)

latest_idx <- full_dat |>
  group_by(ref_area, measure, dimension) |>
  filter(time_period == max(time_period) | is.na(obs_value)) |>
  mutate(time_period = ifelse(gap_value == "No Data", NA, time_period)) |>
  ungroup() %>%
  select(-label, -unit) %>%
  rename(label = label_fr, unit = unit_fr) %>%
  mutate(value_tidy = gsub("No data", "Absence de données", value_tidy))


latest_tot <- readRDS("./data/latest total values.RDS") %>%
  select(-value_tidy) %>%
  rename(value_tidy = value_tidy_fr)

inequalitySeriesPlotter <- inequalitySeriesPlotter_fun
gapPlotter              <- gapPlotter_fun
lollipopPlotter         <- lollipopPlotter_fun

