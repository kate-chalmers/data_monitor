# remotes::install_github("ebailey78/shinyBS")
library(shiny)
library(tidyverse)
library(echarts4r)
library(shinyjs)

# Non-essential
library(shinyBS)
library(shinycssloaders)
source("./utils.R")

last_updated <- file.info("./data/final dataset.RDS") %>% pull(mtime) %>% format(., "%d %B, %Y")

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

material_headline <- c("1_1", "1_2", "1_3", "2_1", "2_2", "2_7", "3_1", "3_2")
quality_headline <- c("5_1", "5_3", "6_2", "6_1_DEP", "9_2", "9_3", "10_1", "10_2_DEP", "11_1", "11_2")
community_headline <- c("4_1", "4_3", "7_1_DEP", "7_2", "8_1_DEP", "8_2")
nature_headline <- c("12_7", "12_8", "12_10")
human_headline <- c("13_1", "13_2", "13_3")
social_headline <- c("14_1", "14_3", "14_5")
econ_headline <- c("15_1", "15_6", "15_7")

headline_indicators <- c(material_headline, quality_headline, community_headline,
                         nature_headline, human_headline, social_headline, econ_headline)

gap_filler <- readRDS("./data/gap filler.RDS")
avg_vals_full <- readRDS("./data/latest point data.RDS") %>% 
  select(-label_name, -unit_tag, -image_caption) %>% 
  rename(label_name = label_name_fr, unit_tag = unit_tag_fr, image_caption = image_caption_fr)
ts_vals_full <- readRDS("./data/time series.RDS")

