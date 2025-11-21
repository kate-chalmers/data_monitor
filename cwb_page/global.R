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

oecd_names <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czechia", "Denmark", 
                "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan",
                "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", "Poland",
                "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Türkiye", "United Kingdom",
                "United States") 

the_thes <- c("United States", "Netherlands", "Slovak Republic", "United Kingdom", "OECD average")

partner_countries <- c("BRA", "ARG", "BGR", "HRV", "PER", "ROU", "IDN", "THA", "ZAF")

partner_country_vector <- c("Brazil"  = "BRA", 
                            "Argentina"= "ARG", 
                            "Bulgaria"= "BGR", 
                            "Croatia" = "HRV", 
                            "Peru" = "PER", 
                            "Romania" = "ROU", 
                            "Indonesia" = "IDN", 
                            "Thailand" = "THA", 
                            "South Africa" = "ZAF")


country_name_vector <- setNames(c("OECD", oecd_countries), c("OECD Average", oecd_names))
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
avg_vals_full <- readRDS("./data/latest point data.RDS") 
ts_vals_full <- readRDS("./data/time series.RDS")

