source("./utils_processing.R")
library(tidyverse)
library(tidymodels)

oecd_countries <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", 
                    "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", 
                    "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", 
                    "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

partner_countries <- c("BRA", "ARG", "BGR", "HRV", "PER", "ROU", "IDN", "THA", "ZAF")


all_countries <- c(oecd_countries, partner_countries)


