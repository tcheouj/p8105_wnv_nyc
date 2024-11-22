# Sean Sorek 11/22/2024

library(tidyverse)
library(rvest)
library(httr)

######## WNV+ Mosquito data by year, date, borough, neighborhood, zip, etc

## 2021 - 2023 are simple HTML tables
mosquitoes_2024_url <- "https://www.nyc.gov/site/doh/health/health-topics/west-nile-virus-activity-2024.page"
mosquitoes_2023_url <- "https://www.nyc.gov/site/doh/health/health-topics/west-nile-virus-activity-2023.page"
mosquitoes_2022_url <- "https://www.nyc.gov/site/doh/health/health-topics/west-nile-virus-activity-2022.page"
mosquitoes_2021_url <- "https://www.nyc.gov/site/doh/health/health-topics/west-nile-virus-activity-2021.page"

mosquitoes_2024_html <- read_html(mosquitoes_2024_url)
mosquitoes_2023_html <- read_html(mosquitoes_2023_url)
mosquitoes_2022_html <- read_html(mosquitoes_2022_url)
mosquitoes_2021_html <- read_html(mosquitoes_2021_url)

mosquitoes_2023_table <- html_table(mosquitoes_2023_html)[[1]]
mosquitoes_2022_table <- html_table(mosquitoes_2022_html)[[1]]
mosquitoes_2021_table <- html_table(mosquitoes_2021_html)[[1]]

## 2024 is hosted on a 3rd party and is obnoxious to deal with
## the obtained dataset was so messy I didnt bother trying to get R to even read it
## if you want to see what im dealing with: https://datawrapper.dwcdn.net/j2B1K/2/dataset.csv
## After getting it in a readable state (minus the neighborhoods)

mosquitoes_2024_table <- read_csv("data/mosquitoes_2024.csv")
