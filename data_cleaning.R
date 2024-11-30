# Sean Sorek 11/22/2024

library(tidyverse)
library(rvest)
library(httr)

######## WNV+ Mosquito data by year, date, borough, neighborhood, zip, etc

# neighborhood reference, so we can just index by zip code and add neighborhoods later
zip_nbhd <- read_html("https://p8105.com/data/zip_codes.html") |>
  html_table() |>
  first() |>
  janitor::clean_names() |>
  filter(!(county == "New York" & zip_code %in% c(10463, 11201))) |>#duplicated zip codes, both are *not* in New York County
  mutate(borough = str_replace_all(county, c(
    "Kings" = "Brooklyn",
    "New York" = "Manhattan",
    "Richmond" = "Staten Island"
  ))) |>
  select(zip_code, neighborhood)


## 2021 - 2023 are simple HTML tables
mosquitoes_2024_url <- "https://www.nyc.gov/site/doh/health/health-topics/west-nile-virus-activity-2024.page"
mosquitoes_2023_url <- "https://www.nyc.gov/site/doh/health/health-topics/west-nile-virus-activity-2023.page"
mosquitoes_2022_url <- "https://www.nyc.gov/site/doh/health/health-topics/west-nile-virus-activity-2022.page"
mosquitoes_2021_url <- "https://www.nyc.gov/site/doh/health/health-topics/west-nile-virus-activity-2021.page"

## 2024 is hosted on a 3rd party and is obnoxious to deal with
## the obtained dataset was so messy I didnt bother trying to get R to even read it
## if you want to see what im dealing with: https://datawrapper.dwcdn.net/j2B1K/2/dataset.csv
## After getting it in a readable state (minus the neighborhoods)

### 2024
mosquitoes_2024_table <- read_csv("data/mosquitoes_2024.csv") |>
  pivot_longer(cols = starts_with("date"), values_to = "date") |>
  select(-name, -detection_type) |>
  mutate(date = lubridate::mdy(date)) |>
  na.omit()

### 2023
mosquitoes_2023_table <- mosquitoes_2023_url |>
  read_html() |> html_table() |> first() |>
  janitor::clean_names() |>
  separate_longer_delim(zip_code, delim = ", ") |>
  separate_longer_delim(zip_code, delim = ",\n") |>
  separate_longer_delim(zip_code, delim = ",") |> #this data is disgusting
  select(-neighborhood, -download) |>
  mutate(
    zip_code = as.integer(zip_code),
    date = str_c(date, ", ", 2023),
    date = lubridate::mdy(date)
  )

### 2022
mosquitoes_2022_table <- mosquitoes_2022_url |>
  read_html() |> html_table() |> first() |>
  janitor::clean_names() |>
  separate_longer_delim(zip_code, delim = ", ") |>
  separate_longer_delim(zip_code, delim = ",\n") |>
  separate_longer_delim(zip_code, delim = ",") |> #this data is disgusting
  select(-download) |>
  mutate(
    zip_code = as.integer(zip_code),
    date = str_c(date, ", ", 2022),
    date = lubridate::mdy(date)
  )

### 2021
mosquitoes_2021_table <- mosquitoes_2021_url |>
  read_html() |> html_table() |> first() |>
  janitor::clean_names() |>
  separate_longer_delim(zip_code, delim = ", ") |>
  separate_longer_delim(zip_code, delim = ",\n") |>
  separate_longer_delim(zip_code, delim = ",") |> #this data is disgusting
  select(-download) |>
  mutate(
    zip_code = as.integer(zip_code),
    date = str_c(date, ", ", 2021),
    date = lubridate::mdy(date)
  )

wnv_mosquito_detection <- bind_rows(
  mosquitoes_2024_table,
  mosquitoes_2023_table,
  mosquitoes_2022_table,
  mosquitoes_2021_table,
) |> left_join(y = zip_nbhd, by = join_by(zip_code)) |>
  write_csv("cleaned_data/wnv_mosquitoes.csv")

######### Heat Vulnerability by Neighborhood
heat_vuln <- read_csv("data/heat_vulnerability.csv") |>
  janitor::clean_names() |> #Already clean! Thank you NYC environment and health
  write_csv("cleaned_data/heat_vuln.csv")

######### West Nile Virus Cases by Year and Borough
wnv_cases <- read_csv("data/wnv_cases.csv") |>
  janitor::clean_names() |>
  pivot_longer(cols = c(
    bronx,
    brooklyn,
    manhattan,
    queens,
    staten_island
  ),
  names_to = "borough",
  values_to = "wnv_cases") |>
  write_csv("cleaned_data/wnv_cases.csv")
