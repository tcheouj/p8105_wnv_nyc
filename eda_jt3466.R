# Johnstone Tcheou - exploratory data analysis

library(rnoaa)
library(GSODR)

wnv_mosquito_detection |> 
  pull(neighborhood) |> 
  length()
  
wnv_mosquito_detection |> 
  pull(zip_code) |> 
  length()

wnv_mosquito_detection |> 
  group_by(neighborhood) |>
  summarize(
    count = n()
  ) |> 
  arrange(desc(count))

# May need to use ZCTA level HVI data instead of NTA level HVI 
# - ZCTA is designed to approximate ZIP codes, when our WNV counts are by ZIP code

hvi_zcta <- read_csv("https://data.cityofnewyork.us/resource/4mhf-duep.csv")  

hvi_wnv_zip <-
  inner_join(wnv_mosquito_detection, hvi_zcta, by = join_by(zip_code == zcta20))

hvi_wnv_zip |> 
  group_by(hvi) |> 
  summarize(
    count = n()
  )

# Bushwick is the geographical center of NYC, providing the following latitude and longitude coordinates 
# Wakefield at the top of the Bronx is around 33 km/20.5 mi away from Bushwick, creating the radius of stations to search for
# Filter for non NYC stations, by filtering out NJ and filtering out Kings Point and Sandy Hook and other Long Island locations

stations <-
  nearest_stations(
    LAT = 40.6958, 
    LON = -73.9171,
    distance = 33
  ) |> 
  filter(STATE != "NJ" & !(NAME %in% c("KINGS POINT", 
                                       "SANDY HOOK", 
                                       "BERGEN POINT", 
                                       "MITCHEL FIELD", 
                                       "HEMPSTEAD MITCHELL FLD AFB", 
                                       "AMBROSE LIGHT  NY", 
                                       "AMBROSE / FT. TILDEN", 
                                       "FREEPORT")))


# Some stations only have historical data preceding our years of interest of 2021-2024, these have been filtered out

stations <- 
  stations |> 
  filter(!(STNID %in% c("999999-14732",
                        "725033-94728",
                        "725060-94728",
                        "999999-94728",
                        "744976-99999",
                        "999999-14786",
                        "999999-94789",
                        "725026-99999",
                        "997439-99999")))
weather_df <- 
  get_GSOD(
    years = c(2021:2024),
    station = stations$STNID
  )

# Mosquito counts by year and borough - does mosquito season start earlier?
grouping_counts <- function(x) {
  df <-
    x |> 
    group_by(borough, date) |> 
    summarize(
      count = n()
    )
  
  df
}


borough_counts <-
  tibble(
    counts = list(mosquitoes_2021_table, mosquitoes_2022_table, mosquitoes_2023_table)
  ) |> 
  mutate(
    borough_counts = map(counts, grouping_counts)
  ) |>
  select(borough_counts) |> 
  unnest(cols = c(borough_counts)) |> 
  mutate(
    month = month(date),
    year = year(date)
  ) |> 
  select(borough, month, year, date, count)

borough_counts |>
  filter(year == 2021) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_grid(~borough)

borough_counts |>
  filter(year == 2022) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_grid(~borough)

borough_counts |>
  filter(year == 2023) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_grid(~borough)

borough_counts |> 
  filter(year == 2021) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_bar(stat = "identity") +
  facet_grid(~borough)

borough_counts |> 
  filter(year == 2022) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_bar(stat = "identity") +
  facet_grid(~borough)

borough_counts |> 
  filter(year == 2023) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_bar(stat = "identity") +
  facet_grid(~borough)
