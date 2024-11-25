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

This station, 999999-14732, only provides data for years 1948 to 1972.
Please send a request that falls within these years. 
2: 
  This station, 725033-94728, only provides data for years 1943 to 1997.
Please send a request that falls within these years. 
3: 
  This station, 725060-94728, only provides data for years 2010 to 2012.
Please send a request that falls within these years. 
4: 
  This station, 999999-94728, only provides data for years 1965 to 1997.
Please send a request that falls within these years. 
5: 
  This station, 744976-99999, only provides data for years 1975 to 1996.
Please send a request that falls within these years. 
6: 
  This station, 999999-14786, only provides data for years 1945 to 1970.
Please send a request that falls within these years. 
7: 
  This station, 999999-94789, only provides data for years 1948 to 1972.
Please send a request that falls within these years. 
8: 
  This station, 725026-99999, only provides data for years 1975 to 1979.
Please send a request that falls within these years. 
9: 
  This station, 997439-99999, only provides data for years 2008 to 2008.
Please send a request that falls within these years. 

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
