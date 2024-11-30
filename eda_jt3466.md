eda_jt3466
================
Johnstone Tcheou
2024-11-25

    ## Rows: 193 Columns: 26
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (25): borough, detection_type, date1, date2, date3, date4, date5, date6,...
    ## dbl  (1): zip_code
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `date = lubridate::mdy(date)`.
    ## Caused by warning:
    ## !  2 failed to parse.

    ## Rows: 197 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): GeoType, Geography
    ## dbl (4): TimePeriod, GeoID, GeoRank, Score out of 5
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 25 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (6): Year, Bronx, Brooklyn, Manhattan, Queens, Staten Island
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 184 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (2): zcta20, hvi
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## # A tibble: 5 × 2
    ##     hvi count
    ##   <dbl> <int>
    ## 1     1   620
    ## 2     2   550
    ## 3     3   590
    ## 4     4   687
    ## 5     5   469

``` r
# get ZIP codes that did not have counts of WNV using anti_join with original ZIP code list (for generating proportions and to ensure ZIPs without cases can still be mapped later)


all_zctas <-
  zip_nbhd |> 
  pull(zip_code) |> 
  c(10000) |> 
  expand_grid(
    zip_code = _,
    year = c(2021, 2022, 2023, 2024),
    count = 0
  )

fill_neg_zctas <- function(yr) {
  
  pos = 
    wnv_mosquito_detection |> 
    mutate(
      year = year(date)
    ) |> 
    filter(year == yr) |> 
    group_by(zip_code, neighborhood, year) |> 
    summarize(
      count = n()
    ) |>
    arrange(count)
  
  neg = 
    all_zctas |> 
    filter(year == yr) |> 
    anti_join(pos, by = join_by(zip_code))
  
  rbind(pos, neg)
    
}

wnv_mosquito_data <-
  tibble(
    years = c(2021, 2022, 2023, 2024),
    mosquito_data = map(years, fill_neg_zctas)
  ) |> 
  unnest(mosquito_data) |> 
  select(-years) |> 
  left_join(zip_nbhd, by = join_by(zip_code, neighborhood)) # rejoin borough variable from original ZIP code list
```

    ## `summarise()` has grouped output by 'zip_code', 'neighborhood'. You can
    ## override using the `.groups` argument.
    ## `summarise()` has grouped output by 'zip_code', 'neighborhood'. You can
    ## override using the `.groups` argument.
    ## `summarise()` has grouped output by 'zip_code', 'neighborhood'. You can
    ## override using the `.groups` argument.
    ## `summarise()` has grouped output by 'zip_code', 'neighborhood'. You can
    ## override using the `.groups` argument.

``` r
wnv_mosquito_data |> 
  pull(count) |> 
  sum() # 2924
```

    ## [1] 2924

``` r
wnv_mosquito_detection |> 
  nrow() # 2924 - matches
```

    ## [1] 2924

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](eda_jt3466_files/figure-gfm/gsodr%20data%20import-1.png)<!-- -->

``` r
# Mosquito counts by year and borough - does mosquito season start earlier?

mosquitoes_2021_table |>
  group_by(borough, date) |> 
  summarize(
    count = n()
  ) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_grid(~borough)
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/cursory%20graphs-1.png)<!-- -->

``` r
mosquitoes_2022_table |>
  group_by(borough, date) |> 
  summarize(
    count = n()
  ) |>   
  ggplot(aes(x = date, y = count)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_grid(~borough)
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/cursory%20graphs-2.png)<!-- -->

``` r
mosquitoes_2023_table |>
  group_by(borough, date) |> 
  summarize(
    count = n()
  ) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_grid(~borough)
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/cursory%20graphs-3.png)<!-- -->

``` r
mosquitoes_2024_table |>
  group_by(borough, date) |> 
  summarize(
    count = n()
  ) |> 
  ggplot(aes(x = date, y = count))
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

![](eda_jt3466_files/figure-gfm/cursory%20graphs-4.png)<!-- -->

``` r
mosquitoes_2021_table |>
  group_by(borough, date) |> 
  summarize(
    count = n()
  ) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_col(stat = "identity") +
  facet_grid(~borough)
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

    ## Warning in geom_col(stat = "identity"): Ignoring unknown parameters: `stat`

![](eda_jt3466_files/figure-gfm/cursory%20graphs-5.png)<!-- -->

``` r
mosquitoes_2022_table |>
  group_by(borough, date) |> 
  summarize(
    count = n()
  ) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_col(stat = "identity") +
  facet_grid(~borough)
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

    ## Warning in geom_col(stat = "identity"): Ignoring unknown parameters: `stat`

![](eda_jt3466_files/figure-gfm/cursory%20graphs-6.png)<!-- -->

``` r
mosquitoes_2023_table |>
  group_by(borough, date) |> 
  summarize(
    count = n()
  ) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_col(stat = "identity") +
  facet_grid(~borough)
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

    ## Warning in geom_col(stat = "identity"): Ignoring unknown parameters: `stat`

![](eda_jt3466_files/figure-gfm/cursory%20graphs-7.png)<!-- -->

``` r
mosquitoes_2024_table |>
  group_by(borough, date) |> 
  summarize(
    count = n()
  ) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_col(stat = "identity") +
  facet_grid(~borough)
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

    ## Warning in geom_col(stat = "identity"): Ignoring unknown parameters: `stat`

![](eda_jt3466_files/figure-gfm/cursory%20graphs-8.png)<!-- -->

``` r
wnv_mosquito_detection |> 
  mutate(count = 1) |> 
  group_by(date) |> 
  summarize(
    count = sum(count)
  ) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point()
```

![](eda_jt3466_files/figure-gfm/cursory%20graphs-9.png)<!-- -->

``` r
# is heat vulnerability associated with WNV+ mosquitoes?

wnv_mosquito_detection |>
  mutate(
    month = month(date), 
    year = year(date)
  ) |> 
  group_by(zip_code, borough) |> 
  summarize(
    count = n()
  ) |> 
  left_join(hvi_zcta, by = join_by(zip_code)) |> 
  ggplot(aes(x = hvi, y = count, color = borough)) +
  geom_point()
```

    ## `summarise()` has grouped output by 'zip_code'. You can override using the
    ## `.groups` argument.

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](eda_jt3466_files/figure-gfm/cursory%20graphs-10.png)<!-- -->

``` r
# not really
```

    ## Reading layer `MODZCTA_2010' from data source 
    ##   `C:\Users\bschn\OneDrive\Documents\P8105 Data Science I\GitHub repos\p8105_wnv_nyc\data\shapefiles\MODZCTA_2010.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 178 features and 2 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 913176 ymin: 120122 xmax: 1067382 ymax: 272844
    ## Projected CRS: Lambert_Conformal_Conic

    ## Warning: There was 1 warning in `stopifnot()`.
    ## ℹ In argument: `zip_code = as.double(label)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

    ## Joining with `by = join_by(zip_code, neighborhood, year, count, borough)`

    ## # A tibble: 0 × 5
    ## # ℹ 5 variables: zip_code <dbl>, neighborhood <chr>, year <dbl>, count <dbl>,
    ## #   borough <chr>

``` r
mos_by_modzcta |> 
  filter(zip_code != modzcta) # no ZIP codes that were reallocated to a differently named MODZCTA, do not need to group by and sum as below 
```

    ## # A tibble: 0 × 8
    ## # ℹ 8 variables: zip_code <dbl>, neighborhood <chr>, year <dbl>, count <dbl>,
    ## #   borough <chr>, modzcta <dbl>, label <chr>,
    ## #   geometry <GEOMETRY [US_survey_foot]>

``` r
# cases_by_modzcta |> 
#   group_by(modzcta, year, geometry) |> 
#   summarize(
#     count = sum(count)
#   ) |> 
#   filter(year == 2021) |> 
#   ggplot(aes(fill = count, geometry = geometry)) +
#   geom_sf()

modzcta_2021 <-
  mos_by_modzcta |> 
  filter(year == 2021) |> 
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  labs(
    fill = "WNV+ mosquitoes",
    title = "2021"
  ) +
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  viridis::scale_fill_viridis(limits = c(0, 25)) # highest count is 23 in 2024, 10314

modzcta_2022 <-
  mos_by_modzcta |>
  filter(year == 2022) |>
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  labs(
    fill = "WNV+ mosquitoes",
    title = "2022"
  ) +
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  viridis::scale_fill_viridis(limits = c(0, 25))

modzcta_2023 <-
  mos_by_modzcta |>
  filter(year == 2023) |>
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  labs(
    fill = "WNV+ mosquitoes",
    title = "2023"
  ) +
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  viridis::scale_fill_viridis(limits = c(0, 25))

modzcta_2024 <-
  mos_by_modzcta |>
  filter(year == 2024) |>
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  labs(
    fill = "WNV+ mosquitoes",
    title = "2024"
  ) +
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  viridis::scale_fill_viridis(limits = c(0, 25))

modzcta_2021 + modzcta_2022 + modzcta_2023 + modzcta_2024 + 
  plot_annotation(
    title = "WNV positive mosquitoes by MODZCTA",
    caption = "Data from NYC DOHMH, shapefile from NYC Open Data"
  ) +
  plot_layout(guides = "collect") 
```

![](eda_jt3466_files/figure-gfm/map%20pos%20mosquito%20per%20modzcta-1.png)<!-- -->

``` r
# cases_by_modzcta |> 
#    filter(year == 2024) |> 
#    arrange(desc(count))
```

``` r
wnv_cases
```

    ## # A tibble: 125 × 3
    ##     year borough       wnv_cases
    ##    <dbl> <chr>             <dbl>
    ##  1  2023 bronx                 2
    ##  2  2023 brooklyn              8
    ##  3  2023 manhattan             5
    ##  4  2023 queens                9
    ##  5  2023 staten_island         8
    ##  6  2022 bronx                 1
    ##  7  2022 brooklyn              6
    ##  8  2022 manhattan             6
    ##  9  2022 queens               13
    ## 10  2022 staten_island        10
    ## # ℹ 115 more rows

``` r
weather_1999_2023 <-
  get_GSOD(
    years = c(1999:2023),
    station = c("725030-14732", "720553-99999", "744860-94789")
  ) |> 
  janitor::clean_names() |>
  select(name, latitude, longitude, yearmoda, temp, max, min, prcp, dewp) |>  # retain relevant variables - station name, latitude and longitude, date (yearmoda) mean temperature (temp), mean precipitation (prcp), mean dew point (dewp) to get an idea of water content in the air, max temperature, and min temperature
  mutate(
    temp = (temp * 9/5) + 32, # convert from Celsius to Fahrenheit
    max = (max * 9/5) + 32,
    min = (min * 9/5) + 32,
    prcp = (prcp * 0.0393701), # convert from mm to in
    year = year(yearmoda) # give stations more intuitive names
  ) |> 
  rename(date = yearmoda)
```

    ## Warning: 
    ## This station, 720553-99999, only provides data for years 2016 to 2024.
    ## Please send a request that falls within these years.

``` r
# 725030-14732 - LGA
# 725053-94728 - Central Park - only data from 2005-2024
# 744860-94789 - JFK 
# 720553-99999 - Port Authority Downtown Helipad - only data from 2016-2024


yearly_weather <-
  weather_1999_2023 |> 
  mutate(
    year = year(date)
  ) |> 
  group_by(year) |> 
  summarize(
    avg_temp = mean(temp),
    avg_min = mean(min),
    avg_max = mean(max), 
    avg_prcp = mean(prcp),
    avg_dewp = mean(dewp)
  )

wnv_cases <-
  wnv_cases |> 
  mutate(
    borough = case_match(
      borough, 
      "manhattan" ~ "Manhattan",
      "brooklyn" ~ "Brooklyn",
      "queens" ~ "Queens",
      "bronx" ~ "Bronx",
      "staten_island" ~ "Staten Island"
    )
  )

avg_hvi_borough <-
  left_join(hvi_zcta, zip_nbhd, by = join_by(zip_code)) |> 
    group_by(borough) |> 
    summarize(
      avg_hvi = mean(hvi)
    )

# is heat vulnerability associated with WNV+ human cases?

left_join(wnv_cases, yearly_weather, by = join_by(year)) |> 
  left_join(avg_hvi_borough, by = join_by(borough)) |> 
  ggplot(aes(x = avg_hvi, y = wnv_cases, fill = year, shape = borough)) +
  geom_point() + 
  stat_summary(fun.y = mean, color = "red")  # perhaps hvi is a parabolic distribution?
```

    ## Warning: The `fun.y` argument of `stat_summary()` is deprecated as of ggplot2 3.3.0.
    ## ℹ Please use the `fun` argument instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Removed 5 rows containing missing values or values outside the scale range
    ## (`geom_segment()`).

![](eda_jt3466_files/figure-gfm/case%20data-1.png)<!-- -->

``` r
left_join(wnv_cases, yearly_weather, by = join_by(year)) |> 
  left_join(avg_hvi_borough, by = join_by(borough)) |> 
  ggplot(aes(x = avg_temp, y = wnv_cases)) + 
  geom_point() +
  geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/poisson%20assumptions-1.png)<!-- -->

``` r
weather_2021_2024 <- 
  weather_2021_2024 |> 
  filter(name == "Central Park") |> 
  rename(date = yearmoda)

wnv_hvi_temp <-
  left_join(wnv_mosquito_detection, hvi_zcta, by = join_by(zip_code)) |> 
  left_join(weather_2021_2024, by = join_by(date)) 

test <- glm(count ~ year + borough, 
            family = poisson, 
            data = wnv_mosquito_data)

test |> 
  broom::tidy()
```

    ## # A tibble: 6 × 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           71.0      33.4         2.13 3.34e-  2
    ## 2 year                  -0.0343    0.0165     -2.08 3.78e-  2
    ## 3 boroughBrooklyn       -0.203     0.0608     -3.35 8.20e-  4
    ## 4 boroughManhattan      -2.23      0.0765    -29.1  1.75e-186
    ## 5 boroughQueens         -0.153     0.0551     -2.78 5.39e-  3
    ## 6 boroughStaten Island   0.507     0.0645      7.86 3.94e- 15
