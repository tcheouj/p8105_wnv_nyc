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
  c(10000) 

wnv_pos_zctas <-
  wnv_mosquito_detection |> 
  pull(zip_code) |> 
  unique()

wnv_neg_zctas <-
  zip_nbhd |> 
  filter(!(zip_code %in% wnv_pos_zctas)) # summing negative and positive ZCTAs gets 131 instead of 130 from the ZIP list as 10000 not in the ZIP list

pos_cases_by_zcta <- 
  wnv_mosquito_detection |> 
  mutate(
    year = year(date)
  ) |> 
  group_by(zip_code, year) |> 
  summarize(
    count = n()
  ) |> 
  arrange(zip_code, year) 
```

    ## `summarise()` has grouped output by 'zip_code'. You can override using the
    ## `.groups` argument.

``` r
neg_cases_by_zcta <-
  wnv_neg_zctas |> 
  pull(zip_code) |> 
  expand_grid(
    zip_code = _,
    year = c(2021, 2022, 2023, 2024)
  ) |> 
  mutate(
    count = 0
  )

cases_by_zcta <- 
  rbind(neg_cases_by_zcta, pos_cases_by_zcta) |> 
  arrange(zip_code, year) |> 
  left_join(zip_nbhd, by = join_by(zip_code)) |>  # bring borough back in after getting counts grouped by zip
  mutate( # catch the 10000 ZIP code
    borough = case_when(
      zip_code == 10000 ~ "Manhattan",
      .default = borough
    ),
    neighborhood = case_when(
      zip_code == 10000 ~ "Central Park",
      .default = neighborhood
    )
  )


### check this - if i can get zctas without cases for each year to appear then i can calculate correct proportions and fix the maps
prptn_wnv_borough <- # calculate each borough's proportion of positive ZIPs out of total ZIPs in that borough
  cases_by_zcta |> 
  mutate(
    detected = case_when(
      count > 0 ~ 1,
      count == 0 ~ 0, 
      .default = NA
    )
  ) |> 
  group_by(borough, year) |> 
  summarize(
    total = n(),
    positive = sum(detected)
  ) |> 
  mutate(prtpn_pos = positive/total)
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

``` r
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
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

``` r
borough_counts |>
  filter(year == 2021) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_grid(~borough)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/cursory%20graphs-1.png)<!-- -->

``` r
borough_counts |>
  filter(year == 2022) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_grid(~borough)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/cursory%20graphs-2.png)<!-- -->

``` r
borough_counts |>
  filter(year == 2023) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_grid(~borough)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/cursory%20graphs-3.png)<!-- -->

``` r
borough_counts |> 
  filter(year == 2021) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_col(stat = "identity") +
  facet_grid(~borough)
```

    ## Warning in geom_col(stat = "identity"): Ignoring unknown parameters: `stat`

![](eda_jt3466_files/figure-gfm/cursory%20graphs-4.png)<!-- -->

``` r
borough_counts |> 
  filter(year == 2022) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_bar(stat = "identity") +
  facet_grid(~borough)
```

![](eda_jt3466_files/figure-gfm/cursory%20graphs-5.png)<!-- -->

``` r
borough_counts |> 
  filter(year == 2023) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_bar(stat = "identity") +
  facet_grid(~borough)
```

![](eda_jt3466_files/figure-gfm/cursory%20graphs-6.png)<!-- -->

``` r
borough_counts |> 
  filter(year == 2021) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_col(stat = "identity") +
  facet_grid(~borough)
```

    ## Warning in geom_col(stat = "identity"): Ignoring unknown parameters: `stat`

![](eda_jt3466_files/figure-gfm/cursory%20graphs-7.png)<!-- -->

``` r
borough_counts |> 
  filter(year == 2022) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_bar(stat = "identity") +
  facet_grid(~borough)
```

![](eda_jt3466_files/figure-gfm/cursory%20graphs-8.png)<!-- -->

``` r
borough_counts |> 
  filter(year == 2023) |> 
  ggplot(aes(x = date, y = count, fill = borough)) +
  geom_col(width = 2.0) +
  facet_grid(~borough)
```

![](eda_jt3466_files/figure-gfm/cursory%20graphs-9.png)<!-- -->

``` r
# is there much variation between the different weather stations?

weather_df |>
#  filter(name %in% c("Central Park", "Wall St")) |> 
  ggplot(aes(x = yearmoda, y = temp, color = name)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(se = FALSE, color = "white", alpha = 0.5) +
  theme(legend.position = "bottom") + 
  geom_errorbar(aes(ymin = min, ymax = max), alpha = 0.5)  
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](eda_jt3466_files/figure-gfm/cursory%20graphs-10.png)<!-- -->

``` r
# despite being different boroughs, measured temperatures between stations are largely coincident - can get by with just one like Central Park
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

    ## Joining with `by = join_by(zip_code, year, count, borough, neighborhood)`

    ## # A tibble: 0 × 5
    ## # ℹ 5 variables: zip_code <dbl>, year <dbl>, count <dbl>, borough <chr>,
    ## #   neighborhood <chr>

``` r
cases_by_modzcta |> 
  filter(zip_code != modzcta) # no ZIP codes that were reallocated to a differently named MODZCTA, do not need to group by and sum as below 
```

    ## # A tibble: 0 × 8
    ## # ℹ 8 variables: zip_code <dbl>, year <dbl>, count <dbl>, borough <chr>,
    ## #   neighborhood <chr>, modzcta <dbl>, label <chr>,
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
  cases_by_modzcta |> 
  filter(year == 2021) |> 
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  labs(
    fill = "WNV cases",
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
  cases_by_modzcta |>
  filter(year == 2022) |>
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  labs(
    fill = "WNV cases",
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
  cases_by_modzcta |>
  filter(year == 2023) |>
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  labs(
    fill = "WNV cases",
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
  cases_by_modzcta |>
  filter(year == 2024) |>
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  labs(
    fill = "WNV cases",
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
    title = "NYC WNV cases by MODZCTA",
    caption = "Data from NYC DOHMH, shapefile from NYC Open Data"
  ) +
  plot_layout(guides = "collect") 
```

![](eda_jt3466_files/figure-gfm/map%20counts%20per%20modzcta-1.png)<!-- -->

``` r
# cases_by_modzcta |> 
#    filter(year == 2024) |> 
#    arrange(desc(count))
```
