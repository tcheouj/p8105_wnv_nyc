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

    ## Reading layer `tl_2021_us_zcta520' from data source 
    ##   `C:\Users\bschn\OneDrive\Documents\P8105 Data Science I\GitHub repos\p8105_wnv_nyc\data\shapefiles\tl_2021_us_zcta520.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 33791 features and 9 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -176.6967 ymin: -14.37378 xmax: 145.8305 ymax: 71.34132
    ## Geodetic CRS:  NAD83

    ## Joining with `by = join_by(zip_code, year, count, borough, neighborhood)`

    ## # A tibble: 0 × 5
    ## # ℹ 5 variables: zip_code <dbl>, year <dbl>, count <dbl>, borough <chr>,
    ## #   neighborhood <chr>

``` r
cases_by_zcta_shp |> 
  filter(year == 2021) |> 
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  viridis::scale_fill_viridis(limits = c(0, 25)) # highest count is 23 in 2024, 10314
```

![](eda_jt3466_files/figure-gfm/maps%20of%20counts%20by%20zcta-1.png)<!-- -->

``` r
cases_by_zcta_shp |> 
  filter(year == 2022) |> 
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  viridis::scale_fill_viridis(limits = c(0, 25))
```

![](eda_jt3466_files/figure-gfm/maps%20of%20counts%20by%20zcta-2.png)<!-- -->

``` r
cases_by_zcta_shp |> 
  filter(year == 2023) |> 
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  viridis::scale_fill_viridis(limits = c(0, 25))
```

![](eda_jt3466_files/figure-gfm/maps%20of%20counts%20by%20zcta-3.png)<!-- -->

``` r
cases_by_zcta_shp |> 
  filter(year == 2024) |> 
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() +
  viridis::scale_fill_viridis(limits = c(0, 25)) 
```

![](eda_jt3466_files/figure-gfm/maps%20of%20counts%20by%20zcta-4.png)<!-- -->

``` r
cases_by_zcta_shp |> 
   filter(year == 2024) |> 
   arrange(desc(count))
```

    ## # A tibble: 299 × 15
    ##    zip_code  year count borough neighborhood zcta5ce20 geoid20 classfp20 mtfcc20
    ##       <dbl> <dbl> <dbl> <chr>   <chr>        <chr>     <chr>   <chr>     <chr>  
    ##  1    10314  2024    23 Staten… Mid-Island   10314     10314   B5        G6350  
    ##  2    10465  2024    13 Bronx   Southeast B… 10465     10465   B5        G6350  
    ##  3    10301  2024    10 Staten… Stapleton a… 10301     10301   B5        G6350  
    ##  4    10305  2024    10 Staten… Stapleton a… 10305     10305   B5        G6350  
    ##  5    10306  2024    10 Staten… South Shore  10306     10306   B5        G6350  
    ##  6    10307  2024    10 Staten… South Shore  10307     10307   B5        G6350  
    ##  7    10309  2024    10 Staten… South Shore  10309     10309   B5        G6350  
    ##  8    10310  2024    10 Staten… Port Richmo… 10310     10310   B5        G6350  
    ##  9    10312  2024    10 Staten… South Shore  10312     10312   B5        G6350  
    ## 10    10471  2024    10 Bronx   Kingsbridge… 10471     10471   B5        G6350  
    ## # ℹ 289 more rows
    ## # ℹ 6 more variables: funcstat20 <chr>, aland20 <dbl>, awater20 <dbl>,
    ## #   intptlat20 <chr>, intptlon20 <chr>, geometry <MULTIPOLYGON [°]>
