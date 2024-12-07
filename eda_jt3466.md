exploratory_data_analysis
================

    ## Rows: 193 Columns: 26
    ## ── Column specification ──────────────────────────────────────────────
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
    ## ── Column specification ──────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): GeoType, Geography
    ## dbl (4): TimePeriod, GeoID, GeoRank, Score out of 5
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 25 Columns: 6
    ## ── Column specification ──────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (6): Year, Bronx, Brooklyn, Manhattan, Queens, Staten Island
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 184 Columns: 2
    ## ── Column specification ──────────────────────────────────────────────
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
  left_join(zip_nbhd, by = join_by(zip_code, neighborhood)) |> # rejoin borough variable from original ZIP code list 
  mutate(
    borough = case_when(
      zip_code == 10000 ~ "Manhattan",
      .default = borough
    ),
    neighborhood = case_when(
      zip_code == 10000 ~ "Central Park",
      .default = neighborhood
    )
  )
```

    ## `summarise()` has grouped output by 'zip_code', 'neighborhood'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'zip_code', 'neighborhood'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'zip_code', 'neighborhood'. You
    ## can override using the `.groups` argument.
    ## `summarise()` has grouped output by 'zip_code', 'neighborhood'. You
    ## can override using the `.groups` argument.

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

    ## Rows: 25 Columns: 6
    ## ── Column specification ──────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (6): year, avg_temp, avg_min, avg_max, avg_prcp, avg_dewp
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 9131 Columns: 6
    ## ── Column specification ──────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (5): avg_temp, avg_min, avg_max, avg_prcp, avg_dewp
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

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

    ## `summarise()` has grouped output by 'borough'. You can override using
    ## the `.groups` argument.
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

    ## `summarise()` has grouped output by 'borough'. You can override using
    ## the `.groups` argument.
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

    ## `summarise()` has grouped output by 'borough'. You can override using
    ## the `.groups` argument.
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/cursory%20graphs-3.png)<!-- -->

``` r
mosquitoes_2024_table |>
  group_by(borough, date) |> 
  summarize(
    count = n()
  ) |> 
  ggplot(aes(x = date, y = count)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_grid(~borough)
```

    ## `summarise()` has grouped output by 'borough'. You can override using
    ## the `.groups` argument.
    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

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

    ## `summarise()` has grouped output by 'borough'. You can override using
    ## the `.groups` argument.

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

    ## `summarise()` has grouped output by 'borough'. You can override using
    ## the `.groups` argument.

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

    ## `summarise()` has grouped output by 'borough'. You can override using
    ## the `.groups` argument.

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

    ## `summarise()` has grouped output by 'borough'. You can override using
    ## the `.groups` argument.

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

    ## Reading layer `MODZCTA_2010' from data source 
    ##   `/Users/mahdi/Desktop/p8105_final_project/data/shapefiles/MODZCTA_2010.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 178 features and 2 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 913176 ymin: 120122 xmax: 1067382 ymax: 272844
    ## Projected CRS: Lambert_Conformal_Conic

    ## Reading layer `nybb' from data source 
    ##   `/Users/mahdi/Desktop/p8105_final_project/data/shapefiles/nybb.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 5 features and 4 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 913175.1 ymin: 120128.4 xmax: 1067383 ymax: 272844.3
    ## Projected CRS: NAD83 / New York Long Island (ftUS)

![](eda_jt3466_files/figure-gfm/geocoding%20human%20case%20data-1.png)<!-- -->

``` r
mos_by_modzcta |>
  ggplot(aes(fill = count, geometry = geometry)) +
  geom_sf() + 
  labs(
    title = "WNV positive mosquitoes by MODZCTA",
    caption = "Data and shapefile from DOHMH"
  ) +
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  viridis::scale_fill_viridis(limits = c(0, 25)) +
  facet_wrap(~year)
```

![](eda_jt3466_files/figure-gfm/map%20pos%20mosquito%20per%20modzcta-1.png)<!-- -->

``` r
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

    ## Warning: Removed 5 rows containing missing values or values outside the scale
    ## range (`geom_segment()`).

![](eda_jt3466_files/figure-gfm/case%20data-1.png)<!-- -->

``` r
merged_df <- 
  wnv_mosquito_data |> 
  filter(!is.na(borough)) |>  # there are (likely nonresidential) ZIP codes with no boroughs and no detected WNV in mosquitoes and humans
  group_by(year, borough) |> 
  summarize(
    wnv_mosquitoes = sum(count) 
  ) |> 
  right_join(wnv_cases, by = join_by(year, borough)) |> 
  left_join(yearly_weather, by = join_by(year)) |> 
  left_join(avg_hvi_borough, by = join_by(borough)) |>  
  select(year, borough, starts_with("wnv_"), everything()) |> 
  arrange(year, borough)
```

    ## `summarise()` has grouped output by 'year'. You can override using
    ## the `.groups` argument.

``` r
merged_df |> 
  ggplot(aes(x = wnv_cases)) +
  geom_histogram() +
  facet_grid(~borough) # right tailed
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](eda_jt3466_files/figure-gfm/poisson%20assumptions-1.png)<!-- -->

``` r
temp_quantiles <-
  merged_df |> 
  pull(avg_temp) |> 
  quantile(probs = c(0.25, 0.50, 0.75))

temp_cat <-
  merged_df |> 
  mutate(
    temp_quantile = case_when(
      avg_temp < temp_quantiles[1] ~ 1,
      avg_temp < temp_quantiles[2] ~ 2,
      avg_temp < temp_quantiles[3] ~ 3,
      avg_temp > temp_quantiles[3] ~ 4,
      .default = NA
    )
  ) # right tailed


temp_cat |> 
  ggplot(aes(x = wnv_cases)) +
  geom_histogram() +
  facet_grid(~temp_quantile) 
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](eda_jt3466_files/figure-gfm/poisson%20assumptions-2.png)<!-- -->

``` r
# check assumption that mean and variance are approximately equal for each group of x (temperature)
# graphically 

merged_df |> 
  ggplot(aes(x = avg_temp, y = wnv_cases)) + 
  geom_point() +
  geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/poisson%20assumptions-3.png)<!-- -->

``` r
merged_df |> 
  ggplot(aes(x = avg_temp, y = wnv_cases, color = borough)) + 
  geom_point() +
  geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/poisson%20assumptions-4.png)<!-- -->

``` r
merged_df |> 
  ggplot(aes(x = avg_temp, y = wnv_cases, color =)) + 
  geom_point() +
  geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](eda_jt3466_files/figure-gfm/poisson%20assumptions-5.png)<!-- -->

``` r
merged_df |> 
  pull(wnv_cases) |> 
  max() # highest number of human WNV cases is 34 - which happened in 1999, when WNV first reached NYC
```

    ## [1] 34

``` r
merged_df |> 
  filter(wnv_cases == 34)
```

    ## # A tibble: 1 × 10
    ## # Groups:   year [1]
    ##    year borough wnv_mosquitoes wnv_cases avg_temp avg_min avg_max avg_prcp
    ##   <dbl> <chr>            <dbl>     <dbl>    <dbl>   <dbl>   <dbl>    <dbl>
    ## 1  1999 Queens              NA        34     55.9    48.5    64.5    0.116
    ## # ℹ 2 more variables: avg_dewp <dbl>, avg_hvi <dbl>

``` r
# tabular check 
merged_df |> 
  group_by(avg_temp) |> 
  summarize(
    avg_count = mean(wnv_cases), 
    variance = var(wnv_cases)
  ) |> 
  arrange(desc(avg_count))
```

    ## # A tibble: 25 × 3
    ##    avg_temp avg_count variance
    ##       <dbl>     <dbl>    <dbl>
    ##  1     55.9       9.4    201. 
    ##  2     56.7       8.4     11.3
    ##  3     57.5       8.2     10.2
    ##  4     56.1       7.6     46.3
    ##  5     55.6       7.2     41.7
    ##  6     56.1       7.2     20.7
    ##  7     53.2       6.4     10.3
    ##  8     57.2       6.4      8.3
    ##  9     55.9       5.8     20.2
    ## 10     56.6       4.2     18.2
    ## # ℹ 15 more rows

``` r
# borough vs count
merged_df |> 
  group_by(borough) |> 
  summarize(
    avg_count = mean(wnv_cases), 
    variance = var(wnv_cases)
  ) |> 
  arrange(desc(avg_count)) 
```

    ## # A tibble: 5 × 3
    ##   borough       avg_count variance
    ##   <chr>             <dbl>    <dbl>
    ## 1 Queens             7.96    55.4 
    ## 2 Brooklyn           4.4     11.8 
    ## 3 Staten Island      3.6     10.8 
    ## 4 Manhattan          2.6      5.5 
    ## 5 Bronx              2.4      7.58

``` r
# test if log of wnv count is linearly related to predictors

merged_df |> 
  mutate(
    log_cases = log(wnv_cases)
  ) |> 
  ggplot(aes(x = avg_temp, y = log_cases)) +
  geom_point() +
  geom_smooth( se = FALSE) # not really
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

![](eda_jt3466_files/figure-gfm/poisson%20assumptions-6.png)<!-- -->

``` r
merged_df |> 
  mutate(
    log_cases = log(wnv_cases)
  ) |> 
  ggplot(aes(x = avg_temp, y = log_cases, color = borough)) +
  geom_point() +
  geom_smooth( se = FALSE) # not really
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 17 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

![](eda_jt3466_files/figure-gfm/poisson%20assumptions-7.png)<!-- -->
