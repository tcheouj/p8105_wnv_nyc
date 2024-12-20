Final Project Sections (MM)
================
Mahdi Maktabi
2024-11-29

## Related Work

Our group has a shared interest in addressing climate change, studying
infectious diseases, and uniting over our mutual hatred of mosquito
bites. Inspiration for this project stems from several relevant news
articles and papers:

- A [2024 New York Times
  article](https://www.nytimes.com/article/west-nile-virus-nyc.html)
  highlights the rising concern for an increased number of mosquitoes
  infected with West Nile virus over the summer in New York City and how
  this likely driven by climate change.
- A [2020 Nature
  article](https://www.nature.com/articles/s41590-020-0648-y) finds that
  climate change can affect mosquito abundance, survival, transmission
  dynamics, and pathogen development within vectors.

This issue is concerning as worsening climate change trends and warmer
temperatures in NYC can create more hospitable environments for
mosquitoes carrying West Nile virus and may increase human-mosquito
interaction. Motivated by these concerns, we aimed to explore the rising
temperatures trends in NYC, West Nile virus incidence in NYC, and what
is being done to control its spread.

## Initial Questions

With this inspiration, we wanted to explore available NYC data sets to
answer some of these initial questions:

- What communities are affected most by rising temperatures? (heat
  vulnerability index)
- What do average temperatures look like over time in NYC? Is there any
  shift?
  - What about precipitation trends?
- If climate change is causing temperatures to rise in NYC, does this
  affect mosquito season?
- What is the incidence of mosquitoes carrying West Nile virus in NYC
  over time? Is it different across boroughs?
- Is there a relationship between heat vulnerability index score and
  incidence of West Nile virus?
- Are the number of West Nile virus human cases increasing over time in
  NYC?
  - If so, are there specific boroughs in NYC that are
    disproportionately affected by West Nile virus?
- How has the NYC DOHMH’s mosquito control efforts during the summer
  affected the incidence of West Nile virus case detection?

We hypothesize that ***between the years of 2021 to 2024, the NYC
boroughs with warmer average temperatures and greater amounts of
precipitation will have a higher incidence of mosquitoes carrying West
Nile virus***. We also predict that areas with higher HVI scores will
also have more positive WNV mosquitoes as these areas may be more likely
to experience worse heat conditions and potentially create more suitable
environments for mosquitoes infected with West Nile virus.

## Data

### Sources

We utilized the [Supplmentary Dataset of NYC Zip
Codes](https://p8105.com/data/zip_codes.html) that was provided on our
P8105 class website, which includes information on *NYC zip codes,
boroughs*, and *neighborhoods*.

We located the [NYC Heat Vulnerability Index
(HVI)](https://a816-dohbesp.nyc.gov/IndicatorPublic/data-explorer/climate/?id=2411#display=summary)
through the NYC DOHMH Environment and Health Data Portal. We also
utilized [Zip Code Tabulated Area Level
HVI](https://catalog.data.gov/dataset/heat-vulnerability-index-rankings)
data, which is designed to approximate zip codes and works well with
merging HVI scores with our other datasets. The HVI shows the risk of
community-level health impacts due to extreme heat and includes factors
such as surface temperatures, green spaces, access to home air
conditioning, median income, and the percentage of residents who are
low-income or non-Latinx Black (who are often excluded from heat
resources). Neighborhoods with a **HVI score of 1** have the lowest risk
while those with a **HVI score of 5** have the highest risk.

Data from the [West Nile Virus Mosquito Activity
reports](https://www.nyc.gov/site/doh/health/health-topics/west-nile-virus-activity.page)
was used to show the detection of positive mosquitoes for West Nile
virus in various NYC neighborhoods. Further, we used geographic files
for [Modified ZIP Code Tabulation Areas
(MODZCTA)](https://github.com/nychealth/coronavirus-data/tree/master/Geography-resources)
to create the NYC maps. All data is provided by the NYC DOHMH. More
information on how this mosquito surveillance is conducted by the NYC
DOHMH can be found
[here](https://www.nyc.gov/assets/doh/downloads/pdf/wnv/2023/wnvplan2023.pdf).

We located the [West Nile Virus NYC Human
Cases](https://www.nyc.gov/assets/doh/downloads/pdf/wnv/2024/wnvplan2024.pdf)
through the *2024 NYC DOHMH Comprehensive Mosquito Surveillance and
Control Plan*. The report provides the number of West Nile virus human
cases in NYC between 1999-2023, separated by borough.

The [Global Surface Summary of the Day
(GSOD)](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00516)
contains global daily average weather observations from weather
stations. A `GSODR` package exists where we can load in data from
specific stations and select various weather measurements. Our
indicators include: mean temperature, dew point temperature, sea level
pressure, station pressure, visibility, wind speed, gust, precipitation
amount, and snow depth. This data is provided by the National Oceanic
and Atmospheric Administration.

### Cleaning

**P8105 Zip Codes Dataset**

The supplementary zip codes dataset was imported using `read_html` and
initially cleaned using `janitor::clean_names`. We filtered the dataset
to only include zip codes within NYC, used a mutate step to change
borough names to: *Brooklyn*, *Manhattan*, and *Staten Island*, and only
selected relevant variables: *zip_code*, *borough*, and *neighborhood*.

**Positive Mosquitoes Detected with West Nile Virus in NYC Datsets
(2021-2024)**

For the years 2021-2023, the datasets provided by the NYC DOHMH are
simple html tables that were imported using `read_html` and cleaned
using `janitor::clean_names`. Zip codes for each detection of
WNV-positive mosquitoes on a specific day are listed and separated by
commas. To clean this, we used multiple `separate_longer_delim` steps to
separate each positive mosquito detection into a separate observation,
while keeping the corresponding detection date.

For the year 2024, a .csv file was imported using `read_csv` and
reshaped using `pivot_longer` to consolidate all WNV-positive mosquito
detection dates into a single `date` column.

For all datasets, we reformatted the `zip_code` column to integers and
the `date` column to a date object (yyyy-mm-dd), corrected any zip code
and date entry errors, and removed unnecessary columns and N/A values.
As a result, each row now represents a single WNV-positive mosquito
detected in a specific zip code/borough/neighborhood on a specific date.

**NYC Positive Cases of West Nile Virus Dataset (1999-2023)**

Data was downloaded as a .csv file, imported using `read_csv`, and
initially cleaned using `janitor::clean_names`. The data was reshaped
using `pivot_longer` such that our columns are now: `year`, `borough`,
and `wnv_cases` (i.e. the number of West Nile virus cases counted within
that borough).

**NYC Heat Vulnerability Index Dataset**

This dataset was already in a tidy format after downloading (thank you
NYC DOHMH Environment and Health!!). We simply imported the dataset
using `read_csv` and applied `janitor::clean_names` for consistency. Our
columns include: `zip_codes` and `hvi` scores.

**GSODR Dataset**

Through online research, we identified Bushwick, Brooklyn as the
geographical center of NYC. We loaded the `GSODR` package and used the
`nearest_stations` function to identify weather stations surrounding the
longitude and latitude coordinates of Bushwick. We used a 20-mile radius
to capture stations across all boroughs. We then filtered out stations
not in NYC and those without 2021-2024 weather data. This left us with
only 5 stations: **Wall St**, **LGA**, **Central Park**, **JFK**, and
**The Battery**.

We pulled weather data between 2021-2024 from the 5 weather stations
using `get_GSOD`, applied `janitor::cleannames`, and selected these
variables: `station name`, `latitude` and `longitude`, date (`yearmoda`)
mean temperature (`temp`), mean precipitation (`prcp`), mean dew point
(`dewp`) (to get an idea of water content in the air), and `max` and
`min` temperature. Finally, we applied a series of mutate steps to
convert temperature measurements to Fahrenheit, precipitation measures
to inches, and renamed weather stations to more intuitive names.

### Merging

To combine our datasets, we utilized the shared `zip_code` variable as a
key for merging. This allowed us to successfully combine WNV-positive
mosquito datasets (2021–2024), HVI scores, and MODZCTA shape files. For
the mosquito data, we ensured that zip codes without mosquito detection
were assigned a value of zero.

Our relevant variables for our datasets include:

**WNV+ Mosquito Data**

- `zip_code`: The zip code within NYC in which the WNV+ mosquitoes were
  detected
- `borough`: The NYC borough (Brooklyn, Manhattan, Bronx, Staten Island,
  or Queens) corresponding to the zip code location.
- `date`: The date in which the WNV+ mosquito was detected within a
  specific zip code.
- `count`: The number of WNV+ mosquitoes detected within a specific zip
  code.
- `neighborhood`: The name of the NYC neighborhood in which the WNV+
  mosquito was detected.
- `hvi`: The HVI score (ranging from 1-5)

**GSODR**

- `name`: The name of the NYC weather station
- `latitude`: Latitude coordinate of the weather station
- `longtitude`: Longitude coordinate of the weather station
- `yearmoda`: year of the weather station data that was pulled
- `temp`: mean temperature (Fahrenheit)
- `max`: maximum temperature measured (Fahrenheit)
- `min`: minimum temperature measured (Fahrenheit)
- `prcp`: mean precipitation (inches)
- `dewp`: mean dew point (Fahrenheit)

Our datasets are now ready for analysis!

## Exploratory Analysis

### Comparing Temperature Data Across Weather Stations

We first wanted to look at which weather station to use for our
temperature analysis as we were curious of whether there were any
variations between temperature data from these 5 stations. We plotted
the temperature data between 2021 and 2024.

We found that, despite being in different boroughs, there is not much
variation in measured temperatures. Since they were all fairly similar,
we ended up taking the average of the 5 weather stations.

### Exploring Trends in WNV-Positive Mosquito Counts

We then wanted to focus on the mosquito data. Specifically, we wanted to
look at the number of mosquito counts by year and borough to explore
whether mosquito season starts earlier. Since we predicted that
temperatures are rising in NYC, then we should expect that warmer
climates may cause mosquito season to start sooner. To visualize this,
we created four separate plots of counts of mosquitoes positive for WNV
for each year from 2021 through 2024, separated by borough.

The figures show that there are drastic differences in the counts of
WNV-positive mosquitoes between boroughs. Queens consistently had the
highest counts of WNV-positive mosquitoes compared to the other boroughs
across all years, while Manhattan had the lowest counts of WNV-positive
mosquitoes. Further, when boroughs are grouped together, we see that the
number of WNV-positive mosquitoes peaked in 2021, followed by a decline
in 2022 and 2023. There is a slight increase in the counts peak in the
year 2024.

We can also see from these figures that between the years of 2021-2024,
mosquito season typically ranges from June to October, which is still
within a normal range of months for mosquito season in NYC.
Unfortunately, the data we have access to only provides detections of
WNV+ mosquitoes within NYC starting from 2021 so we won’t be able to
analyze long-term trends.

### Mapping WNV-Positive Mosquito Trends Across NYC

To visualize this further, we wanted to create a map of NYC between
2021-2024 and overlay the WNV positive mosquitoes data to see if we
could visualize any trends on which boroughs or neighborhoods may be
affected more. Interestingly, from the figure, we found that there are
zip codes in Queens and Staten Island that appear to have an increase in
the number of mosquitoes positive for WNV from 2021 to 2024. Manhattan,
Brooklyn, and the Bronx remained relatively low.

### Examining the Relationship Between Heat Vulnerability and WNV-Positive Mosquitoes

We then wanted to look at whether heat vulnerability index is associated
with WNV+ mosquitoes. To do this, we used our merged dataset, grouped by
zip code and borough, and then plotted hvi score and WNV+ mosquito
counts.

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

    ## Warning: Removed 125 rows containing non-finite outside the scale range
    ## (`stat_summary()`).

    ## Warning in max(f): no non-missing arguments to max; returning -Inf

    ## Warning: Computation failed in `stat_summary()`.
    ## Caused by error in `seq_len()`:
    ## ! argument must be coercible to non-negative integer

    ## Warning: Removed 125 rows containing missing values or values outside the
    ## scale range (`geom_point()`).

![](Report_sections_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

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

The results showed that there were no clear visual trends of association
between HVI score, WNV+ mosquitoes, and borough. One noteworthy pattern
was that neighborhoods with an HVI score of 5 had generally lower counts
of WNV+ mosquito counts compared to neighborhoods with an HVI score of
1, which were more spread out.

## Discussion

For poisson model - Talk about how the precepitation was linked to
increasing WNV+ mosquitoes (Ex: for every one inch of percepitation,
there is 0.367 positive mosquitoes, p-value was significant). -
