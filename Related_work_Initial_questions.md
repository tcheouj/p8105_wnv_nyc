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
- Are there specific boroughs in NYC that are disproportionately
  affected by West Nile virus?
- How has the NYC DOHMH’s mosquito control efforts during the summer
  affected the incidence of West Nile virus case detection?

We hypothesize that ***between the years of 2021 to 2024, the NYC
boroughs with warmer average temperatures will have a higher incidence
of mosquitoes carrying West Nile virus***. We also predict that areas
with higher HVI scores will also have more positive WNV mosquitoes as
these areas may be more likely to experience worse heat conditions and
potentially create more suitable environments for mosquitoes infected
with West Nile virus.

## Data

### Sources

We utilized the [Supplmentary Dataset of NYC Zip
Codes](https://p8105.com/data/zip_codes.html) that was provided on our
P8105 class website, which includes information on *NYC zip codes,
boroughs*, and *neighborhoods*.

We located the [NYC Heat Vulnerability Index
(HVI)](https://a816-dohbesp.nyc.gov/IndicatorPublic/data-explorer/climate/?id=2411#display=summary)
through the NYC Department of Health and Mental Hygiene (DOHMH)
Environment and Health Data Portal. The HVI shows the risk of
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
to create the NYC maps. All data is provided by the NYC DOHMH.

We located the [West Nile Virus Positive NYC
Cases](https://www.nyc.gov/assets/doh/downloads/pdf/wnv/2024/wnvplan2024.pdf)
through the *2024 NYC DOHMH Comprehensive Mosquito Surveillance and
Control Plan*. The report provides the number of West Nile virus cases
in NYC between 1999-2023, separated by borough.

The [Global Surface Summary of the Day (GSOD)
package](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00516)
contains global daily average weather observations from weather
stations. A `GSODR` package exists where we can load in data from
specific stations with specific weather measurements. Our indicators
included: mean temperature, dew point temperature, sea level pressure,
station pressure, visibility, wind speed, gust, precipitation amount,
and snow depth. This data is provided by the National Oceanic and
Atmospheric Administration.

### Cleaning

**P8105 Zip Codes Dataset**

The supplementary zip codes dataset was imported using `read_html` and
initially cleaned using `janitor::clean_names`. We filtered the dataset
to only include zip codes within NYC, used a mutate step to change
borough names to: *Brooklyn*, *Manhattan*, and *Staten Island*, and only
selected relevant variables: *zip_code*, *borough*, and *neighborhood*.

**Positive Mosquitoes Detected with West Nile Virus Datsets
(2021-2024)**

For the years 2021-2023, the datasets provided by the NYC DOHMH are
simple html tables that were imported using `read_html` and cleaned
using `janitor::clean_names`. Zip codes for each detection of positive
mosquitoes on a specific day are listed and separated by commas. To
clean this, we used multiple `separate_longer_delim` steps to separate
each positive mosquito detection into a separate observation, while
keeping the corresponding day.

For the year 2024, a .csv file was imported using `read_csv` and
reshaped using `pivot_longer` to consolidate all positive mosquito
detection dates into a single `date` column.

For all datasets, we reformatted the `zip_code` column to integers and
the `date` column to a date object, corrected any zip code and date
entry errors, and removed unnecessary columns and N/A values.

**NYC Positive Cases of West Nile Virus Dataset (1999-2023)**

Data was downloaded as a .csv file, imported using `read_csv`, and
initially cleaned using `janitor::clean_names`. The data was reshaped
using `pivot_longer` such that our columns are now: `year`, `borough`,
and `wnv_cases` (i.e. the number of West Nile virus cases counted within
that borough).

**NYC Heat Vulnerability Index Dataset**

This dataset was already in a tidy format after downloading. We simply
imported the dataset using `read_csv` and applied `janitor::clean_names`
for consistency.

**GSODR Dataset**

We identified Bushwick, Brooklyn as the geographical center of NYC. We
loaded the `GSODR` package and used the `nearest_stations` function to
identify weather stations surrounding the longitude and latitude
coordinates of Bushwick. We used a 20 mile radius to capture stations in
all boroughs. We then filtered out stations not in NYC and those without
2021-2024 weather data. This left us with only 5 stations: **Wall St**,
**LGA**, **Central Park**, **JFK**, and **The Battery**.

We pulled weather data between 2021-2024 from the 5 weather stations
using `get_GSOD`, applied `janitor::cleannames`, and selected these
variables: `station name`, `latitude` and `longitude`, date (`yearmoda`)
mean temperature (`temp`), mean precipitation (`prcp`), mean dew point
(`dewp`) (to get an idea of water content in the air), and `max` and
`min` temperature. Finally, we applied a series of mutate steps to
convert temperature measurements to Fahrenheit, precipitation to inches,
and renamed weather stations to more intuitive names.

### Merging

To integrate our datasets, we utilized the shared `zip_code` variable as
a key for merging. This allowed us to successfully combine WNV-positive
mosquito datasets (2021–2024), HVI scores, and MODZCTA shapefiles. For
the mosquito data, we ensured that zip codes without mosquito detection
were assigned a value of zero.

Relevant variables included in datasets:

- zip_code

- borough

- date

- neighborhood

- hvi

- name

- latitude

- longtitude

- yearmoda

- temp

- max

- min

- prcp

- dewp

- year

- 

## Exploratory Analysis

look at the distribution of wnv by borough

We first wanted to look at which weather station to use for our
temperature analysis as we were curious of whether there were any
variations between temperature data from these 5 stations. We plotted
the temperature data between 2021 and 2024 and we found that, despite
being in different boroughs, there is not much variation in measured
temperatures. From this, we understood that we could get by with using
just one station and chose weather data from **Central Park**.

We then wanted to focus on the mosquito data. Specifically, we wanted to
look at the number of mosquito counts by year and borough to explore
whether mosquito season starts earlier. Since we predicted that
temperatures are rising in NYC, then we should expect that warmer
climates may cause mosquito season to start sooner. To visualize this,
we created four separate plots of counts of mosquitoes positive for WNV
for each year from 2021 through 2024, separated by borough. (**The
figures show that - add conclusion here**). We also created an
additional plot just looking at (**ask Johnstone about what the plot
with the merged dataset is showing**).

To visualize this further, we wanted to create a map of NYC between
2021-2024 and overlay the positive WNV mosquitoes data to see if we
could visualize any trends on which boroughs or neighborhoods may be
affected more. Interestingly, from the figure, we found that there are
zip codes in Queens and Staten Island that appear to have an increase in
the number of mosquitoes positive for WNV from 2021 to 2024. Manhattan,
Brooklyn, and the Bronx remained relatively low.

We then wanted to look at whether heat vulnerability index is associated
with WNV+ mosquitoes. To do this, we used our merged dataset, grouped by
zipcode and borough, and then plotted hvi score and WNV+ mosquito
counts. **The results showed that there were no clear visual trends of
association between HVI score, WNV+ mosquitoes, and borough. One
noteworthy pattern was that neighborhoods with an HVI score of 5 had
generally lower counts of WNV+ mosquito counts compared to neighborhoods
with an HVI score of 1, which were more spread out.**

**Need to add poisson description**
