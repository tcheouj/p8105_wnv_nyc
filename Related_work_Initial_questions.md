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
- Are heat vulnerability index scores a good indicator for visualizing
  temperature trends in NYC?
- What is the incidence of West Nile virus in NYC over time? Has it
  changed?
- Is there a relationship between heat vulnerability index score and
  incidence of West Nile virus?
- Are there specific boroughs in NYC that are disproportionately
  affected by West Nile virus?
- How has the NYC DOHMH’s mosquito control efforts during the summer
  affected the incidence of West Nile virus case detection?

We hypothesize that ***between the years of 2021 to 2024, the NYC
regions with the highest heat vulnerability index score will have the
highest incidence of West Nile virus cases***. We expect these results
because areas with a high HVI may be more likely to experience greater
heat conditions and potentially create more suitable environments for
mosquitoes infected with West Nile virus.

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
virus in various NYC neighborhoods. This data is provided by the NYC
DOHMH.

We located the [West Nile Virus Positive NYC
Cases](https://www.nyc.gov/assets/doh/downloads/pdf/wnv/2024/wnvplan2024.pdf)
through the *2024 NYC DOHMH Comprehensive Mosquito Surveillance and
Control Plan*. The report provides the number of West Nile virus cases
in NYC between 1999-2023, separated by borough.

The [Global Surface Summary of the Day (GSOD)
package](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00516)
contains global daily average weather observations from weather
stations. Indicators include mean temperature, dew point temperature,
sea level pressure, station pressure, visibility, wind speed, gust,
precipitation amount, and snow depth. This data is provided by the
National Oceanic and Atmospheric Administration.

(should i include which stations we used for NY?)

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

## Exploratory Analysis
