## Sean Sorek 11/30/24

wnv_cases <- read_csv("cleaned_data/wnv_cases.csv")
wnv_mosquitoes <- read_csv("cleaned_data/wnv_mosquitoes.csv")
heat_vuln <- read_csv("cleaned_data/heat_vuln.csv")
day_weather <- read_csv("cleaned_data/day_weather.csv")
## Most basic model: mosquitoes by year

mosq_count_by_year <- wnv_mosquitoes |>
  mutate(year = lubridate::year(date)) |>
  group_by(year) |>
  summarise(count = n()) |>
  filter(year < 3000)

fit1 <- glm(count ~ year, mosq_count_by_year, family = "poisson")
summary(fit1)


# About as uninformative as youd expect

## mosquitoes by month

mosq_count_by_month <- wnv_mosquitoes |>
  mutate(month = lubridate::month(date)) |>
  group_by(month) |>
  summarise(count = n())

fit2 <- glm(count ~ month, mosq_count_by_month, family = "poisson")
summary(fit2)

fit3 <- glm(count ~ month + I(month^2), mosq_count_by_month, family = "poisson")
summary(fit3)
plot(mosq_count_by_month)
lines(6:10, predict(fit3, type = "response"))

## mosquitoes by month and year

mosq_month_year <- wnv_mosquitoes |>
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) |>
  group_by(year, month) |>
  summarise(count = n()) |>
  filter(year < 3000)
  
# does year make a difference in prediction between 2021-2024?
fit3.5 <- glm(count ~ month + I(month^2), mosq_month_year, family = "poisson")
fit4 <- glm(count ~ month + I(month^2) + year, mosq_month_year, family = "poisson")
summary(fit3.5)
summary(fit4)

anova(fit3.5, fit4)



## mosquito cases by weekly temperature

mosq_cases_temp <-
  wnv_mosquitoes |>
  inner_join(y = day_weather, by = join_by(date)) |>
  mutate(
    year = lubridate::year(date),
    week = lubridate::week(date)) |>
  group_by(year, week) |>
  summarise(avg_temp = mean(avg_temp),
            avg_prcp = mean(avg_prcp),
            avg_dewp = mean(avg_dewp),
            avg_max = mean(avg_max),
            avg_min = mean(avg_min),
            count = n()) |>
  mutate(weekyear = (year - 2021)*52 + week) #week since start of 2021

null_week_year <- glm(count ~ week + I(week^2) + year, mosq_cases_temp, family = poisson)
summary(null_week_year)
plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(null_week_year, type = "response"))
plot(mosq_cases_temp$count, resid(null_week_month)) # null model works well


### Temperature

temp_fit <- glm(count ~ avg_temp, mosq_cases_temp, family = poisson)
summary(temp_fit)

plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(temp_fit, type = "response"))


### Temperature and Precipitation
temp_rain_fit <- glm(count ~ avg_temp + avg_prcp, mosq_cases_temp, family = poisson)
summary(temp_rain_fit)

plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(temp_rain_fit, type = "response"))

### Temp, Precip, Humidity

all_weather_fit <- glm(count ~ avg_temp + avg_prcp + avg_dewp, mosq_cases_temp, family = poisson)
summary(all_weather_fit)

plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(all_weather_fit, type = "response"))

### comparing max, avg, min temperature as a predictor

max_fit <- glm(count ~ avg_max, mosq_cases_temp, family = poisson)
min_fit <- glm(count ~ avg_min, mosq_cases_temp, family = poisson)

summary(max_fit)
summary(temp_fit)
summary(min_fit)

plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(max_fit, type = "response"), col = "red")
lines(mosq_cases_temp$weekyear, predict(temp_fit, type = "response"))
lines(mosq_cases_temp$weekyear, predict(min_fit, type = "response"), col = "blue")

### just precipitation

rain_fit <- glm(count ~ avg_prcp, mosq_cases_temp, family = poisson)
summary(rain_fit)

plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(rain_fit, type = "response"))


## Best model: min temps + rain

min_rain <- glm(count ~ avg_min + avg_prcp, mosq_cases_temp, family = poisson)
summary(min_rain)

plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(min_rain, type = "response"))
lines(mosq_cases_temp$weekyear, predict(null_week_year, type = "response"), col = "red")

# We do not perform very well, this model is missing something

## Hypothesis: This is an autoregressive model, 
# the number spotted per week depends on the number spotted the previous week

ar_min_rain <- glm(count ~ avg_min + avg_prcp + lag(count), mosq_cases_temp, family = poisson)
summary(ar_min_rain)

plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear[-1], predict(ar_min_rain, type = "response"))
lines(mosq_cases_temp$weekyear, predict(null_week_year, type = "response"), col = "red")

## OH YEAAAAAH! LETS GOOOO
## This isn't quite right because I added a lag in the laziest possible way, but this model is much better

### Adding a correct lag

lag_mosq_cases <- mosq_cases_temp |>
  group_by(year) |>
  arrange(year, week) |>
  mutate(lag_count = lag(count)) |>
  na.omit()

lag_null <- glm(count ~ week + I(week^2) + year, lag_mosq_cases, family = poisson)
ar_min_rain_2 <- glm(count ~ avg_min + avg_prcp + lag_count, lag_mosq_cases, family = poisson)
summary(ar_min_rain_2)

plot(count ~ weekyear, mosq_cases_temp)
lines(lag_mosq_cases$weekyear, predict(ar_min_rain_2, type = "response"))
lines(lag_mosq_cases$weekyear, predict(lag_null, type = "response"), col = "red")

## This is not the best model in the entire world, and the plots need to be cleaned up
# But this is good enough to answer our research questions so far I think