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
day_weather

mosq_cases <-
  wnv_mosquitoes |>
  mutate(
    year = lubridate::year(date),
    week = lubridate::week(date)) |>
  group_by(year, week) |>
  summarise(count = n()) |>
  mutate(weekyear = (year - 2021)*52 + week) |>
  filter(year < 3000)
  
weekly_weather <- day_weather |>
  mutate(
    year = lubridate::year(date),
    week = lubridate::week(date)) |>
  group_by(year, week) |>
  summarise(avg_temp = mean(avg_temp),
            avg_prcp = mean(avg_prcp),
            avg_dewp = mean(avg_dewp),
            avg_max = mean(avg_max),
            avg_min = mean(avg_min)) |>
  arrange(year, week) |>
  mutate(lag_temp = lag(avg_temp),
         lag_prcp = lag(avg_prcp),
         lag_dewp = lag(avg_dewp),
         lag_max =  lag(avg_max),
         lag_min =  lag(avg_min)) |>
  mutate(weekyear = (year - 2021)*52 + week) #week since start of 2021

mosq_cases_temp <- mosq_cases |>
  left_join(y = weekly_weather, by = join_by(weekyear)) |>
  select(-ends_with(".y")) |>
  rename(year = year.x, week = week.x) |>
  mutate(
    lag_count = lag(count),
    lag_count = case_when(
      is.na(lag_count) ~ 0,
      .default = lag_count
    )
  ) |>
  na.omit()

null_fit <- MASS::glm.nb(count ~ week + I(week^2) + year, mosq_cases_temp)
summary(null_fit)
plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(null_fit, type = "response"))
plot(mosq_cases_temp$count, resid(null_fit)) # null model works well

ar_fit <- MASS::glm.nb(count ~ lag_count, mosq_cases_temp)
summary(ar_fit)
plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(ar_fit, type = "response"))
plot(mosq_cases_temp$count, resid(ar_fit)) 

ar_temp_fit <- MASS::glm.nb(count ~ lag_count + lag_temp, mosq_cases_temp)
summary(ar_temp_fit)
plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(ar_temp_fit, type = "response"))
lines(mosq_cases_temp$weekyear, predict(null_fit, type = "response"), col = "red")
plot(mosq_cases_temp$count, resid(ar_temp_fit)) 

ar_temp_rain_fit <- MASS::glm.nb(count ~ lag_count + lag_temp + lag_prcp, mosq_cases_temp)
summary(ar_temp_rain_fit)
plot(count ~ weekyear, mosq_cases_temp)
lines(mosq_cases_temp$weekyear, predict(ar_temp_rain_fit, type = "response"))
lines(mosq_cases_temp$weekyear, predict(ar_temp_fit, type = "response"), col = "blue")
lines(mosq_cases_temp$weekyear, predict(null_fit, type = "response"), col = "red")
plot(mosq_cases_temp$count, resid(ar_temp_rain_fit)) 

# rain doesnt significantly contribute, but makes fit visually better
# especially for 2023

final_fit <- ar_temp_fit
car::influencePlot(final_fit)