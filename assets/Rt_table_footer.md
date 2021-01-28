Only locations with more than 50 total cases are shown. Occasionally, locations
may have negative values for new cases or new deaths because of reporting
issues. Full table available for download [on
Github](https://github.com/lin-lab/COVID19-Viz/blob/master/clean_data).

## Table Column Explanations

+ `UID`: Unique location ID. Taken from JHU's [UID lookup
  table](https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv).
+ `dispID`: Location name as a string.
+ `date`: Rt was calculated using a 7-day window starting 7 days before this
  date and ending on this date. This is also the date of the cases, new cases,
  deaths, new deaths, etc for that row.
+ `date_lag`: 5 days before `date`. Because the number of reported cases on a
  particular day does not represent the number of people who contracted
  COVID-19, the Rt curve needs to be adjusted to account for the fact that
  people contract COVID-19 before their case gets counted. As a heuristic, we
  assume that there is a 5-day lag from the time a person contracts COVID-19
  until they are reported as a case, so we shift the Rt curve back 5 days to
  reflect this.
+ `Rt_plot`: Value of Rt for plotting. This differs from the Rt value calculated
  by EpiEstim
  [here](https://github.com/lin-lab/COVID19-Rt/tree/master/initial_estimates)
  when the number of cases or new cases is too low. Rt estimation becomes
  unstable when there are only a few new cases per day or when the total number
  of cases is small. Because of low population size or lack of reporting, this
  is the case for many rural counties in the US as well as many countries with
  underdeveloped healthcare infrastructure. Therefore, we do not show the Rt
  value on dates when the number of total cases is below 50 or when the average
  number of new cases within the previous 7 days is below 10. When the number of
  total cases is below 50, Rt_plot is set to -888. When the number of total
  cases is above 50 but the average number of new cases within the previous 7
  days is below 10, Rt_plot is set to -88.
+ `Rt_upr` and `Rt_lwr`: Upper and lower bounds of the 95% credible interval for
  Rt. Set to NA whenever the Rt estimate would be unstable (see above).
+ `Rt_loess_fit`, `Rt_loess_upr`, and `Rt_loess_lwr`: Loess fit to positive
  values of `Rt_plot` against `date_lag`. `Rt_loess_fit` is the fitted value,
  and `Rt_loess_upr`/`Rt_loess_lwr` are the upper and lower 95% confidence
  intervals from loess, respectively. They are set to NA whenever the Rt
  estimate would be unstable (see above).
+ `positiveIncrease`: Number of new COVID-19 cases on that date. Note that
  different locations may define a case differently, e.g. some may only include
  those confirmed by PCR tests, while others would include those who tested
  positive on an antibody test and had COVID-19 symptoms.
+ `positive`: Cumulative number of cases up to and including that date.
+ `positive_7day`: Average number of new cases in the past 7 days.
+ `positive_percapita`: Number of cumulative cases up to and including that day
  per million population. We used the population numbers from the JHU CSSE data
  to calculate any per capita statistics.
+ `positiveIncr_percapita`: Number of new cases on that day per million
  population.
+ `deathIncrease`: Number of new COVID-19 deaths on that date. Note that different
  locations may define a COVID-19 death differently.
+ `death`: Cumulative number of deaths up to and including that date.
+ `positive_7day`: Average number of new cases in the past 7 days.
+ `death_percapita`: Number of cumulative deaths up to and including that day
  per million population.
  We used the population numbers from the JHU CSSE data to calculate this.
+ `deathIncr_percapita`: Number of new deaths on that day per million
  population.
