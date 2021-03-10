The Rt, case rate, and death rate might also not be shown for certain locations
and certain dates if that location had too few total cases or too few new cases
on that date. Occasionally, locations may have negative values for new cases or
new deaths because of reporting issues. Full table available for download [on
Github](https://github.com/lin-lab/COVID19-Viz/blob/master/clean_data_pois).

## Table Column Explanations

+ **Location**: Location name as a string.
+ **Rt, Rt (lwr), Rt (upr)**: Value of Rt and the lower and upper 95% confidence
  interval bounds, calculated using the Poisson method. Note that there is a
  7-day lag for Rt, so there are no estimates of Rt or its confidence interval
  in the last 7 days of the data.
+ **Case rate, Case rate (lwr), Case rate (upr)**: Value of the case rate per
  million population and the lower and upper 95% confidence interval bounds,
  calculated using the Poisson method.
+ **Death rate, Death rate (lwr), Death rate (upr)**: Value of the death rate
  per million population and the lower and upper 95% confidence interval bounds,
  calculated using the Poisson method.
+ **Cum. cases**: Cumulative number of cases up to and including the date
  selected.
+ **Daily new cases**: Number of new COVID-19 cases on the date selected. Note
  that different locations may define a case differently, e.g. some may only
  include those confirmed by PCR tests, while others would include those who
  tested positive on an antibody test and had COVID-19 symptoms.
+ **Cum. cases per million**: Number of cumulative cases up to and including the
  date selected per million population. We used the population numbers from the
  JHU CSSE data to calculate any per capita statistics.
+ **Daily new cases per million**: Number of new COVID-19 cases on the date
  selected per million population. Note that different locations may define a
  case differently, e.g. some may only include those confirmed by PCR tests,
  while others would include those who tested positive on an antibody test and
  had COVID-19 symptoms.
+ **Cum. deaths**: Cumulative number of deaths up to and including the date
  selected. Note that different locations may define a COVID-19 death
  differently.
+ **Daily new deaths**: Number of new COVID-19 deaths on the date selected. Note
  that different locations may define a COVID-19 death differently.
+ **Cum. deaths per million**: Cumulative number of deaths up to and including
  the date selected per million population.
+ **Daily new deaths per million**: Number of new deaths on the date selected
  per million population.
+ **Population**: Population of the location from the JHU CSSE data used to
  calculate any per capita statistics.
