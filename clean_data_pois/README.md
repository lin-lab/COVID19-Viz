# Cleaned Data from Poisson GLM Method

These are the cleaned data files that we use for our COVID-19 Rt visualization.
These files are generated by `02_clean_merge.R`.

+ [`names_list.rds`](https://hsph-covid-study.s3.us-east-2.amazonaws.com/website_files_pois/names_list.rds):
  A list of lists that maps UIDs of locations to their names. The outer list
  contains entries for states/provinces, US counties, countries, and US states
  with counties (Some "states" like Puerto Rico or DC don't have counties).
+ [`rt_long_all.rds`](https://hsph-covid-study.s3.us-east-2.amazonaws.com/website_files_pois/rt_long_all.rds):
  A long [`data.table`](https://cran.r-project.org/package=data.table)
  containing the Rt and various other variables for each location and each date.
  See below for description of columns.
+ [`rt_table_export.csv.zip`](https://hsph-covid-study.s3.us-east-2.amazonaws.com/website_files_pois/rt_table_export.csv.zip):
  A zipped CSV version of the above file. This file is the most suited for
  download.
+ [`sf_all.rds`](https://hsph-covid-study.s3.us-east-2.amazonaws.com/website_files_pois/sf_all.rds):
  An [sf](https://cran.r-project.org/package=sf) object containing the Rt values
  merged with the shape information for each geographical entity on our map
  (states/provinces, counties, and countries).
+ [`state_centers.rds`](https://hsph-covid-study.s3.us-east-2.amazonaws.com/website_files_pois/state_centers.rds):
  A list mapping state UIDs to their centers. Used for
  setting the location and zoom for the map on the explore states tab.

## Rt Table CSV File

The `rt_table_export.csv.zip` is a zipped file of a csv file. It contains all
the information shown on the tables on the website as well as some additional
columns. If you wish to download the tables, you should download this file.

### Description of Columns

+ `UID`: Unique location ID. Taken from JHU's [UID lookup
  table](https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv).
+ `dispID`: Location name as a string.
+ `date`: Rt was calculated using a 7-day window starting 7 days before this
  date and ending on this date. This is also the date of the cases, new cases,
  deaths, new deaths, etc for that row.
+ `date_lag`: 7 days before `date`. Because the number of reported cases on a
  particular day does not represent the number of people who contracted
  COVID-19, the Rt curve needs to be adjusted to account for the fact that
  people contract COVID-19 before their case gets counted. As a heuristic, we
  assume that there is a 7-day lag from the time a person contracts COVID-19
  until they are reported as a case, so we shift the Rt curve back 7 days to
  reflect this.
+ `resolution`: Specifies whether this location is a country (`country`),
  county (`county`), or subnational unit (any entry starting with `subnat_`).
+ `population`: Population of the location from the JHU CSSE data used to
  calculate any per capita statistics.
+ `rt`, `rt_lower`, `rt_upper`: Value of estimated Rt and the lower and upper
  95% confidence interval bounds, calculated using the Poisson method. Note that
  there is a 7-day lag for Rt, so there are no estimates of Rt or its confidence
  interval in the last 7 days of the data.
+ `case_rate`, `case_lower`, `case_upper`: Value of the estimated case rate per
  million population and the lower and upper 95% confidence interval bounds,
  calculated using the Poisson method.
+ `death_rate`, `death_lower`, `death_upper`: Value of the estimated death rate
  per million population and the lower and upper 95% confidence interval bounds,
  calculated using the Poisson method.
+ `positive`: Cumulative number of cases up to and including the date
  selected.
+ `positiveIncrease`: Number of new COVID-19 cases on the date selected. Note
  that different locations may define a case differently, e.g. some may only
  include those confirmed by PCR tests, while others would include those who
  tested positive on an antibody test and had COVID-19 symptoms.
+ `positive_percapita`: Number of cumulative cases up to and including the
  date selected per million population. We used the population numbers from the
  JHU CSSE data to calculate any per capita statistics.
+ `positiveIncrease_percapita`: Number of new COVID-19 cases on the date
  selected per million population. Note that different locations may define a
  case differently, e.g. some may only include those confirmed by PCR tests,
  while others would include those who tested positive on an antibody test and
  had COVID-19 symptoms.
+ `death`: Cumulative number of deaths up to and including the date
  selected. Note that different locations may define a COVID-19 death
  differently.
+ `deathIncrease`: Number of new COVID-19 deaths on the date selected. Note
  that different locations may define a COVID-19 death differently.
+ `death_percaptia`: Cumulative number of deaths up to and including
  the date selected per million population.
+ `deathIncrease_percapita`: Number of new deaths on the date selected
  per million population.