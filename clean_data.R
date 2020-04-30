library(readr)
library(dplyr)
library(tidyr)
library(sp)
library(tigris)
library(lubridate)
library(stringr)
library(magrittr)

# load FIPs codes so we can merge Rts with map data.
data(fips_codes)

########################################################################
## State-level data
########################################################################
state_rt_wide <- read_csv("raw_data/State_Specific_Rt_3.1_4.20_updated4.28.csv")
state_rt_long <- state_rt_wide %>%
  gather(key = "Date", value = "Rt", -State, -Total_Cases) %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date >= ymd("2020-03-19"))
state_maps <- states(cb = TRUE, resolution = "20m")
state_merged <- merge(state_maps, state_rt_wide, by.x = "NAME",
                      by.y = "State")
saveRDS(state_merged, "clean_data/state_merged.rds")
saveRDS(state_rt_long, "clean_data/state_rt_long.rds")

########################################################################
## County-level data
########################################################################

county_rt <- read_csv("raw_data/County_Specific_Rt_3.1_4.20_updated4.28.csv")
county_maps <- counties(cb = TRUE, resolution = "20m")

county_rt %>%
  filter(State == "NaN") %>%
  select(County)


# remove last word in each county (e.g. change Middlesex County ->
# Middlesex)
fips_codes_clean <- fips_codes %>%
  mutate(county_name = str_trim(str_replace(county, "\\w+$", "")))

# should have no NAs
fips_codes_clean %>%
  filter(is.na(county_name))

# merge FIPS codes with county level Rts
county_rt_fips <- merge(county_rt, fips_codes_clean,
                        by.x = c("County", "State"),
                        by.y = c("county_name", "state_name")) %>%
  mutate(GEOID = paste0(state_code, county_code)) %>%
  as_tibble()

county_merged <- merge(county_maps, county_rt_fips, by = "GEOID")
saveRDS(county_merged, "clean_data/county_merged.rds")

county_rt_long <-
  gather(county_rt_fips, key = "Date", value = "Rt",
         -County, -State, -Total_Cases, -state, -state_code,
         -county_code, -county, -GEOID) %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date >= ymd("2020-03-19"))
saveRDS(county_rt_long, "clean_data/county_rt_long.rds")

########################################################################
## County and state names
########################################################################

state_names1 <- state_rt_wide %>%
  filter(State != "District Of Columbia") %>%
  use_series(State) %>%
  paste("State", sep = " ")

state_names <- c(state_names1, "District Of Columbia")
county_names <- paste(county_rt_fips$county, county_rt_fips$state,
                      sep = ", ")
state_county_choices <- c(state_names, county_names)
saveRDS(state_county_choices, "clean_data/state_county_choices.rds")
