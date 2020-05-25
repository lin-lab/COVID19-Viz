library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(magrittr)
library(data.table)
library(sf)
library(tigris)
library(USAboundaries)

POS_CUTOFF <- 50
START_DATE <- ymd("2020-03-19")

# load FIPs codes so we can merge Rts with map data.
data(fips_codes)

########################################################################
## State-level data
########################################################################

state_rt_long <- read_csv("raw_data/jhu_state_rt.csv") %>%
  select(stateName, date, mean_rt, ci_lower, ci_upper, positive, death) %>%
  data.table()
# when number of cases is too small, set Rt to NA
state_rt_long[positive >= POS_CUTOFF,
              `:=`(Rt_plot = mean_rt,
                   Rt_upr = ci_upper,
                   Rt_lwr = ci_lower)]
state_rt_long[positive < POS_CUTOFF,
              `:=`(Rt_plot = NA, Rt_upr = NA, Rt_lwr = NA)]

# make wide format
state_rt_tomerge <- state_rt_long %>%
  select(stateName, date, Rt_plot, Rt_upr, Rt_lwr, positive, death) %>%
  pivot_wider(id_cols = c(stateName), names_from = date,
              values_from = c(Rt_plot, Rt_upr, Rt_lwr, positive, death))

state_maps <- us_states()
state_merged <- merge(state_maps, state_rt_tomerge, by.x = "state_name",
                      by.y = "stateName", all.x = FALSE) %>%
  rename(stateName = state_name)

saveRDS(state_merged, "clean_data/state_merged.rds")
saveRDS(as_tibble(state_rt_long), "clean_data/state_rt_long.rds")

########################################################################
## County-level data
########################################################################

county_maps <- us_counties()
county_rt_long <- read_csv("raw_data/jhu_county_rt.csv",
                           guess_max = 10000) %>%
  select(UID, date, county, stateName, FIPS, positive, death, mean_rt,
         ci_lower, ci_upper) %>%
  data.table()
# when number of cases is too small, set Rt to NA
county_rt_long[positive >= POS_CUTOFF,
               `:=`(Rt_plot = mean_rt,
                    Rt_upr = ci_upper,
                    Rt_lwr = ci_lower)]
county_rt_long[positive < POS_CUTOFF,
               `:=`(Rt_plot = NA, Rt_upr = NA, Rt_lwr = NA)]
county_rt_long[mean_rt >= 10 & positive >= POS_CUTOFF,]
stopifnot(all(county_rt_long$Rt_upr >= county_rt_long$Rt_lwr, na.rm = TRUE))

# TODO: Weird counties. Deal with this later
weird_counties <- county_rt_long[is.na(FIPS),
                                 .(county, stateName, UID)] %>%
  distinct()
weird_counties

max_date <- max(county_rt_long$date)

county_rt_long %>%
  filter(stateName == "Massachusetts", date == max_date)
county_rt_long %>%
  filter(stateName == "New York", date == max_date)
county_rt_long %>%
  filter(stateName == "Missouri", date == max_date)
county_rt_long %>%
  filter(stateName == "Utah", date == max_date)

county_rt_long %>%
  filter(county == "Kansas City")

county_rt_wide <- county_rt_long %>%
  filter(!(UID %in% weird_counties$UID)) %>%
  select(county, stateName, UID, FIPS, date, Rt_plot, Rt_upr, Rt_lwr,
         positive, death) %>%
  pivot_wider(id_cols = c(county, stateName, UID, FIPS),
              names_from = date,
              values_from = c(Rt_plot, Rt_upr, Rt_lwr, positive, death))

# check if there will be counties that don't merge with the county maps
state_fips <- fips_codes %>%
  select(state_code, state_name) %>%
  distinct()
# data frame of FIPS in the map
maps_df <- data.frame(geoid = county_maps$geoid,
                      name = county_maps$name,
                      statefp = county_maps$statefp,
                      stringsAsFactors = FALSE) %>%
  inner_join(state_fips, by = c("statefp" = "state_code"))

old_names <- anti_join(county_rt_wide, maps_df,
                       by = c("FIPS" = "geoid")) %>%
  select(FIPS, county, stateName)
print(old_names, n = Inf)


# do the merge
county_merged <- merge(county_maps, county_rt_wide, by.x = "geoid",
                       by.y = "FIPS", all.x = FALSE)

saveRDS(county_merged, "clean_data/county_merged.rds")
saveRDS(as_tibble(county_rt_long), "clean_data/county_rt_long.rds")

########################################################################
## County and state names
########################################################################

state_names1 <- state_merged %>%
  filter(stateName != "District of Columbia") %>%
  use_series(stateName) %>%
  paste("State", sep = " ")



state_names <- c(state_names1, "District of Columbia")
county_names <- paste(county_merged$county, county_merged$stateName,
                      sep = ", ")
state_county_choices <- c(state_names, county_names)
saveRDS(state_county_choices, "clean_data/state_county_choices.rds")

state_choices <- unique(county_merged$stateName)
saveRDS(state_choices, "clean_data/state_choices.rds")
