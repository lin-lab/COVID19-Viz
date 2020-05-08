library(readr)
library(dplyr)
library(tidyr)
library(sp)
library(tigris)
library(lubridate)
library(stringr)
library(magrittr)
library(data.table)

POS_CUTOFF <- 50
START_DATE <- ymd("2020-03-19")

# load FIPs codes so we can merge Rts with map data.
data(fips_codes)

########################################################################
## State-level data
########################################################################
state_rt_wide <- read_csv("raw_data/State_Specific_Rt_5.5_8pm.csv") %>%
  rename(State = Location) %>%
  filter(State != "USA")
rt_cols <- grepl("[[:digit:]]$", colnames(state_rt_wide))
colnames(state_rt_wide)[rt_cols] <- paste0(colnames(state_rt_wide)[rt_cols], "_RtOrig")

# make data into long format
state_rt_long <- state_rt_wide %>%
  pivot_longer(-c(FIPS, State),
               names_to = c("Date", "variable"),
               names_pattern = "([0-9\\-]+)_(.*)",
               values_to = "value") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date >= START_DATE) %>%
  rename(posIncr = pos_incr, totalPos = total_pos) %>%
  data.table()

# when number of cases is too small, set Rt to NA
state_rt_long[totalPos >= POS_CUTOFF, Rt := RtOrig]
state_rt_long[totalPos < POS_CUTOFF, Rt := NA]
# one day in Rhode Island has Rt of >100 probably because of reporting
# Set those as Na also
state_rt_long[RtOrig > 100, Rt := NA]

state_rt_tomerge <- state_rt_long %>%
  pivot_wider(id_cols = c(FIPS, State), names_from = Date,
              values_from = c(posIncr, totalPos, Rt))

state_maps <- states(cb = TRUE, resolution = "20m")
state_merged <- merge(state_maps, state_rt_tomerge, by.x = "NAME",
                      by.y = "State", all.x = FALSE)
saveRDS(state_merged, "clean_data/state_merged.rds")
saveRDS(as_tibble(state_rt_long), "clean_data/state_rt_long.rds")

########################################################################
## County-level data
########################################################################

county_maps <- counties(cb = TRUE, resolution = "20m")
county_rt_wide <- read_csv("raw_data/County_Specific_Rt_5.5_8pm.csv") %>%
  mutate(FIPS_str = sprintf("%05d", FIPS))
rt_cols <- grepl("[[:digit:]]$", colnames(county_rt_wide))
colnames(county_rt_wide)[rt_cols] <- paste0(colnames(county_rt_wide)[rt_cols], "_RtOrig")

# take care of two counties whose names and FIPS have changed
state_fips <- fips_codes %>%
  select(state_code, state_name) %>%
  distinct()
maps_df <- data.frame(GEOID = county_maps$GEOID,
                      name = county_maps$NAME,
                      statefp = county_maps$STATEFP,
                      stringsAsFactors = FALSE) %>%
  inner_join(state_fips, by = c("statefp" = "state_code"))

new_names <- anti_join(maps_df, county_rt_wide,
                       by = c("GEOID" = "FIPS_str")) %>%
  filter(state_name != "Puerto Rico")
new_names
old_names <- anti_join(county_rt_wide, maps_df,
                       by = c("FIPS_str" = "GEOID")) %>%
  select(FIPS_str, County, State)
old_names
stopifnot(nrow(new_names) == nrow(old_names))

# replace the names
for (i in 1:nrow(new_names)) {
  index <- county_rt_wide$FIPS_str == old_names$FIPS_str[i]
  county_rt_wide$FIPS_str[index] <- new_names$GEOID[i]
  stopifnot(county_rt_wide$County[index] == old_names$County[i])
  county_rt_wide$County[index] <- new_names$name[i]
}

# check that the replacement happened
nrows_new <- county_rt_wide %>%
  filter(FIPS_str %in% new_names$GEOID) %>%
  nrow()
stopifnot(nrows_new == nrow(new_names))
nrows_old <- county_rt_wide %>%
  filter(FIPS_str %in% old_names$FIPS_str) %>%
  nrow()
stopifnot(nrows_old == 0)

# convert wide data to long data
county_rt_long <- county_rt_wide %>%
  select(-FIPS) %>%
  pivot_longer(-c(FIPS_str, County, State),
               names_to = c("Date", "variable"),
               names_pattern = "([0-9\\-]+)_(.*)",
               values_to = "value") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date >= START_DATE) %>%
  rename(posIncr = pos_incr, totalPos = total_pos) %>%
  data.table()
# set Rt based on number of positive cases
county_rt_long[totalPos >= POS_CUTOFF, Rt := RtOrig]
county_rt_long[totalPos < POS_CUTOFF, Rt := NA]

# one day in Rhode Island has Rt of >100 probably because of reporting
# Set those as Na also
county_rt_long[RtOrig > 100, Rt := NA]

# prepare to merge
county_rt_tomerge <- county_rt_long %>%
  pivot_wider(id_cols = c(FIPS_str, County, State),
              names_from = Date,
              values_from = c(posIncr, totalPos, Rt))

# do the merge
county_merged <- merge(county_maps, county_rt_tomerge, by.x = "GEOID",
                       by.y = "FIPS_str", all.x = FALSE)

saveRDS(county_merged, "clean_data/county_merged.rds")
saveRDS(as_tibble(county_rt_long), "clean_data/county_rt_long.rds")

########################################################################
## County and state names
########################################################################

state_names1 <- state_rt_wide %>%
  filter(State != "District Of Columbia") %>%
  use_series(State) %>%
  paste("State", sep = " ")

state_names <- c(state_names1, "District Of Columbia")
county_names <- paste(county_rt_tomerge$County, county_rt_tomerge$State,
                      sep = ", ")
state_county_choices <- c(state_names, county_names)
saveRDS(state_county_choices, "clean_data/state_county_choices.rds")
