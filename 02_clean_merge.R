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
library(rnaturalearth)
library(rgeos)
library(purrr)
library(ggplot2)


#' Calculate loess curve for Rt by date_lag.
Rt_loess <- function(dat) {
  if (nrow(dat) < 3) {
    ret_df <- data.table(Rt_loess_fit = NA_real_,
                         Rt_loess_lwr = NA_real_,
                         Rt_loess_upr = NA_real_,
                         rowid = dat$rowid)
  } else {
    loess_fit <- loess(Rt_plot ~ as.numeric(date_lag), data = dat)
    pred_obj <- predict(loess_fit, dat, se = TRUE)
    tstar <- qt(0.975, pred_obj$df)
    ret_df <- data.table(Rt_loess_fit = pred_obj$fit,
                        Rt_loess_lwr = pred_obj$fit - tstar * pred_obj$se.fit,
                        Rt_loess_upr = pred_obj$fit + tstar * pred_obj$se.fit,
                        rowid = dat$rowid)
  }
  return(ret_df)
}

#' Lag, subset, and modify the Rt data.
#'
#' This function lags the Rt by a specified number of days. Then, we filter
#' out Rts outside the date range. We also set Rt to be NA on days when there
#' are not enough total cases or when the average number of new cases in the
#' past few days is below a certain threshold. We do this for each location in
#' the data. We also compute the per capita case and death rate (per 10000)
#' and the per capita new cases and new deaths (per million)
#'
#' @param dt Input data frame. Assumed to have columns date, positiveIncrease,
#' positive, mean_rt, ci_upper, ci_lower.
#' @param group_var Variable that indicates each location.
#' @param nlag Number of days to lag Rt by.
#' @param window_size Number of days to average the new cases over. Note that
#' the window is right-aligned, e.g. if the date is June 7, we will take the
#' average from June 1 to June 7.
#' @param pos_cutoff Set Rt to NA if the number of total cases on any particular
#' day does not exceed this number.
#' @param posincr_cutoff Set Rt to NA if the average number of new cases on
#' the past window_size days does not exceed this number.
#' @param start_date Take times after this start date.
#' @param end_date Take times before this end date.
#'
lag_subset_mod <- function(dt, group_var = "UID", nlag = 5,
                           window_size = 7, pos_cutoff = 50,
                           posincr_cutoff = 10,
                           start_date = ymd("2020-03-19"),
                           end_date = today()) {
  stopifnot(start_date <= end_date)
  dt[positiveIncrease <= 0, posIncr_trunc := 0]
  dt[positiveIncrease > 0, posIncr_trunc := positiveIncrease]
  dt[, date_lag := date - ddays(nlag)]
  dt[, rolling_posIncr := frollmean(posIncr_trunc, n = window_size,
                                    align = "right", algo = "exact"),
      by = group_var]
  dt[, positive_7day := frollmean(positiveIncrease, n = 7,
                                  align = "right", algo = "exact"),
      by = group_var]
  dt[, positive_percapita := 1e6 * positive/ population]
  dt[, death_percapita := 1e6 * death / population]
  dt[, positiveIncr_percapita := 1e6 * positiveIncrease / population]
  dt[, deathIncr_percapita := 1e6 * deathIncrease / population]

  dt_subset <- dt[date >= start_date & date <= end_date & !is.na(rolling_posIncr)]
  dt_subset[positive >= pos_cutoff & rolling_posIncr >= posincr_cutoff,
      `:=` (Rt_plot = mean_rt, Rt_upr = ci_upper, Rt_lwr = ci_lower)]
  dt_subset[positive >= pos_cutoff & rolling_posIncr < posincr_cutoff,
      Rt_plot := -88]
  dt_subset[positive < pos_cutoff, Rt_plot := -888]
  dt_subset[, rowid := rep(1:.N)]

  loess_dt <- dt_subset[Rt_plot > 0, Rt_loess(.SD), by = group_var]
  ret <- merge(dt_subset,
               loess_dt[, .(Rt_loess_fit, Rt_loess_lwr, Rt_loess_upr, rowid)] ,
               by = "rowid", all.x = TRUE, all.y = FALSE)
  ret[, rowid := NULL]

  return(ret)
}

#end_date <- ymd("2020-06-06")
end_date <- today()

########################################################################
## State-level data
########################################################################

state_rt_long_orig <- read_tsv("raw_data/jhu_state_rt.tsv") %>%
  data.table()

state_rt_long <- lag_subset_mod(state_rt_long_orig, "stateName",
                                end_date = end_date)

state_rt_long[date >= end_date - ddays(2),
              .(stateName, positive, population, positive_percapita)][
              order(positive_percapita, decreasing = TRUE)]


# check that we calculated the rolling mean correctly
state_check <- state_rt_long[stateName == "New Jersey", ]
stopifnot(all.equal(state_check$rolling_posIncr[7],
                    mean(state_check$posIncr_trunc[1:7])))
stopifnot(all.equal(state_check$rolling_posIncr[8],
                    mean(state_check$posIncr_trunc[2:8])))
stopifnot(all.equal(state_check$rolling_posIncr[10],
                    mean(state_check$posIncr_trunc[4:10])))

# make wide format
state_rt_tomerge <- state_rt_long %>%
  select(stateName, date_lag, Rt_plot, Rt_upr, Rt_lwr) %>%
  pivot_wider(id_cols = c(stateName), names_from = date_lag,
              values_from = c(Rt_plot, Rt_upr, Rt_lwr))

state_maps <- ne_states(country = "united states of america",
                        returnclass = "sf")

# Add points for American Samoa, Guam, Puerto Rico, etc
sort(state_maps$name)
jhu_states <- state_rt_tomerge$stateName
add_states <- dplyr::setdiff(jhu_states, state_maps$name)
print(add_states)

unjoined_states <- state_rt_long %>%
  filter(stateName %in% add_states) %>%
  select(stateName, UID, Lat, Long_) %>%
  distinct()

#' Make point geometries from a data frame.
#'
make_points <- function(df, name_col, crs) {
  point_lst <- list()
  for (i in 1:nrow(df)) {
    long_cur <- df$Long_[i]
    lat_cur <- df$Lat[i]
    point_lst[[i]] <- st_point(c(long_cur, lat_cur))
  }

  new_points <- st_sf(UID = df$UID,
                      name = df[[name_col]],
                      geometry = st_sfc(point_lst, crs = crs))
  return(new_points)
}

new_state_points <- make_points(unjoined_states, "stateName", st_crs(state_maps))

state_merged <- state_maps %>%
  select(name, geometry, fips) %>%
  mutate(UID = as.integer(paste0("840000", substring(fips, 3)))) %>%
  select(-fips) %>%
  rbind(new_state_points) %>%
  merge(state_rt_tomerge, by.x = "name", by.y = "stateName", all.x = FALSE) %>%
  mutate(resolution = "state_USA", dispID = paste0(name, ", USA")) %>%
  select(UID, dispID, resolution, starts_with("Rt_"))

exported_states <-
  with(state_merged,
       data.table(UID = UID, stateName = substr(dispID, 1, nchar(dispID) - 5)))
state_rt_long_export <- state_rt_long[exported_states, on = "stateName"] %>%
  mutate(resolution = "state_USA",
         dispID = paste0(stateName, ", USA")) %>%
  select(UID, dispID, date, resolution, date_lag,
         starts_with("Rt_"), starts_with("positive"), starts_with("death"))

########################################################################
## County-level data
########################################################################

county_maps <- us_counties() %>%
  mutate(UID = as.integer(paste0("840", geoid)))
county_rt_long_orig <- read_tsv("raw_data/jhu_county_rt.tsv",
                                guess_max = 10000) %>%
  data.table()

county_rt_long <- lag_subset_mod(county_rt_long_orig, end_date = end_date)
stopifnot(all(county_rt_long$Rt_upr >= county_rt_long$Rt_lwr, na.rm = TRUE))

# make combined key for county rt long
county_rt_long[, Combined_Key := paste(county, stateName, sep = ", ")]

# Fix weird counties.
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
  filter(county == "Kansas City")
county_rt_long %>%
  filter(stateName == "Utah", date == max_date)

# join Dukes and Nantucket in MA
dukes_nantucket <- county_maps %>%
  filter(name %in% c("Dukes", "Nantucket"),
         state_name == "Massachusetts")
stopifnot(nrow(dukes_nantucket) == 2)
sf_dukes_nantucket <- st_sf(UID = 84070002,
                            geometry = st_union(dukes_nantucket))
county_rt_long <- county_rt_long[!(county %in% c("Dukes", "Nantucket") &
                                    stateName == "Massachusetts")]

# join NYC counties
nyc_remove <- c("Kings", "Queens", "Bronx", "Richmond")
nyc_counties <- c("New York", nyc_remove)
nyc_maps <- county_maps %>%
  filter(name %in% nyc_counties, state_name == "New York")
stopifnot(nrow(nyc_maps) == 5)
sf_nyc <- st_sf(UID = 84036061,
                geometry = st_union(nyc_maps))
county_rt_long <- county_rt_long[!(county %in% nyc_remove &
                                    stateName == "New York")]
county_rt_long[stateName == "New York", unique(county)]
# We remove New York County so we can replace it later
county_maps <- county_maps %>%
  filter(UID != 84036061)

# make a point for Kansas City
sf_kc <- st_sf(UID = 84070003,
               geometry = st_sfc(st_point(c(-94.5786, 39.0997)),
                                 crs = st_crs(county_maps)))

# join Utah counties
utah_join <- list(
  `Bear River` = c("Box Elder", "Cache", "Rich"),
  TriCounty = c("Daggett", "Duchesne", "Uintah"),
  `Central Utah` = c("Juab", "Millard", "Sanpete", "Sevier", "Piute", "Wayne"),
  `Southeast Utah` = c("Emery", "Grand", "Carbon"),
  `Southwest Utah` = c("Beaver", "Iron", "Washington", "Kane", "Garfield"),
  `Weber-Morgan` = c("Weber", "Morgan"))

utah_sf_orig <- county_maps %>%
  filter(state_name == "Utah")
new_geometries <- list()
utah_uids <- list()
for (i in seq_along(utah_join)) {
  join_counties <- utah_sf_orig %>%
    filter(name %in% utah_join[[i]]) %>%
    select(geometry)
  new_geometries[[i]] <- st_union(join_counties)
  utah_uids[[i]] <- county_rt_long %>%
    filter(stateName == "Utah", date == max_date,
           county == names(utah_join)[i]) %>%
    use_series(UID)
}

sf_utah <- st_sf(UID = unlist(utah_uids),
                 geometry = st_sfc(unlist(new_geometries,
                                          recursive = FALSE),
                                   crs = st_crs(county_maps)))
utah_counties_remove <- unlist(utah_join)
county_rt_long <- county_rt_long[!(county %in% utah_counties_remove &
                                   stateName == "Utah")]

county_rt_wide <- county_rt_long %>%
  select(county, stateName, UID, FIPS, date_lag, Rt_plot, Rt_upr, Rt_lwr,
         Combined_Key) %>%
  pivot_wider(id_cols = c(county, stateName, UID, FIPS, Combined_Key),
              names_from = date_lag,
              values_from = c(Rt_plot, Rt_upr, Rt_lwr))

county_maps_new <- county_maps %>%
  select(UID, geometry) %>%
  rbind(sf_dukes_nantucket, sf_nyc, sf_kc, sf_utah)

# check if there will be counties that don't merge with the county maps
# data frame of FIPS in the map
maps_df <- data.frame(UID = county_maps_new$UID)

old_names <- anti_join(county_rt_wide, maps_df, by = "UID") %>%
  select(UID, county, stateName)
print(old_names, n = Inf)

# do the merge
county_merged <- county_maps_new %>%
  mutate(resolution = "county") %>%
  merge(county_rt_wide, by = "UID", all = FALSE) %>%
  select(-county, -stateName, -FIPS) %>%
  select(UID, dispID = Combined_Key, resolution, starts_with("Rt_"))

exported_counties <- data.table(UID = county_merged$UID)
county_rt_long_export <- county_rt_long[exported_counties, on = "UID"] %>%
  mutate(resolution = "county") %>%
  select(UID, dispID = Combined_Key, date, date_lag, resolution,
         starts_with("Rt_"), starts_with("positive"), starts_with("death"))

########################################################################
## International data
########################################################################

global_rt_long_orig <- fread("raw_data/jhu_global_rt.tsv", fill = TRUE)
global_rt_long_orig[Country_Region == "Canada",]
global_rt_long_orig[, date := ymd(date)]

# fix a typo
global_rt_long_orig[UID == 12406,
                    Combined_Key := "Northwest Territories, Canada"]

global_rt_long <- lag_subset_mod(global_rt_long_orig, end_date = end_date)
stopifnot(all(global_rt_long$Rt_upr >= global_rt_long$Rt_lwr, na.rm = TRUE))

global_rt_wide <- global_rt_long %>%
  select(-population, -interval_start, -FIPS, -Admin2, -positiveIncrease,
         -deathIncrease, -positive, -positiveIncrease, -death) %>%
  pivot_wider(id_cols = c(UID, Province_State:Combined_Key),
              names_from = date_lag,
              values_from = c(Rt_plot, Rt_upr, Rt_lwr)) %>%
  data.table()
stopifnot(uniqueN(global_rt_wide$UID) == nrow(global_rt_wide))

########################################################################
## Take care of provinces
########################################################################
countries_w_provinces <- c("Canada", "Australia", "China")
province_uids <- global_rt_wide[!is.na(Province_State) &
                                 Country_Region %in% countries_w_provinces,
                                UID]

provinces_wide <- global_rt_wide[UID %in% province_uids]
countries_wide <- global_rt_wide[!(UID %in% province_uids)]

provinces_sf_orig <- ne_states(geounit = c("Canada", "Australia", "China"),
                          returnclass = "sf")
x <- provinces_sf_orig %>%
  filter(admin == "China")
x$name_en
x[19,]
sort(x$name_en)

x <- provinces_sf_orig %>%
  filter(admin == "Canada")
sort(x$name_en)

x <- provinces_sf_orig %>%
  filter(admin == "Australia")
x$name
sort(x$name)

remove_provinces <- c("Nunavut", "Jervis Bay Territory",
                      "Macquarie Island", "Lord Howe Island")

provinces_sf <- provinces_sf_orig %>%
  filter(!(name %in% remove_provinces)) %>%
  select(name, admin, name_en, geometry) %>%
  group_by(admin) %>%
  arrange(name_en) %>%
  mutate(
    province_id = sprintf("%02d", 1:n()),
    country_id = case_when(
      admin == "Australia" ~ "36",
      admin == "China" ~ "156",
      admin == "Canada" ~ "124"
    ),
    UID = as.integer(paste0(country_id, province_id))
  ) %>%
  ungroup() %>%
  select(UID, country_id, province_id, name, admin, geometry) %>%
  st_sf()

# Add Macau and Hong Kong
macau_hk <- global_rt_wide[Province_State %in% c("Macau", "Hong Kong")]
stopifnot(nrow(macau_hk) == 2)

macau_hk_sf <- make_points(macau_hk, "Combined_Key", st_crs(provinces_sf))

provinces_sf_final <- provinces_sf %>%
  select(UID, geometry) %>%
  rbind(macau_hk_sf[, c("UID", "geometry")], .)

# see which provinces didn't merge
x1 <- data.frame(UID = provinces_sf_final$UID)
x2 <- data.frame(UID = provinces_wide$UID,
                 name = provinces_wide$Combined_Key)
anti_join(x1, x2, by = "UID")
anti_join(x2, x1, by = "UID")

provinces_merged <- provinces_sf_final %>%
  select(UID, geometry) %>%
  merge(provinces_wide, by = "UID", all = FALSE) %>%
  rename(dispID = Combined_Key) %>%
  mutate(resolution = case_when(
    Country_Region == "Canada" ~ "state_Canada",
    Country_Region == "China" ~ "state_China",
    Country_Region == "Australia" ~ "state_Australia")) %>%
  select(UID, dispID, resolution, starts_with("Rt_"))

exported_provinces <- data.table(UID = provinces_merged$UID)
provinces_rt_long_export <- global_rt_long[exported_provinces, on = "UID"] %>%
  mutate(resolution = case_when(
    Country_Region == "Canada" ~ "state_Canada",
    Country_Region == "China" ~ "state_China",
    Country_Region == "Australia" ~ "state_Australia")) %>%
  select(UID, dispID = Combined_Key, date, date_lag, resolution,
         starts_with("Rt_"), starts_with("positive"), starts_with("death"))

########################################################################
## International countries
########################################################################

sf_tiny_countries <- ne_countries(type = "tiny_countries",
                                  returnclass = "sf")
sf_world_orig <- ne_countries(returnclass = "sf")

# separate out France and French Guiana
french_guiana <- ne_states(geounit = "french guiana", returnclass = "sf") %>%
  select(geometry) %>%
  mutate(UID = 254, name = "French Guiana")
metro_france <- st_sf(UID = 250, name = "France",
  geometry = st_union(ne_states(geounit = "france", returnclass = "sf")))

# ignore northern cyprus and somaliland but keep kosovo
sf_world_orig %>%
  filter(is.na(iso_n3)) %>%
  select(admin)

# get rid of France for now. France includes French Guiana, but French Guiana is
# counted separately by JHU. We add France back in at the end.
sf_world_use <- sf_world_orig %>%
  filter(admin != "France") %>%
  mutate(
    UID = case_when(
      admin == "Kosovo" ~ 383L,
      TRUE ~ as.integer(iso_n3)
    )
  ) %>%
  select(UID, name, geometry) %>%
  rbind(french_guiana, metro_france)

# ignore these countries
sf_tiny_countries$iso_n3
sf_tiny_countries %>%
  filter(is.na(iso_n3))

# check if there are any tiny countries in the original world countries
world_countries_temp <- dplyr::select(sf_world_orig, iso_n3, name) %>%
  st_drop_geometry()
tiny_countries_temp <- dplyr::select(sf_tiny_countries, iso_n3, name) %>%
  st_drop_geometry()
inner_join(world_countries_temp, tiny_countries_temp, by = "iso_n3")

# remove Trinidad and Tobago and Brunei from tiny countries
remove_iso_n3 <- c("096", "780")
sf_tiny_use <- sf_tiny_countries %>%
  filter(!(iso_n3 %in% remove_iso_n3)) %>%
  mutate(UID = as.integer(iso_n3)) %>%
  select(UID, name, geometry)

sf_world_temp <- rbind(sf_world_use, sf_tiny_use)

# Get countries hat don't join.
x1 <- with(sf_world_temp,
           data.frame(UID = UID, name = name))
x2 <- with(countries_wide,
           data.frame(UID = UID, Province_State = Province_State,
                      Country_Region = Country_Region,
                      Combined_Key = Combined_Key))
anti_join(x1, x2, by = "UID")

cruise_ships <- c("Diamond Princess", "MS Zaandam")
unjoined_uids <- anti_join(x2, x1, by = "UID") %>%
  filter(!(Country_Region %in% cruise_ships))

unjoined_latlong <- global_rt_wide[UID %in% unjoined_uids$UID,
                                   .(UID, Combined_Key, Lat, Long_)]
new_points <- make_points(unjoined_latlong, "Combined_Key", st_crs(sf_tiny_use))

sf_world <- rbind(sf_world_temp, new_points) %>%
  select(UID, geometry)

countries_w_states_provinces <- c("US", "Canada", "Australia", "China")
world_merged <- countries_wide %>%
  select(UID, Combined_Key, starts_with("Rt_")) %>%
  mutate(resolution = "country") %>%
  merge(sf_world, ., by = "UID", all = FALSE) %>%
  rename(dispID = Combined_Key)

exported_countries <- data.table(UID = world_merged$UID)
world_rt_long_export <- global_rt_long[exported_countries, on = "UID"] %>%
  mutate(resolution = "country") %>%
  select(UID, dispID = Combined_Key, date, date_lag, resolution,
         starts_with("Rt_"), starts_with("positive"), starts_with("death"))

########################################################################
## Get choices for names
########################################################################

sf_all <- rbind(world_merged, provinces_merged, state_merged, county_merged)
rt_long_all <- rbind(state_rt_long_export, county_rt_long_export,
                     provinces_rt_long_export, world_rt_long_export) %>%
  as_tibble()

state_province_names1 <- sf_all %>%
  filter(startsWith(resolution, "state"),
         !(dispID %in% c("Hong Kong, China", "Macau, China"))) %>%
  select(dispID, UID)
state_province_names2 <- sf_all %>%
  filter(startsWith(resolution, "state"),
         dispID %in% c("Hong Kong, China", "Macau, China")) %>%
  select(dispID, UID)
state_province_names <- rbind(state_province_names1, state_province_names2) %>%
  separate(dispID, into = c("state", "country"), sep = "\\, ",
           remove = FALSE) %>%
  group_by(country) %>%
  arrange(state) %>%
  ungroup() %>%
  arrange(desc(country))

us_state_dt <- state_province_names %>%
  filter(country == "USA") %>%
  select(state, UID)

county_names_unsrt <- sf_all %>%
  filter(resolution == "county") %>%
  select(dispID, UID) %>%
  separate(col = "dispID", into = c("County", "State"),
           sep = ", ", remove = FALSE)
county_ord <- county_names_unsrt$dispID %>%
  strsplit(", ", fixed = TRUE) %>%
  vapply(function(x) { paste(x[2], x[1]) }, "Utah") %>%
  order()
county_names <- county_names_unsrt[county_ord, ]

country_names <- sf_all %>%
  filter(resolution == "country") %>%
  select(dispID, UID) %>%
  arrange(dispID)

us_states_w_counties <- us_state_dt %>%
  filter(state %in% unique(county_names$State), state != "District of Columbia")
stopifnot(nrow(us_states_w_counties) == 50)

names_list <- list(
  state = as.list(state_province_names$UID),
  county = as.list(county_names$UID),
  country = as.list(country_names$UID),
  us_state = as.list(us_state_dt$UID),
  us_states_w_counties = as.list(us_states_w_counties$UID))
names(names_list$state) <- state_province_names$dispID
names(names_list$county) <- county_names$dispID
names(names_list$country) <- country_names$dispID
names(names_list$us_state) <- us_state_dt$state
names(names_list$us_states_w_counties) <- us_states_w_counties$state

names_dt <- data.table(names = c(state_province_names$UID,
                                 county_names$UID,
                                 country_names$UID))
rep_names <- names_dt[, .N, by = names][N > 1, ]
rep_names
stopifnot(nrow(rep_names) == 0)

########################################################################
## Get state centers for plotting
########################################################################

get_lnglat <- function(geometry, UID) {
  bbox <- st_bbox(geometry)
  # do a weighted mean because we want the legend on the right side
  lng <- weighted.mean(c(bbox["xmin"], bbox["xmax"]), c(0.25, 0.75))
  lat <- mean(c(bbox["ymin"], bbox["ymax"]))
  if (UID == "84000002") {
    # Alaska
    lng <- -147
  } else if (UID == "84000015") {
    # Hawaii
    lng <- -154.65
    lat <- 20.09
  }
  return(c(lng, lat))
}

usa_state_counties <- sf_all %>%
  filter(resolution == "state_USA", UID > 12499) %>%
  select(UID, dispID, geometry)

state_centers <- map2(usa_state_counties$geometry, usa_state_counties$UID,
                      get_lnglat)
names(state_centers) <- usa_state_counties$UID

########################################################################
## Save everything
########################################################################

saveRDS(sf_all, "clean_data/sf_all.rds")
saveRDS(rt_long_all, "clean_data/rt_long_all.rds")
saveRDS(names_list, "clean_data/names_list.rds")
saveRDS(state_centers, "clean_data/state_centers.rds")
write_csv(rt_long_all, "clean_data/rt_table_export.csv")
