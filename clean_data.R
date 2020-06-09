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

#'
#'
lag_and_subset <- function(dt, group_var = "UID", nlag = 5,
                           window_size = 7, pos_cutoff = 50,
                           posincr_cutoff = 10,
                           start_date = ymd("2020-03-19")) {
  setnames(dt, old = "date", new = "date_orig")
  dt[, date := date_orig - days(nlag)]
  dt[positiveIncrease <= 0, posIncr_trunc := 0]
  dt[positiveIncrease > 0, posIncr_trunc := positiveIncrease]
  dt[, rolling_posIncr := frollmean(posIncr_trunc, n = window_size,
                                    align = "right", algo = "exact"),
      by = group_var]
  ret <- dt[date >= start_date & !is.na(rolling_posIncr)]
  ret[positive >= pos_cutoff & rolling_posIncr >= posincr_cutoff,
      `:=` (Rt_plot = mean_rt, Rt_upr = ci_upper, Rt_lwr = ci_lower)]
  ret[positive >= pos_cutoff & rolling_posIncr < posincr_cutoff,
      Rt_plot := -88]
  ret[positive < pos_cutoff, Rt_plot := -888]
  return(ret)
}

########################################################################
## State-level data
########################################################################

state_rt_long_orig <- read_tsv("raw_data/jhu_state_rt.tsv") %>%
  data.table()

state_rt_long <- lag_and_subset(state_rt_long_orig, "stateName")

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
  select(stateName, date, Rt_plot, Rt_upr, Rt_lwr) %>%
  pivot_wider(id_cols = c(stateName), names_from = date,
              values_from = c(Rt_plot, Rt_upr, Rt_lwr))

state_maps <- ne_states(country = "united states of america",
                        returnclass = "sf")
state_merged <- state_maps %>%
  select(name, geometry, fips) %>%
  mutate(UID = as.integer(paste0("840000", substring(fips, 3))),
         resolution = "state_USA_Canada", dispID = paste0(name, ", USA")) %>%
  select(-fips) %>%
  merge(state_rt_tomerge, by.x = "name",
                      by.y = "stateName", all.x = FALSE) %>%
  select(UID, dispID, resolution, starts_with("Rt_"))

exported_states <-
  with(state_merged,
       data.table(UID = UID, stateName = substr(dispID, 1, nchar(dispID) - 5)))
state_rt_long_export <- state_rt_long[exported_states, on = "stateName"] %>%
  mutate(resolution = "state_USA_Canada",
         dispID = paste0(stateName, ", USA")) %>%
  select(UID, dispID, date, resolution,
         starts_with("Rt_"), starts_with("positive"), starts_with("death"))

########################################################################
## County-level data
########################################################################

county_maps <- us_counties() %>%
  mutate(UID = as.integer(paste0("840", geoid)))
county_rt_long_orig <- read_tsv("raw_data/jhu_county_rt.tsv",
                                guess_max = 10000) %>%
  data.table()

county_rt_long <- lag_and_subset(county_rt_long_orig)
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
  select(county, stateName, UID, FIPS, date, Rt_plot, Rt_upr, Rt_lwr,
         Combined_Key) %>%
  pivot_wider(id_cols = c(county, stateName, UID, FIPS, Combined_Key),
              names_from = date,
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
  select(UID, dispID = Combined_Key, date, resolution,
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

global_rt_long <- lag_and_subset(global_rt_long_orig)
stopifnot(all(global_rt_long$Rt_upr >= global_rt_long$Rt_lwr, na.rm = TRUE))

global_rt_wide <- global_rt_long %>%
  select(-Population, -interval_start, -FIPS, -Admin2, -positiveIncrease,
         -deathIncrease, -positive, -positiveIncrease, -death) %>%
  pivot_wider(id_cols = c(UID, Province_State:Combined_Key),
              names_from = date,
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
  select(name, admin, woe_label, geometry) %>%
  group_by(admin) %>%
  arrange(name) %>%
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
macau_hk <- global_rt_wide[Province_State %in% c("Macau SAR", "Hong Kong SAR")]
stopifnot(nrow(macau_hk) == 2)

point_lst <- list()
for (i in 1:nrow(macau_hk)) {
  uid_cur <- macau_hk$UID[i]
  lat_cur <- macau_hk$Lat[i]
  long_cur <- macau_hk$Long_[i]

  point_lst[[i]] <- st_point(c(long_cur, lat_cur))
}

macau_hk_sf <- st_sf(UID = macau_hk$UID,
                     geometry = st_sfc(point_lst,
                                       crs = st_crs(provinces_sf)))

provinces_sf_final <- provinces_sf %>%
  select(UID, geometry) %>%
  rbind(macau_hk_sf, .)

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
    Country_Region == "Canada" ~ "state_USA_Canada",
    Country_Region == "China" ~ "state_China",
    Country_Region == "Australia" ~ "state_Australia")) %>%
  select(UID, dispID, resolution, starts_with("Rt_"))

exported_provinces <- data.table(UID = provinces_merged$UID)
provinces_rt_long_export <- global_rt_long[exported_provinces, on = "UID"] %>%
  mutate(resolution = case_when(
    Country_Region == "Canada" ~ "state_USA_Canada",
    Country_Region == "China" ~ "state_China",
    Country_Region == "Australia" ~ "state_Australia")) %>%
  select(UID, dispID = Combined_Key, date, resolution,
         starts_with("Rt_"), starts_with("positive"), starts_with("death"))

########################################################################
## International countries
########################################################################

sf_tiny_countries <- ne_countries(type = "tiny_countries",
                                  returnclass = "sf")
sf_world_orig <- ne_countries(returnclass = "sf")

# ignore northern cyprus and somaliland but keep kosovo
sf_world_orig %>%
  filter(is.na(iso_n3)) %>%
  select(admin)
sf_world_use <- sf_world_orig %>%
  mutate(
    UID = case_when(
      admin == "Kosovo" ~ 383L,
      TRUE ~ as.integer(iso_n3)
    )
  ) %>%
  select(UID, name, geometry)

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
point_lst <- list()
for (i in 1:nrow(unjoined_latlong)) {
  uid_cur <- unjoined_latlong$UID[i]
  lat_cur <- unjoined_latlong$Lat[i]
  long_cur <- unjoined_latlong$Long_[i]

  point_lst[[i]] <- st_point(c(long_cur, lat_cur))
}

new_points <- st_sf(UID = unjoined_latlong$UID,
                    name = unjoined_latlong$Combined_Key,
                    geometry = st_sfc(point_lst,
                                      crs = st_crs(sf_tiny_use)))

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
  select(UID, dispID = Combined_Key, date, resolution,
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
  select(dispID, UID)
county_ord <- county_names_unsrt$dispID %>%
  strsplit(", ", fixed = TRUE) %>%
  vapply(function(x) { paste(x[2], x[1]) }, "Utah") %>%
  order()
county_names <- county_names_unsrt[county_ord, ]

country_names <- sf_all %>%
  filter(resolution == "country") %>%
  select(dispID, UID) %>%
  arrange(dispID)

names_list <- list(
  state = as.list(state_province_names$UID),
  county = as.list(county_names$UID),
  country = as.list(country_names$UID),
  us_state = as.list(us_state_dt$UID))
names(names_list$state) <- state_province_names$dispID
names(names_list$county) <- county_names$dispID
names(names_list$country) <- country_names$dispID
names(names_list$us_state) <- us_state_dt$state

names_dt <- data.table(names = c(state_province_names$dispID,
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
  lng <- mean(c(bbox["xmin"], bbox["xmax"]))
  lat <- mean(c(bbox["ymin"], bbox["ymax"]))
  if (UID == "84000002") {
    lng <- -147
  }
  return(c(lng, lat))
}


usa_counties <- sf_all %>%
  filter(resolution == "state_USA_Canada", UID > 12499) %>%
  select(UID, dispID, geometry)

state_centers <- map2(usa_counties$geometry, usa_counties$UID, get_lnglat)
names(state_centers) <- usa_counties$UID

########################################################################
## Save everything
########################################################################

saveRDS(sf_all, "clean_data/sf_all.rds")
saveRDS(rt_long_all, "clean_data/rt_long_all.rds")
saveRDS(names_list, "clean_data/names_list.rds")
saveRDS(state_centers, "clean_data/state_centers.rds")
