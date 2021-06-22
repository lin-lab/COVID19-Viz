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
library(stringi)

START_DATE <- ymd("2020-04-01")

reformat_data <- function(dt, start_date) {
  scale_cols <- c("case_rate", "case_lower", "case_upper", "death_rate",
                  "death_lower", "death_upper")
  dt[, (scale_cols) := lapply(.SD, function(x) { x * 1e6 }),
     .SDcols = scale_cols]
  dt[, positive_percapita := 1e6 * positive / population]
  dt[, death_percapita := 1e6 * death / population]
  dt[, positiveIncrease_percapita := 1e6 * positiveIncrease / population]
  dt[, deathIncrease_percapita := 1e6 * deathIncrease / population]
  ret <- dt[date >= start_date]
  return(ret)
}


########################################################################
## State-level data
########################################################################

state_cols <- cols(
  UID = col_double(),
  stateName = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  positiveIncrease = col_double(),
  deathIncrease = col_double(),
  positive = col_double(),
  death = col_double(),
  population = col_double(),
  Lat = col_double(),
  Long_ = col_double(),
  Combined_Key = col_character(),
  rt = col_double(),
  rt_lower = col_double(),
  rt_upper = col_double(),
  case_rate = col_double(),
  case_lower = col_double(),
  case_upper = col_double(),
  death_rate = col_double(),
  death_lower = col_double(),
  death_upper = col_double(),
  date_lag = col_date(format = "%Y-%m-%d")
)


state_rt_long <- read_csv("raw_data/jhu_state_rt_case_death_rate.csv",
                          col_types = state_cols) %>%
  data.table() %>%
  reformat_data(START_DATE)

# read state-level vaccine data
state_vax <- read_csv("../COVID-data-cleaning/cdc_vax_data/govex_COVID-19/data_tables/vaccine_data/us_data/time_series/vaccine_data_us_timeline.csv") %>%
  data.table()

uid_table <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv") %>%
  data.table()

state_pop <- uid_table[Country_Region == "US" & is.na(Admin2) &
                       Combined_Key != "US",
                       .(Combined_Key, Population)]
state_vax <- merge(state_vax, state_pop, by = "Combined_Key", all.x = TRUE,
                   all.y = FALSE)
vax_cols <- c("Stage_One_Doses", "Stage_Two_Doses")
state_vax[, (vax_cols) := lapply(.SD, function(x) { x / Population * 100 }),
          .SDcols = vax_cols]

state_vax_wide <- state_vax %>%
  select(Province_State, Date, Vaccine_Type, Stage_One_Doses, Stage_Two_Doses) %>%
  pivot_wider(id_cols = c(Province_State, Date),
              names_from = Vaccine_Type,
              values_from = c(Stage_One_Doses, Stage_Two_Doses))
state_rt_vax <- left_join(state_rt_long, state_vax_wide,
                          by = c("stateName" = "Province_State", "date" = "Date"))

# Select the state info of the states we want to merge.
state_rt_tomerge <- state_rt_long %>%
  filter(date == max(date)) %>%
  select(UID, stateName)

state_maps <- ne_states(country = "united states of america",
                        returnclass = "sf")
# for JHU, Puerto Rico's UIDs start with 630

pr_maps <- us_counties() %>%
  filter(state_name == "Puerto Rico") %>%
  st_union() %>%
  st_sf() %>%
  mutate(name = "Puerto Rico", UID = 630)

# Add points for American Samoa, Guam, Puerto Rico, etc
jhu_states <- state_rt_tomerge$stateName
add_states <- dplyr::setdiff(jhu_states, c(state_maps$name, "Puerto Rico"))
print(add_states)

unjoined_states <- state_rt_long %>%
  filter(stateName %in% add_states) %>%
  select(stateName, UID, Lat, Long_) %>%
  distinct()

#' Make point geometries from a data frame.
#'
make_points <- function(df, name_col, crs) {
  point_lst <- list()
  for (i in seq_len(nrow(df))) {
    long_cur <- df$Long_[i]
    lat_cur <- df$Lat[i]
    point_lst[[i]] <- st_point(c(long_cur, lat_cur))
  }

  new_points <- st_sf(UID = df$UID,
                      name = df[[name_col]],
                      geometry = st_sfc(point_lst, crs = crs))
  return(new_points)
}

new_state_points <- make_points(unjoined_states, "stateName",
                                st_crs(state_maps))

state_merged <- state_maps %>%
  select(name, geometry, fips) %>%
  mutate(UID = as.integer(paste0("840000", substring(fips, 3)))) %>%
  select(-fips) %>%
  rbind(pr_maps, new_state_points) %>%
  merge(state_rt_tomerge, by = "UID", all.x = FALSE) %>%
  mutate(resolution = "subnat_USA", dispID = paste0(name, ", USA")) %>%
  select(UID, dispID, resolution, starts_with("rt"), starts_with("case_"),
         starts_with("death_"))


exported_states <- with(state_merged, data.table(UID = UID))
state_rt_long_export <- state_rt_vax[exported_states, on = "UID"][,
    resolution := "subnat_USA"]
setnames(state_rt_long_export, old = "Combined_Key", new = "dispID")
# drop unneeded columns
state_rt_long_export[, `:=` (Lat = NULL, Long_ = NULL, stateName = NULL)]

########################################################################
## County-level data
########################################################################

# for JHU, Puerto Rico's UIDs start with 630
county_maps <- us_counties() %>%
  mutate(UID = case_when(state_name == "Puerto Rico" ~ as.integer(paste0("630", geoid)),
                         TRUE ~ as.integer(paste0("840", geoid))))

county_cols <- cols(
  UID = col_double(),
  county = col_character(),
  stateName = col_character(),
  date = col_date(format = "%Y-%m-%d"),
  FIPS = col_character(),
  positiveIncrease = col_integer(),
  deathIncrease = col_integer(),
  positive = col_integer(),
  death = col_double(),
  population = col_double(),
  Combined_Key = col_character(),
  rt = col_double(),
  rt_lower = col_double(),
  rt_upper = col_double(),
  case_rate = col_double(),
  case_lower = col_double(),
  case_upper = col_double(),
  death_rate = col_double(),
  death_lower = col_double(),
  death_upper = col_double(),
  date_lag = col_date(format = "%Y-%m-%d")
)

county_rt_long <- read_csv("raw_data/jhu_county_rt_case_death_rate.csv",
                           col_types = county_cols) %>%
  data.table() %>%
  reformat_data(START_DATE)

# make combined key for county rt long
county_rt_long[, Combined_Key := paste(county, stateName, sep = ", ")]

# read county-level vax data
county_vax_orig <- fread("../cdc-vax-data/vaccine_county_data.csv")
county_vax <- county_vax_orig[Completeness_pct >= 80 & !is.na(Completeness_pct)
                              & FIPS != "UNK", ]
setnames(county_vax, old = "Date", new = "date")
setorderv(county_vax, "date")

county_vax[, Census2019_18PlusPop := Series_Complete_18Plus / Series_Complete_18PlusPop_Pct * 100]
county_vax[, Census2019_65PlusPop := Series_Complete_65Plus / Series_Complete_65PlusPop_Pct * 100]
county_vax[, Census2019_Pop := Series_Complete_Yes / Series_Complete_Pop_Pct * 100]
county_vax[StateName != "Puerto Rico", UID := 84000000 + as.integer(FIPS)]
county_vax[StateName == "Puerto Rico", UID := 63000000 + as.integer(FIPS)]

max_date <- max(county_rt_long$date)
rt_fips <- county_rt_long[date == max_date, .(UID, FIPS, stateName, county)]

vax_fips <- county_vax[date == max_date, .(UID, FIPS, StateName, County)]
rt_vax_fips <- merge(rt_fips, vax_fips, by.x = "UID", by.y = "UID",
                     all = TRUE)

# Fix weird counties.
weird_counties <- county_rt_long[is.na(FIPS),
                                 .(county, stateName, UID)] %>%
  distinct()
weird_counties

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

county_vax_subset <- county_vax[County == "Dukes" | County == "Nantucket", ]
combine_county_vax <- function(county_names, statename) {
  stopifnot(length(county_names) > 1)
  stopifnot(length(statename) == 1)
  county_vax_subset <- county_vax[(County %in% county_names) &
                                  (StateName == statename), ]
  uniq_counties <- uniqueN(county_vax_subset$FIPS)

  # don't combine if only 0-1 counties
  if (uniq_counties == 1) {
    return(county_vax_subset)
  }

  county_name <- county_vax_subset$FIPS
  county_lst <- split(county_vax_subset, by = "FIPS")
  ages <- c("Yes", "12Plus", "18Plus", "65Plus")
  ret_lst <- list()
  for (age in ages) {
    pop_col <- ifelse(age == "Yes", "Census2019_Pop",
                      paste0("Census2019_", age, "Pop"))
    vax_col <- paste0("Series_Complete_", age)
    pop_lst <- lapply(county_lst, function(x) { x[[pop_col]] })
    total_pop <- purrr::reduce(pop_lst, `+`)
    vax_lst <- lapply(county_lst, function(x) { x[[vax_col]]})
    total_vax <- purrr::reduce(vax_lst, `+`)
    ret_lst[[vax_col]] <- total_vax
    vax_col_name <- ifelse(age == "Yes", "Series_Complete_Pop_Pct",
                           paste0(vax_col, "Pop_Pct"))
    ret_lst[[vax_col_name]] <- 100 * total_vax / total_pop
  }
  ret <- data.table(do.call(cbind, ret_lst))
  ret[, date := county_lst[[1]]$date]
  return(ret)
}

# join Dukes and Nantucket in MA
dukes_nantucket <- county_maps %>%
  filter(name %in% c("Dukes", "Nantucket"),
         state_name == "Massachusetts")
stopifnot(nrow(dukes_nantucket) == 2)
sf_dukes_nantucket <- st_sf(UID = 84070002,
                            geometry = st_union(dukes_nantucket))
county_rt_long <- county_rt_long[!(county %in% c("Dukes", "Nantucket") &
                                    stateName == "Massachusetts")]
vax_dukes_nantucket <- combine_county_vax(c("Dukes", "Nantucket"),
                                          "Massachusetts")
vax_dukes_nantucket[, UID := 84070002]
county_vax <- county_vax[!(County %in% c("Dukes", "Nantucket") &
                           StateName == "Massachusetts"), ]

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

# create a combined NYC county and remove individual boroughs
vax_nyc <- combine_county_vax(nyc_counties, "New York")
vax_nyc[, UID := 84036061]
county_vax <- county_vax[!(County %in% nyc_counties & StateName == "New York"), ]

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
utah_vax_lst <- list()
for (i in seq_along(utah_join)) {
  join_counties <- utah_sf_orig %>%
    filter(name %in% utah_join[[i]]) %>%
    select(geometry)
  new_geometries[[i]] <- st_union(join_counties)
  utah_uids[[i]] <- county_rt_long %>%
    filter(stateName == "Utah", date == max_date,
           county == names(utah_join)[i]) %>%
    use_series(UID)

  vax_cur <- combine_county_vax(utah_join[[i]], "Utah")
  vax_cur$UID <- utah_uids[[i]]
  utah_vax_lst[[i]] <- vax_cur
}

sf_utah <- st_sf(UID = unlist(utah_uids),
                 geometry = st_sfc(unlist(new_geometries,
                                          recursive = FALSE),
                                   crs = st_crs(county_maps)))
utah_counties_remove <- unlist(utah_join)
county_rt_long <- county_rt_long[!(county %in% utah_counties_remove &
                                   stateName == "Utah")]

vax_utah <- do.call(rbind, utah_vax_lst)
county_vax <- county_vax[!(County %in% utah_counties_remove & StateName == "Utah"), ]

# get one county per row
county_rt_wide <- county_rt_long %>%
  filter(date == max_date) %>%
  select(county, stateName, UID, FIPS, date, starts_with("rt"),
         starts_with("case_"), starts_with("death_"), Combined_Key)

county_maps_new <- county_maps %>%
  select(UID, geometry) %>%
  rbind(sf_dukes_nantucket, sf_nyc, sf_kc, sf_utah)

county_vax_final <- county_vax %>%
  select(starts_with("Series_Complete"), date, UID) %>%
  select(!ends_with("SVI")) %>%
  rbind(vax_dukes_nantucket, vax_nyc, vax_utah)


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
  select(UID, dispID = Combined_Key, resolution, starts_with("rt"),
         starts_with("case_"), starts_with("death_"))

exported_counties <- data.table(UID = county_merged$UID)

county_rt_long_export <- county_rt_long[exported_counties, on = "UID"]
county_rt_long_export[, resolution := "county"]

county_final <- merge(county_rt_long_export, county_vax_final,
                      by = c("date", "UID"), all.x = TRUE)

# drop unneeded columns
county_final[, `:=` (FIPS = NULL, stateName = NULL, county = NULL)]
setnames(county_final, old = "Combined_Key", new = "dispID")


########################################################################
## International data
########################################################################

global_cols <- cols(
  UID = col_double(),
  Province_State = col_character(),
  Country_Region = col_character(),
  positive = col_integer(),
  date = col_date(format = "%Y-%m-%d"),
  death = col_integer(),
  positiveIncrease = col_integer(),
  deathIncrease = col_integer(),
  iso2 = col_character(),
  iso3 = col_character(),
  code3 = col_double(),
  FIPS = col_integer(),
  Admin2 = col_character(),
  Lat = col_double(),
  Long_ = col_double(),
  Combined_Key = col_character(),
  population = col_double(),
  rt = col_double(),
  rt_lower = col_double(),
  rt_upper = col_double(),
  case_rate = col_double(),
  case_lower = col_double(),
  case_upper = col_double(),
  death_rate = col_double(),
  death_lower = col_double(),
  death_upper = col_double(),
  date_lag = col_date(format = "%Y-%m-%d")
)

global_rt_long <- read_csv("raw_data/jhu_global_rt_case_death_rate.csv",
                           col_types = global_cols) %>%
  data.table() %>%
  reformat_data(START_DATE)

global_rt_long[UID == 840, Country_Region := "United States"]
global_rt_long[UID == 840, Combined_Key := "United States"]

global_rt_long[Country_Region == "Canada", ]

# fix a typo
global_rt_long[UID == 12406,
               Combined_Key := "Northwest Territories, Canada"]

global_rt_wide <- global_rt_long %>%
  select(UID, Province_State, Country_Region, Lat, Long_, Combined_Key, date,
         starts_with("rt"), starts_with("case_"), starts_with("death_")) %>%
  pivot_wider(id_cols = UID:Combined_Key,
              names_from = date,
              values_from = c(starts_with("rt"), starts_with("case_"),
                              starts_with("death_"))) %>%
  data.table()
stopifnot(uniqueN(global_rt_wide$UID) == nrow(global_rt_wide))

########################################################################
## Take care of provinces
########################################################################
countries_w_provinces <- c("Canada", "Australia", "China")
province_uids <- global_rt_wide[Province_State != "" &
                                 Country_Region %in% countries_w_provinces,
                                UID]

provinces_wide <- global_rt_wide[UID %in% province_uids]
countries_wide <- global_rt_wide[!(UID %in% province_uids)]

provinces_sf_orig <- ne_states(geounit = c("Canada", "Australia", "China"),
                          returnclass = "sf")
x <- provinces_sf_orig %>%
  filter(admin == "China")
x$name_en
x[19, ]
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
  mutate(resolution = paste0("subnat_", Country_Region)) %>%
  select(UID, dispID, resolution, starts_with("rt"), starts_with("case_"),
         starts_with("death_"))

exported_provinces <- data.table(UID = provinces_merged$UID)
provinces_rt_long_export <- global_rt_long[exported_provinces, on = "UID"] %>%
  mutate(resolution = paste0("subnat_", Country_Region)) %>%
  select(UID, dispID = Combined_Key, date, date_lag, resolution, population,
         starts_with("rt"), starts_with("positive"), starts_with("death"),
         starts_with("case")) %>%
  data.table()

########################################################################
## Additional subnational data
########################################################################
subnat_cols <- cols(
  UID = col_integer(),
  positive = col_integer(),
  death = col_integer(),
  date = col_date(format = "%Y-%m-%d"),
  Combined_Key = col_character(),
  iso2 = col_character(),
  iso3 = col_character(),
  code3 = col_double(),
  FIPS = col_logical(),
  Admin2 = col_logical(),
  Province_State = col_character(),
  Country_Region = col_character(),
  Lat = col_double(),
  Long_ = col_double(),
  population = col_integer(),
  deathIncrease = col_integer(),
  positiveIncrease = col_integer(),
  rt = col_double(),
  rt_lower = col_double(),
  rt_upper = col_double(),
  case_rate = col_double(),
  case_lower = col_double(),
  case_upper = col_double(),
  death_rate = col_double(),
  death_lower = col_double(),
  death_upper = col_double(),
  date_lag = col_date(format = "%Y-%m-%d")
)

subnat_rt_long <- read_csv("raw_data/jhu_subnational_rt_case_death_rate.csv",
                           col_types = subnat_cols) %>%
  # some plcae was duplicated a lot on a single day
  distinct() %>%
  data.table() %>%
  reformat_data(START_DATE)

stopifnot(identical(anyDuplicated(subnat_rt_long), 0L))

subnat_rt_wide <- subnat_rt_long %>%
  select(UID, Province_State, Country_Region, Lat, Long_, Combined_Key, date,
         starts_with("rt"), starts_with("case_"), starts_with("death_")) %>%
  pivot_wider(id_cols = UID:Combined_Key,
              names_from = date,
              values_from = c(starts_with("rt"), starts_with("case_"),
                              starts_with("death_")))

uniq_countries <- unique(subnat_rt_long$Country_Region)
# Handle UK, Chile, and India separately
sep_countries <- c("United Kingdom", "India")
select_countries <- uniq_countries[!(uniq_countries %in% sep_countries)]
subnat_sf_orig <- ne_states(country = select_countries, returnclass = "sf") %>%
  st_make_valid()

# For UK, we only have England, Scotland, Wales, and Northern Ireland
uk_sf <- ne_countries(country = "united kingdom", type = "map_units",
                      returnclass = "sf") %>%
  mutate(Combined_Key = paste0(name_long, ", United Kingdom"))

sf_lst <- list()
for (country in uniq_countries) {
  if (country %in% sep_countries) {
    next
  }
  country_sf <- subnat_sf_orig %>%
    filter(admin == country)
  if (country == "Spain" || country == "Italy") {
    country_sf <- country_sf %>%
      select(region) %>%
      group_by(region) %>%
      summarize(geometry = st_union(geometry)) %>%
      rename(name_en = region) %>%
      mutate(admin = country, woe_name = name_en)
  }
  if (country == "Germany" || country == "Netherlands" || country == "Mexico") {
    tmp <- country_sf %>%
      mutate(name_noacc = stri_trans_general(name, "latin-ascii"))
  } else {
    tmp <- country_sf %>%
      mutate(name_noacc = stri_trans_general(name_en, "latin-ascii"))
  }

  # be aware of spaces. Spaces after means this is a prefix, spaces before means
  # it's a postfix
  subnat_patt <- c("Province of " = "", "Community of " = "", " Province" = "",
                  " Department" = "", " Prefecture" = "", " Region" = "",
                  " County" = "", "Republic of " = "")
  final_sf <- tmp %>%
    mutate(name_clean = str_replace_all(name_noacc, subnat_patt),
           Combined_Key = paste0(name_clean, ", ", admin)) %>%
    select(Combined_Key, geometry, woe_name)

  if (country == "Chile") {
    final_sf <- final_sf %>%
      filter(!(Combined_Key %in% c("Bio Bio, Chile", "Maule, Chile")))
  }
  sf_lst[[country]] <- final_sf
}
sf_lst[["united kingdom"]] <- select(uk_sf, Combined_Key, geometry, woe_name = subunit)

# For India, there was a new province created in 2014 that isn't updated on
# rnaturalearth
india_simplified_fname <- "./ext-data/india_sf_simplified.rds"
if (!file.exists(india_simplified_fname)) {
  library(rmapshaper)
  message("Simplifying India geometry...")
  india_subnat_orig <- readRDS("./ext-data/gadm36_IND_1_sf.rds")
  india_simplified <- india_subnat_orig %>%
    ms_simplify()
  saveRDS(india_simplified, file = india_simplified_fname)
} else {
  india_simplified <- readRDS(india_simplified_fname)
}

india_subnat <- india_simplified %>%
  mutate(Combined_Key = paste0(NAME_1, ", ", NAME_0)) %>%
  select(Combined_Key, geometry, woe_name = NAME_1)
sf_lst[["India"]] <- india_subnat

chile_subnat <- readRDS("./ext-data/gadm36_CHL_1_sf.rds") %>%
  mutate(woe_name = stri_trans_general(NAME_1, "latin-ascii"),
         Combined_Key = paste0(woe_name, ", ", NAME_0)) %>%
  select(Combined_Key, geometry, woe_name) %>%
  filter(woe_name %in% c("Bio-Bio", "Maule", "Nuble"))
sf_lst[["Chile2"]] <- chile_subnat

subnat_sf <- do.call(rbind, sf_lst)
rownames(subnat_sf) <- NULL

#relabel subnational units to align rt/map dfs
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Federal District, Brazil"] ="Distrito Federal, Brazil"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Bio-Bio, Chile"] ="Biobio, Chile"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Khmelnytsky Oblast, Ukraine"] ="Khmelnytskyi Oblast, Ukraine"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Zaporizhzhya Oblast, Ukraine"] ="Zaporizhia Oblast, Ukraine"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Sodermanland, Sweden"] ="Sormland, Sweden"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Islamabad Capital Territory, Pakistan"] ="Islamabad, Pakistan"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Azad Kashmir, Pakistan"] ="Azad Jammu and Kashmir, Pakistan"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Tumbes region, Peru"] ="Tumbes, Peru"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Canary Is., Spain"] ="Canarias, Spain"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Andalucia, Spain"] ="Andalusia, Spain"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Islas Baleares, Spain"] ="Baleares, Spain"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Foral de Navarra, Spain"] ="Navarra, Spain"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Castilla-La Mancha, Spain"] ="Castilla - La Mancha, Spain"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Cataluna, Spain"] ="Catalonia, Spain"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Valenciana, Spain"] ="C. Valenciana, Spain"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Jamtland, Sweden"] ="Jamtland Harjedalen, Sweden"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Archipelago of Saint Andrews, Colombia"] ="San Andres y Providencia, Colombia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key == "Magallanes y la Antartica Chilena, Chile"] <- "Magallanes, Chile"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Bogota, Colombia"] ="Capital District, Colombia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Sicily, Italy"] ="Sicilia, Italy"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Santiago Metropolitan, Chile"] ="Metropolitana, Chile"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Libertador General Bernardo O'Higgins, Chile"] ="OHiggins, Chile"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Apulia, Italy"] ="Puglia, Italy"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Friuli-Venezia Giulia, Italy"] ="Friuli Venezia Giulia, Italy"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Trentino-Alto Adige, Italy"] ="P.A. Trento, Italy"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Distrito Federal, Mexico"] ="Ciudad de Mexico, Mexico"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Dagestan, Russia"] ="Dagestan Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Oryol Oblast, Russia"] ="Orel Oblast, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Sakha Republic, Russia"] ="Sakha (Yakutiya) Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Kalmykia, Russia"] ="Kalmykia Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Bashkortostan, Russia"] ="Bashkortostan Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Mordovia, Russia"] ="Mordovia Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Ingushetia, Russia"] ="Ingushetia Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Karelia, Russia"] ="Karelia Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Buryatia, Russia"] ="Buryatia Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Jewish Autonomous Oblast, Russia"] ="Jewish Autonomous Okrug, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Khakassia, Russia"] ="Khakassia Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Tatarstan, Russia"] ="Tatarstan Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Chuvash Republic, Russia"] ="Chuvashia Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="North Ossetia-Alania, Russia"] ="North Ossetia - Alania Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Kabardino-Balkar Republic, Russia"] ="Kabardino-Balkarian Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Tuva Republic, Russia"] ="Tyva Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Adygea, Russia"] ="Adygea Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Autonomous Crimea, Russia"] ="Crimea Republic*, Ukraine"
subnat_sf$Combined_Key[subnat_sf$Combined_Key=="Sevastopol, Russia"] ="Sevastopol*, Ukraine"
subnat_sf$Combined_Key[subnat_sf$Combined_Key == "Brussels-Capital, Belgium"] <- "Brussels, Belgium"
subnat_sf$Combined_Key[subnat_sf$Combined_Key == "Luxembourg District, Belgium"] <- "Luxembourg, Belgium"
subnat_sf$Combined_Key[subnat_sf$woe_name == "Kiev"] <- "Kiev Oblast, Ukraine"
subnat_sf$Combined_Key[subnat_sf$woe_name == "Kiev City Municipality"] <- "Kiev, Ukraine"
subnat_sf$Combined_Key[subnat_sf$woe_name == "Gorno-Altay"] <- "Altai Republic, Russia"
subnat_sf$Combined_Key[subnat_sf$woe_name == "Altay"] <- "Altai Krai, Russia"
subnat_sf$Combined_Key[subnat_sf$Combined_Key == "NCT of Delhi, India"] <- "Delhi, India"
subnat_sf$Combined_Key[subnat_sf$Combined_Key == "Andaman and Nicobar, India"] <- "Andaman and Nicobar Islands, India"

subnat_sf$Combined_Key[subnat_sf$Combined_Key == "Dadra and Nagar Haveli, India"] <- "Dadra and Nagar Haveli and Daman and Diu, India"
subnat_sf$Combined_Key[subnat_sf$Combined_Key == "Daman and Diu, India"] <- "Dadra and Nagar Haveli and Daman and Diu, India"

valid <- subnat_sf %>%
  st_is_valid()
all(valid)

repeated_subnat <- subnat_sf %>%
  group_by(Combined_Key) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>%
  arrange(Combined_Key) %>%
  pull(Combined_Key)

stopifnot(repeated_subnat == c("Dadra and Nagar Haveli and Daman and Diu, India", "Lima, Peru"))

# join together two versions of Lima, Peru (one is the city itself, one is the
# area surrounding the city) and join two Indian provinces
subnat_sf <- subnat_sf %>%
  group_by(Combined_Key) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_as_sf(sf_column_name = "geometry")

subnat_merged <- subnat_rt_wide %>%
  merge(subnat_sf, by = "Combined_Key", all.x = FALSE) %>%
  mutate(resolution = paste0("subnat_", Country_Region), dispID = Combined_Key) %>%
                              select(UID, dispID, resolution, starts_with("rt"), starts_with("case_"),
                              starts_with("death_"), geometry)

exported_subnats <- data.table(UID = subnat_merged$UID)
subnat_rt_long_export <- subnat_rt_long[exported_subnats, on = "UID"] %>%
  mutate(resolution = paste0("subnat_", Country_Region)) %>%
  select(UID, dispID = Combined_Key, date, date_lag, resolution, population,
         starts_with("rt"), starts_with("positive"), starts_with("death"),
         starts_with("case")) %>%
  data.table()

subnat_rt_test <- subnat_rt_wide %>%
  select(Combined_Key, UID, Province_State, Country_Region)

# provinces with no maps
cat("Provinces with no map data:\n")
subnat_rt_test %>%
  anti_join(subnat_sf, by = "Combined_Key") %>%
  arrange(Country_Region)

# maps with no COVID data
cat("Maps with no COVID data:\n")
subnat_sf %>%
  select(Combined_Key) %>%
  st_drop_geometry() %>%
  anti_join(subnat_rt_test, by = "Combined_Key")

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
tiny_joined <- inner_join(world_countries_temp, tiny_countries_temp,
                          by = "iso_n3")

# remove tiny countries that are repeated in non-tiny countries
remove_tiny_iso <- tiny_joined %>%
  filter(!is.na(iso_n3)) %>%
  pull(iso_n3)
sf_tiny_use <- sf_tiny_countries %>%
  filter(!(iso_n3 %in% remove_tiny_iso)) %>%
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

world_merged <- countries_wide %>%
  select(UID, Combined_Key, starts_with("rt_"), starts_with("case_"),
         starts_with("death_")) %>%
  mutate(resolution = "country") %>%
  merge(sf_world, ., by = "UID", all = FALSE) %>%
  rename(dispID = Combined_Key)

exported_countries <- data.table(UID = world_merged$UID)
world_rt_long_export <- global_rt_long[exported_countries, on = "UID"] %>%
  mutate(resolution = "country") %>%
  select(UID, dispID = Combined_Key, date, date_lag, resolution, population,
         starts_with("rt"), starts_with("positive"), starts_with("death"),
         starts_with("case")) %>%
  data.table()

########################################################################
## Get choices for names
########################################################################

# use fill; not all dates available in subnational data
sf_all <- bind_rows(world_merged, provinces_merged, subnat_merged, state_merged, county_merged)
rt_long_all <- rbind(world_rt_long_export, provinces_rt_long_export, subnat_rt_long_export,
                     state_rt_long_export, county_final, fill = TRUE)

state_province_names1 <- sf_all %>%
  filter(startsWith(resolution, "subnat"),
         !(dispID %in% c("Hong Kong, China", "Macau, China"))) %>%
  select(dispID, UID)
state_province_names2 <- sf_all %>%
  filter(startsWith(resolution, "subnat"),
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
stopifnot(nrow(us_states_w_counties) == 51)

names_list <- list(
  subnat = as.list(state_province_names$UID),
  county = as.list(county_names$UID),
  country = as.list(country_names$UID),
  us_state = as.list(us_state_dt$UID),
  us_states_w_counties = as.list(us_states_w_counties$UID))
names(names_list$subnat) <- state_province_names$dispID
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

sf_all <- sf_all %>%
  select(UID)

########################################################################
## Save everything
########################################################################

out_dir = "clean_data_pois"
saveRDS(sf_all, file.path(out_dir, "sf_all.rds"))
saveRDS(rt_long_all, file.path(out_dir, "rt_long_all.rds"))
saveRDS(names_list, file.path(out_dir, "names_list.rds"))
fwrite(rt_long_all, file.path(out_dir, "rt_table_export.csv"))
