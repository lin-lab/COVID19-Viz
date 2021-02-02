#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(jsonlite)
library(httr)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(sf)
library(DT)
library(digest)
library(ggplot2)
library(cowplot)
library(viridis)
library(htmltools)
library(purrr)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(shinyStore)

########################################################################
## Load data files
########################################################################

BASE_URL <- "https://hsph-covid-study.s3.us-east-2.amazonaws.com/website_files_pois"
BASE_PATH <- "clean_data_pois"
IP_API_KEY <- Sys.getenv("IPSTACK_API_KEY")

#' Read a specified file locally if it exists, else read from AWS
#'
#' When the site is live, we basically are only going to read from AWS. I wrote
#' this function because when I was testing the app locally, it was annoying to
#' constantly fetch data from AWS instead of using local data.
read_aws_or_local <- function(fname, base_url = BASE_URL,
                              base_path = BASE_PATH) {
  stopifnot(endsWith(fname, ".rds"))
  local_fname <- file.path(base_path, fname)
  if (file.exists(local_fname)) {
    ret <- readRDS(local_fname)
  } else {
    url <- sprintf("%s/%s", base_url, fname)
    ret <- readRDS(url(url))
  }
  return(ret)
}

# shape file: wide data that has 1 row per location with all Rts, Rt CI's, and
# shapes
sf_all <- read_aws_or_local("sf_all.rds")

# long data frame of Rts
rt_long_all <- read_aws_or_local("rt_long_all.rds")
setkey(rt_long_all, UID)

# make sure we picked up the right one
stopifnot("case_rate" %in% colnames(rt_long_all))

# get range of possible dates
date_real_range <- range(rt_long_all$date)
date_lag_range <- range(rt_long_all$date_lag)
lag_rt <- as.integer(difftime(date_real_range[2], date_lag_range[2]),
                     units = "days")
# create rt lag
setorderv(rt_long_all, cols = c("UID", "date"))
rt_long_all[, `:=` (rt_lag = shift(rt, n = lag_rt, fill = NA, type = "lead"),
                    rt_lower_lag = shift(rt_lower, n = lag_rt, fill = NA,
                                         type = "lead"),
                    rt_upper_lag = shift(rt_upper, n = lag_rt, fill = NA,
                                         type = "lead")), by = UID]

# choices for each place
place_choices <- read_aws_or_local("names_list.rds")

# state centers
state_centers <- read_aws_or_local("state_centers.rds")

# map state UIDs to place name
state_uid_to_place <- as.list(names(place_choices$us_state))
names(state_uid_to_place) <- unlist(place_choices$us_state, use.names = FALSE)

########################################################################
## Define globals
########################################################################

# bins and colors for the map
bins_rt <- c(0, 0.5, 0.75, 1.0, 1.25, 1.5, 2, Inf)
bins_cases <- c(0, 50, 100, 250, 500, 750, 1000, Inf)
bins_deaths <- c(0, 1, 2, 5, 10, 25, 50, Inf)
colors_rt <- rev(brewer.pal(7, "RdYlBu"))
colors_rt <- viridis(7)
#colors_cases <- brewer.pal(7, "YlOrRd")
colors_cases <- viridis(7)

cases_color_labels <- c("0 - 50", "50 - 100", "100 - 250", "250 - 500",
                        "500 - 750", "750 - 1000", "1000+")
deaths_color_labels <- c("0 - 1", "1 - 2", "2 - 5", "5 - 10", "10 - 25",
                         "25 - 50", "50+")
rt_color_labels <- c("0.00 - 0.50", "0.50 - 0.75", "0.75 - 1.00", "1.00 - 1.25",
                     "1.25 - 1.50", "1.50 - 2.00", "2+")
pal_rt <- purrr::partial(colorBin, palette = colors_rt, bins = bins_rt)
pal_cases <- purrr::partial(colorBin, palette = colors_cases,
                            bins = bins_cases)
pal_deaths <- purrr::partial(colorBin, palette = colors_cases,
                             bins = bins_deaths)
pal_lst <- list(rt = pal_rt, case = pal_cases, death = pal_deaths)

# set up defaults for adding stuff to leaflet maps
addPolygons_default <-
  purrr::partial(addPolygons, opacity = 1, weight = 0.5, color = "white",
                dashArray = "3", fillOpacity = 0.7,
                highlight = highlightOptions(
                  weight = 5, color = "#666", dashArray = "",
                  fillOpacity = 0.7, bringToFront = TRUE),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px", direction = "auto"))

addCircles_default <-
  purrr::partial(addCircles, opacity = 1, weight = 1, radius = 17000,
                dashArray = "3", fillOpacity = 0.7, stroke = TRUE,
                color = "white",
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px", direction = "auto"),
                highlightOptions = highlightOptions(
                  weight = 5, color = "#666", dashArray = "",
                  fillOpacity = 0.7, bringToFront = TRUE))


country_uids <- sf_all %>%
  filter(resolution == "country") %>%
  pull(UID)

table_col_choices <-
  list("Rt" = "rt_lag",
       "Rt (lwr)" = "rt_lower_lag",
       "Rt (upr)" =  "rt_upper_lag",
       "Case rate" = "case_rate",
       "Case rate (lwr)" = "case_lower",
       "Case rate (upr)" = "case_upper",
       "Death rate" = "death_rate",
       "Death rate (lwr)" = "death_lower",
       "Death rate (upr)" = "death_upper",
       "Cum. cases" = "positive",
       "Daily new cases" = "positiveIncrease",
       "Cum. cases per million" = "positive_percapita",
       "Daily new cases per million" = "positiveIncrease_percapita",
       "Cum. deaths" = "death",
       "Daily new deaths" = "deathIncrease",
       "Cum. deaths per million" = "death_percapita",
       "Daily new deaths per million" = "deathIncrease_percapita",
       "Population" = "population")


resolution_uniq <- unique(rt_long_all$resolution)
resolution_names <- data.table(orig = resolution_uniq)
resolution_names[, c("resolution", "country") := tstrsplit(orig, "_", fixed = TRUE)]
resolution_names[, pretty := fcase(orig == "country", "World",
                                   orig == "subnat_USA", "US States",
                                   orig == "county", "All US Counties",
                                   startsWith(orig, "subnat"),
                                   paste0(country, " Subnational"))]
setorderv(resolution_names, cols = "country")

resolution_choices <- list("Auto-detected" = "auto", "World" = "country")
subnat_choices <- as.list(resolution_names[resolution == "subnat", orig])
names(subnat_choices) <- resolution_names[resolution == "subnat", pretty]
resolution_choices$Subnational <- subnat_choices
resolution_choices$County <- c(place_choices$us_states_w_counties, list(`All US Counties` = "county"))

# omitting all us counties. We won't draw forest plots / heatmaps for all US
# counties.
resolution_choices_noallus <- list(World = "country",
                                   Subnational = subnat_choices,
                                   County = place_choices$us_states_w_counties)

# This javascript callback re-numbers DT::datatables rows after sorting.
# Source: https://stackoverflow.com/questions/35502931/automatic-row-numbers-after-filtering-dt-in-shiny
dt_js_callback = JS("table.on( 'order.dt search.dt', function () {
                                table.column(0, {search:'applied', order:'applied'}).nodes().each( function (cell, i) {
                                      cell.innerHTML = i+1;});}).draw();")

########################################################################
## Define helper functions
########################################################################

#' Query IP Address for geolocation
query_ip <- function(ip, key = IP_API_KEY) {
  message(sprintf("Querying ip address %s...", ip))
  res <- GET("http://api.ipstack.com", path = ip,
             query = list(access_key = key))
  ret <- list(latitude = -999, longitude = -999, place_str = "Not Detected",
              ipaddr = ip)
  if (res$status == 200) {
    dat <- fromJSON(rawToChar(res$content))
    if (!is.null(dat$latitude) && !is.null(dat$longitude)) {
      ret$latitude <- dat$latitude
      ret$longitude <- dat$longitude
      ret$place_str <- with(dat, paste(city, region_name, country_name,
                                       sep = ", "))
    }
  }
  return(ret)
}

#' Generate labels for Rt, case rate, or death rate.
#'
rate_labeller <- function(sf_dat, metric = c("rt", "case", "death")) {
  stopifnot(names(sf_dat) == c("estimate", "ci_lower", "ci_upper", "UID",
                               "dispID", "geometry"))
  metric <- match.arg(metric)
  metric_col <- sf_dat$estimate
  insufficient_str <- ifelse(metric == "death", "deaths", "cases")
  labels_out <- rep(NA, length(metric))
  na_idx <- is.na(metric_col)
  labels_out[na_idx] <- sprintf("Too few %s", insufficient_str)
  labels_out[!na_idx] <- sprintf("%s: %0.2f (%0.2f - %0.2f)",
                                 metric, metric_col[!na_idx],
                                 sf_dat$ci_lower[!na_idx],
                                 sf_dat$ci_upper[!na_idx])
  return(labels_out)
}

#' Generate labels for the map
#'
#' @param sf_dat An sf object with columns "Rt", "Rt_lwr", "Rt_upr", "UID",
#' "dispID", "geometry"
#' @param date_select The selected date
master_labeller <- function(sf_dat, date_select, metric) {
  rate_labels <- rate_labeller(sf_dat, metric)
  labels_final <- sprintf("<strong>%s</strong><br/>Date: %s<br/>%s",
                          sf_dat$dispID, date_select, rate_labels) %>%
      lapply(htmltools::HTML)
  return(labels_final)
}

#' Get UIDs for all counties in a state, given a state UID.
#'
#' @param state_uid_str State UID as a string.
get_county_uids <- function(state_uid_str) {
  # MA, UT, MO have extra counties
  extra_states <- c("84000025", "84000049", "84000029")
  state_uid <- as.integer(state_uid_str)
  state_id <- state_uid %% 100

  if (state_uid == 630) {
    # Puerto Rico
    state_uid_lwr <- 63072000
    state_uid_upr <- 63073000
  } else {
    # based on the UID lookup table logic from JHU data
    state_uid_lwr <- 84000000 + state_id * 1e3
    state_uid_upr <- state_uid_lwr + 1e3
  }

  if (state_uid_str %in% extra_states) {
    extra_uids <- switch(state_uid_str,
      `84000025` = 84070002,
      `84000029` = 84070003,
      `84000049` = 84070015:84070020
    )
  } else {
    extra_uids <- NULL
  }
  ret <- list(uid_lwr = state_uid_lwr, uid_upr = state_uid_upr,
              extra_uids = extra_uids)
  return(ret)
}

#' Subset the sf_all object by date and resolution.
#'
#' @param date_select Selcted date as a date object.
#' @param sel_resolution Selected resolution.
#' @param state_uid A state UID string. If non-null, uses the state UID to
#' select the counties in that state. See get_county_uids.
sf_by_date_res <- function(date_select, metric = c("rt", "case", "death"),
                           sel_resolution, state_uid = NULL) {
  metric <- match.arg(metric)
  date_str <- format(date_select, "%Y-%m-%d")
  rate_col <- ifelse(metric == "rt", paste0(metric, "_", date_str),
                     paste0(metric, "_rate_", date_str))
  lwr_col <- paste0(metric, "_lower_", date_str)
  upr_col <- paste0(metric, "_upper_", date_str)
  select_cols <- c(rate_col, lwr_col, upr_col, "UID", "dispID")

  if (is.null(state_uid)) {
    ret_sf <- sf_all %>%
      dplyr::filter(resolution == sel_resolution) %>%
      dplyr::select(!!select_cols)
  } else {
    stopifnot(sel_resolution == "county")
    county_uids <- get_county_uids(state_uid)
    ret_sf <- sf_all %>%
      dplyr::filter(resolution == sel_resolution,
                    (UID > county_uids$uid_lwr & UID < county_uids$uid_upr) |
                      UID %in% county_uids$extra_uids) %>%
      dplyr::select(!!select_cols)
  }
  stopifnot(nrow(ret_sf) >= 1)
  names(ret_sf) <- c("estimate", "ci_lower", "ci_upper", "UID", "dispID", "geometry")
  return(ret_sf)
}

#' Add polygons and points to a leaflet object.
#'
#' @param .map A leaflet object
#' @param data An sf object containing the geometries to put on the map.
#' @param labels Labels for each row in the sf data.
#' @param grpid Group ID for the added polygons and points. Allows one to use
#' clearGroup to remove the added polygons and points.
addPolygon_Point <- function(.map, .data, labels,
                             metric = c("rt", "case", "death"),
                             grpid = "default") {
  metric <- match.arg(metric)
  stopifnot(names(.data) == c("estimate", "ci_lower", "ci_upper", "UID",
                              "dispID", "geometry"))
  pal_cur <- pal_lst[[metric]]
  pal <- pal_cur(domain = .data$estimate)
  polygon_idx <- st_is(.data, "POLYGON") | st_is(.data, "MULTIPOLYGON")
  data_polygons <- .data[polygon_idx, ]
  data_points <- .data[!polygon_idx, ]
  map_ret <- addPolygons_default(.map, data = data_polygons,
                                 group = grpid,
                                 fillColor = ~pal(estimate), layer = ~UID,
                                 label = labels[polygon_idx])

  if (sum(polygon_idx) < nrow(.data)) {
    map_ret <- addCircles_default(map_ret, data = data_points,
                                  group = grpid,
                                  fillColor = ~pal(estimate), layer = ~UID,
                                  label = labels[!polygon_idx])
  }
  return(map_ret)
}

#' Set the zoom level for a state based on its size
#'
#' @param state_uid_str State UID as a string.
#' @param default_zoom Default zoom level for leaflet
set_state_zoom <- function(state_uid_str, default_zoom = 6) {
  stopifnot(default_zoom >= 2)
  state_str <- state_uid_to_place[state_uid_str]

  # use variables defined by R
  state_area_ordered <- state.name[order(state.area)]
  huge_states <- state_area_ordered[50]
  large_states <- state_area_ordered[48:49]
  small_states <- state_area_ordered[4:10]
  tiny_states <- state_area_ordered[1:3]


  # zoom in more for tiny states, less for small states, and zoom out for large
  # states.
  if (state_str %in% tiny_states) {
    return(default_zoom + 2)
  } else if (state_str %in% small_states && state_str != "Hawaii") {
    return(default_zoom + 1)
  } else if (state_str %in% large_states) {
    return(default_zoom - 1)
  } else if (state_str %in% huge_states) {
    return(default_zoom - 2)
  } else {
    return(default_zoom)
  }
}

#' Generate a plot of Rt over time and new cases over time based on map click.
click_plot <- function(plt_dat) {
  place_name <- unique(plt_dat$dispID)
  rt_plt_title <- sprintf("Rt for %s", place_name)
  ymax_rt <- min(10, max(plt_dat$rt_upper, na.rm = TRUE))
  rt_plt <- plt_dat %>%
    ggplot(aes(x = date, y = rt, ymin = rt_lower, ymax = rt_upper)) +
    geom_ribbon(fill = "#9e9e9e") + geom_line() +
    coord_cartesian(ylim = c(0, ymax_rt)) +
    geom_hline(yintercept = 1, lty = 2) +
    xlab("Date") + ylab("") + ggtitle(rt_plt_title) +
    theme_cowplot() +
    background_grid(major = "xy", minor = "xy") +
    theme(text = element_text(size = 18),
          axis.text = element_text(size = 15))

  newcases_plt_title <- sprintf("New Cases for %s", place_name)
  ymax_newcases <- min(300, max(plt_dat$case_upper,
                                plt_dat$positiveIncrease_percapita,
                                na.rm = TRUE))
  ymax_newcases <- NA
  newcases_plt <- plt_dat %>%
    ggplot(aes(x = date, y = case_rate, ymin = case_lower, ymax = case_upper)) +
    geom_ribbon(fill = "#9e9e9e") + geom_line(aes(linetype = "Smoothed")) +
    geom_line(aes(y = positiveIncrease_percapita, linetype = "Unsmoothed")) +
    xlab("Date") + ylab("") + ggtitle(newcases_plt_title) +
    theme_cowplot() +
    #coord_cartesian(ylim = c(1, ymax_newcases)) +
    coord_cartesian(ylim = c(0, NA)) +
    background_grid(major = "xy", minor = "xy") +
    theme(text = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.position = "bottom") +
    guides(linetype = guide_legend(nrow = 2))
  deaths_plt_title <- sprintf("New Deaths for %s", place_name)
  deaths_plt <- plt_dat %>%
    ggplot(aes(x = date, y = death_rate, ymin = death_lower, ymax = death_upper)) +
    geom_ribbon(fill = "#9e9e9e") + geom_line(aes(linetype = "Smoothed")) +
    geom_line(aes(y = deathIncrease_percapita, linetype = "Unsmoothed")) +
    xlab("Date") + ylab("") + ggtitle(deaths_plt_title) +
    theme_cowplot() +
    coord_cartesian(ylim = c(0, NA)) +
    background_grid(major = "xy", minor = "xy") +
    theme(text = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.position = "bottom") +
    guides(linetype = guide_legend(nrow = 2))
  final_plt <- plot_grid(rt_plt, newcases_plt, deaths_plt, ncol = 1,
                         align = "v", axis = "l")
  return(final_plt)
}

#' Draw a blank plot
blank_plot <- function(title = "Insufficient data") {
  p <- ggplot() + ggtitle(title) +
    theme(axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          axis.title = element_text(size = 20),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          plot.title = element_text(size = 24),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  return(p)
}

#' Helper function for forest plot and heat map
get_plt_params <- function(metric = c("rt", "case", "death")) {
  metric <- match.arg(metric)
  plt_params <- switch(metric,
    "rt" = list(bins = bins_rt, var = "rt", lwr = "rt_lower", upr = "rt_upper",
                labels = rt_color_labels, color = colors_rt,
                date_var = "date_lag", title_str = "Rt"),
    "case" = list(bins = bins_cases, var = "case_rate", lwr = "case_lower",
                  upr = "case_upper", labels = cases_color_labels,
                  color = colors_cases, date_var = "date",
                  title_str = "Case Rate per Million"),
    "death" = list(bins = bins_deaths, var = "death_rate", lwr = "death_lower",
                   upr = "death_upper", labels = deaths_color_labels,
                   color = colors_cases, date_var = "date",
                   title_str = "Death Rate per Million")
  )
  return(plt_params)
}

subset_rt_by_res_date <- function(sel_resolution, date_select = NULL, metric = NULL) {
  if (startsWith(sel_resolution, "840") || sel_resolution == "630") {
    county_uids <- get_county_uids(sel_resolution)
    dat_subset <- rt_long_all[resolution == "county" &
                              ((UID > county_uids$uid_lwr & UID < county_uids$uid_upr) |
                              UID %in% county_uids$extra_uids), ]
  } else {
    dat_subset <- rt_long_all[resolution == sel_resolution, ]
  }
  if (!is.null(metric) && metric == "rt" && !is.null(date_select)) {
    dat_subset <- dat_subset[date_lag == date_select, ]
  } else if (!is.null(date_select)) {
    dat_subset <- dat_subset[date == date_select, ]
  }
  return(dat_subset)
}

#' Set up data frame for forest plot and heat map
setup_plot_df <- function(sel_resolution, date_select = NULL,
                          metric = c("rt", "case", "death"),
                          sorted = c("alphabetical", "metric")) {

  metric <- match.arg(metric)
  sorted <- match.arg(sorted)

  # first, subset rt_long_all
  dat_subset <- subset_rt_by_res_date(sel_resolution, date_select, metric)

  plt_params <- get_plt_params(metric)
  dat_subset$range <- cut(dat_subset[[plt_params$var]],
                          breaks = plt_params$bins,
                          labels = plt_params$labels, include.lowest = TRUE,
                          right = FALSE)

  # configure colors for bins
  color_pal <- plt_params$color
  names(color_pal) <- levels(dat_subset$range)

  plt_df <- dat_subset[!is.na(get(plt_params$var)), ]

  if (isTRUE(identical(sel_resolution, "county")) ||
      isTRUE(startsWith(sel_resolution, "840")) ||
      isTRUE(identical(sel_resolution, "630")) ||
      isTRUE(startsWith(sel_resolution, "subnat_"))) {
    # change e.g. Texas, USA -> Texas; Kings, New York -> Kings
    plt_df[, dispID := sub(", [A-Za-z ]+", "", dispID)]
  }
  if (sorted == "metric") {
    unique_dispIDs <- uniqueN(dat_subset$dispID)
    stopifnot(isTRUE(unique_dispIDs == nrow(dat_subset)))
    setorderv(plt_df, cols = plt_params$var, order = 1, na.last = TRUE)
    plt_df[, dispID_ord := factor(dispID, levels = dispID)]
  } else {
    stopifnot(isTRUE(identical(sorted, "alphabetical")))
    plt_df[, dispID_ord := factor(dispID)]
  }
  uniqn_dispIDs <- uniqueN(plt_df$dispID)
  ret <- list(plt_df = plt_df, plt_params = plt_params, color_pal = color_pal,
              uniqn_dispIDs = uniqn_dispIDs, date_select = date_select)
  return(ret)
}

#' Draw forest plot of current Rt and confidence interval
forest_plot <- function(plt_df_params) {

  p <- with(plt_df_params, {
    if (nrow(plt_df) == 0) {
      # quit if there's no data
      return(blank_plot())
    }
    title_str <- sprintf("%s on %s", plt_params$title_str, date_select)
    xlab_str <- sprintf("%s and 95%% CI", plt_params$title_str)
    p <- ggplot(plt_df,
          aes_string(x = plt_params$var, y = "dispID_ord",
                      xmin = plt_params$lwr, xmax = plt_params$upr)) +
      geom_point(size = 3) + geom_pointrange() +
      xlab(xlab_str) + ylab("") +
      ggtitle(title_str) +
      #coord_cartesian(xlim = c(0, 5)) +
      theme(axis.text.y = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.title = element_text(size = 20),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 16),
            plot.title = element_text(size = 24))
    if (plt_params$var == "rt") {
      p <- p + geom_vline(xintercept = 1, lty = 2, color = "black", lwd = 1.5)
    }
    p
  })
  return(p)
}

#' Draw heat map of Rt/case/death rate
heat_map <- function(plt_df_params) {
  p <- with(plt_df_params, {
    if (nrow(plt_df) == 0) {
      return(blank_plot())
    }

    title_str <- sprintf("%s Heatmap", plt_params$title_str)
    xlab_str <- sprintf("%s", plt_params$title_str)
    p <- ggplot(plt_df,
          aes_string(x = plt_params$date_var, y = "dispID_ord",
                    fill = "range")) +
      geom_tile(alpha = 0.7) +
      scale_fill_manual(drop = FALSE, values = color_pal,
                        name = plt_params$title_str) +
      xlab(xlab_str) + ylab("") +
      ggtitle(title_str) +
      scale_y_discrete(limits = rev(levels(plt_df$dispID_ord))) +
      #coord_cartesian(xlim = c(0, 5)) +
      scale_x_date(expand = c(0, 0)) +
      theme(axis.text.y = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.title = element_text(size = 20),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 16),
            plot.title = element_text(size = 24),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
      p
  })
  return(p)
}

#' Helper for compare_plot
#'
#' Actual function that draws the plot.
compare_plt_helper <- function(dt, x, y, metric_str, ci_lwr = NULL,
                               ci_upr = NULL, color = "dispID", fill = "dispID",
                               yintercept = NA) {
  title_str <- sprintf("Comparison of %s", metric_str)
  plt <- ggplot(dt, aes_string(x = x, y = y, color = color)) +
    geom_line() + geom_point() +
    scale_color_discrete(name = "Location") +
    scale_fill_discrete(name = "Location") +
    coord_cartesian(ylim = c(0, NA)) +
    ggtitle(title_str) + ylab("") +
    theme(text = element_text(size = 18),
          axis.text = element_text(size = 15))

  if (!is.null(ci_lwr)) {
    stopifnot(!is.null(ci_upr))
    plt <- plt +
      geom_ribbon(aes_string(ymin = ci_lwr, ymax = ci_upr, fill = fill),
                  alpha = 0.2)
  }
  if (!is.na(yintercept)) {
    plt <- plt +
      geom_hline(yintercept = yintercept, lty = 2)
  }

  if (x == "date_lag") {
    plt <- plt + xlab("Date (lagged 5 days)")
  } else if (x == "date") {
    plt <- plt + xlab("Date")
  }
  return(plt)
}

#' Draw the comparison plots.
#'
#' Set up plotting arguments and return a list of plots
compare_plot <- function(dt, metric_lst) {
  stopifnot(all(metric_lst %in% colnames(dt)))
  xlim_max <- max(dt$date)
  xlim_min <- min(dt$date_lag)

  plt_lst <- list()
  # plots where we need CI
  for (met in metric_lst) {
    plt_params <- switch(met,
      "rt" = list(x = "date_lag", y = met, ci_lwr = "rt_lower",
                  ci_upr = "rt_upper", yintercept = 1),
      "case_rate" = list(x = "date", y = met, ci_lwr = "case_lower",
                          ci_upr = "case_upper"),
      "death_rate" = list(x = "date", y = met, ci_lwr = "death_lower",
                          ci_upr = "death_upper"),
      list(x = "date", y = met)
    )
    plt_params$metric_str <- switch(met,
      "rt" = "Rt",
      "case_rate" = "Daily New Cases per Million",
      "death_rate" = "Daily New Deaths per Million",
      "positiveIncrease" = "Daily New Cases",
      "deathIncrease" = "Daily New Deaths",
      "positive_percapita" = "Total Cases per Million",
      "death_percapita" = "Total Deaths per Million",
      "positive" = "Total Cases",
      "death" = "Total Deaths",
      "metric")

    call_lst <- c(list(dt = dt), plt_params)
    plt_lst[[met]] <- do.call(compare_plt_helper, call_lst) +
        xlim(xlim_min, xlim_max)
  }
  return(plt_lst)
}

#' Get display ID from resolution

dispID_from_res <- function(sel_resolution) {
  if (startsWith(sel_resolution, "840") || sel_resolution == "630") {
    cur_uid <- sel_resolution
    dispID_cur <- sf_all %>%
      filter(UID == cur_uid) %>%
      pull(dispID) %>%
      sub(", [A-Za-z ]+", "", .)
  } else if (isTRUE(startsWith(sel_resolution, "subnat_"))) {
    dispID_cur <- substring(sel_resolution, first = 8)
  } else {
    dispID_cur <- sel_resolution
  }
  return(dispID_cur)
}

#' Set resolution from location
#'
#' @param loc_info_cur List that holds the return value from query_ip
res_from_locinfo <- function(loc_info_cur) {
  lat <- loc_info_cur$latitude
  long <- loc_info_cur$longitude
  latlong_dat <- data.frame(Latitude = lat, Longitude = long)
  if (nrow(latlong_dat) == 0 || lat < -900 || long < -900) {
    set_res <- "country"
  } else {
    latlong_sf <- latlong_dat %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(sf_all))

    intersected <- suppressMessages({
      st_intersects(latlong_sf, sf_all)
    })
    sf_options <- sf_all[unlist(intersected), ] %>%
      select(dispID, resolution, UID)

    res_options <- sf_options$resolution

    if ("county" %in% res_options) {
      set_res <- sf_options %>%
        filter(resolution == "subnat_USA") %>%
        pull(UID) %>%
        as.character()
    } else if (any(startsWith(res_options, "subnat_"))) {
      set_res <- sf_options %>%
        filter(resolution == "country") %>%
        pull(UID) %>%
        as.character()
    } else {
      set_res <- "country"
    }
  }
  return(set_res)
}

set_date_input <- function(session, id, lag,
                           metric = c("rt", "case", "death")) {
  metric <- match.arg(metric)
  if (metric == "rt") {
    max_date <- date_lag_range[2] - 1
  } else {
    max_date <- date_real_range[2] - 1
  }
  new_date <- max_date - as.difftime(lag, unit = "days")
  updateDateInput(session, id, value = new_date)
}

########################################################################
## Define UI
########################################################################

ui <- function(req) {
  dashboardPage(
  dashboardHeader(title = "Visualizing COVID-19 Spread Metrics",
                  titleWidth = 450),
  dashboardSidebar(
    # If you want to add / subtract a tab from the sidebar, you must modify it
    # here and also add a new tabItem below.
    sidebarMenu(
      menuItem("Map", tabName = "Map"),
      menuItem("Compare Rt", tabName = "compare_rt"),
      menuItem("Table", tabName = "table"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tags$head(tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}")),
    tags$head(tags$style(type = "text/css", "body {font-size: 16px} .aboutpage {font-size: 16px}")),
    #tags$head(tags$script(src = "shinyjs_funs.js")),
    tags$head(includeHTML("assets/google-analytics.html")),
    tags$head(tags$style(
        ".leaflet .legend {text-align: left;}",
        ".leaflet .legend i{float: left;}",
        ".leaflet .legend label{float:left; text-align: left;}"
    )),
    # initialize ShinyStore. Do not remove this.
    initStore("store", "shinyStore"),

    useShinyjs(),

    # This allows all links to be opened in a new tab. Fixes issue where links
    # sometimes refuse to connect.
    tags$head(tag("base", varArgs = list(target = "_blank"))),
    fluidPage(
      tabItems(
        # first panel: big Rt map with multiple resolutions.
        tabItem("Map",
          br(),
          fluidRow(
            includeMarkdown("assets/header.md"),
          ),
          fluidRow(
            column(width = 4,
                   dateInput("map_date", label = "Date (lagged by 7 days for Rt)",
                               min = date_lag_range[1],
                               max = date_lag_range[2] - 1,
                               value = date_lag_range[2] - 1,
                               format = "D MM d, yyyy", width = "95%"),
                   actionButton("map_latest", label = "Latest"),
                   actionButton("map_2week", label = "2 weeks ago"),
                   actionButton("map_1month", label = "1 month ago"),
                   actionButton("map_2month", label = "2 months ago")
            ), # end of column 1
            column(width = 4,
              radioButtons("map_metric", "Metric:",
                          choices = list("Rt (effective reproduction number), lagged 7 days" = "rt",
                                          "Daily new cases per million" = "case",
                                          "Daily new deaths per million" = "death")),
            ), # end of column 2
            column(width = 4,
              selectInput("select_resolution", "Resolution:",
                          choices = resolution_choices,
                          selected = "auto"),
              actionButton("reset_plot", label = "Reset Plot"),
              downloadButton("click_plot_dl", "Download Plot")
            ) # end of column 3
          ), # end of fluidRow 1
          fluidRow(
            column(8, leafletOutput("map_main", height = "600px", width = "100%")),
            # plot of Rt over time
            column(4, plotOutput("map_click_plot", height = "600px"))
          ), # end of fluidRow 2
          # hidden heatmap and forestplot
          fluidRow(
            column(6, shinyjs::hidden(uiOutput("heatmap_ui")), align = "center"),
            column(6, shinyjs::hidden(uiOutput("forestplot_ui")), align = "center")
          ), # end of fluidRow 3
          # download buttons for heatmap and forestplot
          fluidRow(
            column(6,
              shinyjs::hidden(
                downloadButton("heatmap_dl", "Download Heat Map")
              ),
              align = "center"),
            column(6,
              shinyjs::hidden(
                downloadButton("forestplot_dl", "Download Forest Plot")
              ),
              align = "center")
          ), # end of fluidRow
          fluidRow(
            column(12, actionButton("toggle_more", label = "Show More"),
                   align = "center")
          ),
          fluidRow(
            textOutput("ip_addr")
          )
        ), # end of tabItem
        # Second tab: Compare Rt across different regions.
        tabItem("compare_rt",
          fluidPage(
            column(width = 4,
              h4("Select areas to compare their Rt."),
              p(sprintf("Note the Rt is lagged by %d days.", lag_rt)),
              p("Some areas may not appear in the plot for all time points because of insufficient data."),
              p("Occasionally, locations may have negative values for new cases because of reporting issues."),
              # break up the selection by state, county, and country
              # source for remove button:
              # https://gist.github.com/pvictor/ee154cc600e82f3ed2ce0a333bc7d015
              selectizeInput("compare_sel_states", label = "States/Provinces",
                             choices = place_choices$us_state,
                             multiple = TRUE,
                             options = list('plugins' = list('remove_button'),
                                            'create' = TRUE,
                                            'persist' = FALSE)),
              selectizeInput("compare_sel_counties", label = "Counties (US)",
                             choices = NULL,
                             multiple = TRUE,
                             options = list('plugins' = list('remove_button'),
                                            'create' = TRUE,
                                            'persist' = FALSE)),
              selectizeInput("compare_sel_countries", label = "Countries",
                             choices = place_choices$country,
                             multiple = TRUE,
                             options = list('plugins' = list('remove_button'),
                                            'create' = TRUE,
                                            'persist' = FALSE)),
              checkboxGroupInput("compare_metric",
                                 label = "Select metrics to compare",
                                 choices = list("Rt" = "rt",
                                                "Daily new cases per million" = "case_rate",
                                                "Daily new deaths per million" = "death_rate",
                                                "Daily new cases" = "positiveIncrease",
                                                "Daily new deaths" = "deathIncrease",
                                                "Total cases per million" = "positive_percapita",
                                                "Total deaths per million" = "death_percapita",
                                                "Total cases" = "positive",
                                                "Total deaths" = "death"),
                                 selected = c("rt", "case_rate", "death_rate")),
              actionButton("compare_submit", label = "Draw Plot"),
              actionButton("compare_reset", label = "Reset Fields"),
              downloadButton("compare_dl", label = "Download Plot")
            ), # end of column
            column(width = 8, uiOutput("compare_plt_ui"))
          ) # end of fluidPage
        ), # end of tabItem
        # 3rd tab: table of Rts and other metrics
        tabItem("table",
          fluidRow(
            box(width = 6,
              dateInput("table_date", label = "Date",
                        min = date_real_range[1], max = date_real_range[2] - 1,
                        value = date_lag_range[2] - 1,
                        format = "D MM d, yyyy")
            ), # end of box 1
            box(width = 6,
              selectInput("table_select_resolution", "Resolution:",
                          choices = resolution_choices,
                          selected = "country")
            ) # end of box 2
          ), # end of fluidRow 1
          # fluidRow 2: selecting columns
          fluidRow(
            column(width = 6,
              selectizeInput("table_cols",
                             label = "Select columns for table",
                             choices = table_col_choices,
                             selected = c("rt_lag", "rt_lower_lag", "rt_upper_lag", "case_rate", "case_lower", "case_upper", "death_rate", "death_lower", "death_upper"),
                             multiple = TRUE,
                             options = list('plugins' = list('remove_button'),
                                            'create' = TRUE,
                                            'persist' = FALSE),
                             width = "80%")
            ),
            column(width = 6,
              actionButton("table_reset", "Reset Columns"),
              downloadButton("table_download", "Download Table"),
              align = "center"
            )
          ), # end of fluidRow 2
          # fluidRow 3: reset & download buttons
          # fluidRow 4: actual table output
          fluidRow(
            h2(textOutput("Rt_table_title")),
            p(sprintf("Click a column to sort by that metric. Note that Rt is not available beyond %s due to the %d day lag.",
                      format(date_lag_range[2] - 1, "%a %B %-e, %Y"), lag_rt)),
          ),
          fluidRow(
             div(style = 'overflow-x: scroll', DT::DTOutput("Rt_table"))
          ),
          fluidRow(
            br(),
            includeMarkdown("assets/Rt_table_footer.md")
          ) # end of fluidRow 4
        ), # end of tabItem for table
        # last tab: About page
        tabItem("about",
          withTags({
            div(class = "aboutpage",
              # TODO: edit the about page
              includeMarkdown("assets/about.md")
            )
          }) # end of withTags
        ) # end of tabItem
      ), # end of tabItems

      # Invisible div to get the user's IP address, sourced from:
      # https://github.com/rstudio/shiny/issues/141#issuecomment-351857670
      div(style = "display: none;",
        textInput("remote_addr", "remote_addr",
          if (!is.null(req[["HTTP_X_FORWARDED_FOR"]]))
            req[["HTTP_X_FORWARDED_FOR"]]
          else
            req[["REMOTE_ADDR"]]
        )
      ), # end of div
    ) # end of fluidPage
  ) # end of dashboardBody
) # end of dashboardPage
}

########################################################################
## Define Server function
########################################################################

server <- function(input, output, session) {
  cdata <- session$clientData

  ########################################################################
  ## Query the IP address for geolocation
  ########################################################################
  cat("The remote IP is", isolate(input$remote_addr), "\n")

  loc_info <- reactiveValues(value = NULL, resolution = NULL)

  observe({
    ipaddr <- strsplit(isolate(input$remote_addr), ", ", fixed = TRUE)[[1]][1]
    if (is.null(input$store$loc_info) ||
        is.null(input$store$loc_info$ipaddr) ||
        input$store$loc_info$ipaddr != ipaddr) {
      ipinfo <- query_ip(ipaddr)
      loc_info$value <- ipinfo
      updateStore(session, "loc_info", ipinfo)
    } else {
      message("Read value from storage")
      # spin lock while waiting for storage to update
      while (is.null(input$store$loc_info)) {
        Sys.sleep(0.1)
      }
      loc_info$value <- input$store$loc_info
    }
    loc_info$resolution <- res_from_locinfo(loc_info$value)
  })

  ########################################################################
  ## 1st tab: Big Rt Map
  ########################################################################

  # update the data based on inputs
  sf_dat_update <- reactive({
    shiny::validate(need(input$map_date, "Please select a date."))
    shiny::validate(need(input$select_resolution, "Please select a resolution."))
    shiny::validate(need(input$map_metric, "Please select a metric."))
    cur_res <- ifelse(input$select_resolution == "auto",
                      loc_info$resolution, input$select_resolution)
    cur_metric <- input$map_metric
    date_value_cur <- input$map_date

    if (cur_metric == "rt") {
      min_date <- date_lag_range[1]
      max_date <- date_lag_range[2] - 1
    } else {
      min_date <- date_real_range[1]
      max_date <- date_real_range[2] - 1
    }

    # check that date is in bounds
    req(min_date <= date_value_cur && date_value_cur <= max_date)

    # if selected resolution starts with 840 is 630 it's a US county
    if (startsWith(cur_res, "840") || cur_res == "630") {
      resolution <- "county"
      state_uid <- cur_res
    } else {
      resolution <- cur_res
      state_uid <- NULL
    }
    date_touse <- date_value_cur
    if (cur_metric == "rt") {
      date_touse <- date_value_cur + as.difftime(lag_rt, units = "days")
    }
    sf_by_date_res(date_touse, metric = cur_metric,
                   sel_resolution = resolution, state_uid = state_uid)
  })

  map_view <- reactive({
    cat(file = stderr(), "Calling map_view...\n")
    res <- input$select_resolution
    sf_dat_cur <- isolate(sf_dat_update())

    # by default, use bounding box, but these are exceptions where the bounding
    # box doesn't work
    ret <- list()
    ret$view <- switch(res,
                       "subnat_USA" = c(-96, 37.8, 4),
                       "county" = c(-96, 37.8, 4),
                       "subnat_Canada" = c(-100.78, 51.52, 3),
                       "84000002" = c(-147, 61.31395, 4),
                       NULL)
    ret$type <- "view"

    if (is.null(ret$view)) {
      ret$view <- as.numeric(st_bbox(sf_dat_cur))
      ret$type <- "bbox"
    }
    ret
  })

  # The basic big Rt map
  output$map_main <- renderLeaflet({
    map <- leaflet(options = leafletOptions(worldCopyJump = TRUE)) %>%
      setView(0, 30, 2) %>%
      setMaxBounds(-180, -90, 180, 90) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                        options = providerTileOptions(minZoom = 2,
                                                      noWrap = TRUE))
    map_view_cur <- isolate(map_view())
    if (map_view_cur$type == "bbox") {
      bbox <- map_view_cur$view
      map <- map %>%
        fitBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3],
                  lat2 = bbox[4])
    } else {
      cur_view <- map_view_cur$view
      map <- map %>%
        setView(cur_view[1], cur_view[2], cur_view[3])
    }
    legend_params <- switch(isolate(input$map_metric),
      "rt" = list(cur_colors = colors_rt, cur_labels = rt_color_labels,
                  cur_title = "Rt"),
      "case" = list(cur_colors = colors_cases, cur_labels = cases_color_labels,
                    cur_title = "Cases per mil."),
      "death" = list(cur_colors = colors_cases, cur_labels = deaths_color_labels,
                     cur_title = "Deaths per mil.")
    )
    map <- map %>%
        addLegend(colors = legend_params$cur_colors,
                  labels = legend_params$cur_labels,
                  opacity = 0.7, title = legend_params$cur_title,
                  position = "bottomleft", layerId = "legend")
    suppressWarnings(map)
  })

  # change the date selector widget / send the new date to sf_dat_update
  #observeEvent(input$map_metric, {
  #  if (input$map_metric == "rt") {
  #    min_date <- date_lag_range[1]
  #    max_date <- date_lag_range[2] - 1
  #  } else {
  #    min_date <- date_real_range[1]
  #    max_date <- date_real_range[2] - 1
  #  }

  #  new_slider_val <- input$map_date
  #  if (isTRUE(is.null(new_slider_val)) || isTRUE(is.na(new_slider_val)) ||
  #      isTRUE(new_slider_val > max_date)) {
  #    # if current value is beyond max range or if current value is the max Rt
  #    # date, set it to the max date
  #    new_slider_val <- max_date
  #  } else if (new_slider_val < min_date) {
  #    new_slider_val <- min_date
  #  }
  #  updateDateInput(session, "map_date", value = new_slider_val,
  #                  min = min_date, max = max_date)
  #})

  # change the zoom level when the resolution changes.
  observeEvent(map_view(), {
    map_view_cur <- map_view()

    if (map_view_cur$type == "bbox") {
      bbox <- map_view_cur$view
      suppressWarnings(
        leafletProxy("map_main", session) %>%
          fitBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3],
                    lat2 = bbox[4])
      )
    } else {
      cur_view <- map_view_cur$view
      suppressWarnings(
        leafletProxy("map_main", session) %>%
          setView(cur_view[1], cur_view[2], cur_view[3])
      )
    }
  })

  # change the default clicked object when resolution changes

  # keep track of previous group ID so we can clear its shapes
  prev_grpid_all_reactive <- reactiveValues(val = "")

  # change the shapes on the map when the resolution or date changes
  observe({
    sf_dat_cur <- sf_dat_update()
    date_select <- format(isolate(input$map_date), "%Y-%m-%d")
    sel_resolution <- isolate(input$select_resolution)
    cur_metric <- isolate(input$map_metric)
    labels_final <- master_labeller(sf_dat_cur, date_select, cur_metric)
    cur_grpid <- digest::digest(c(date_select, sel_resolution, cur_metric))
    prev_grpid <- prev_grpid_all_reactive$val
    map <- leafletProxy("map_main", data = sf_dat_cur)
    if (prev_grpid != cur_grpid) {
      suppressWarnings({
        map <- map %>%
          addPolygon_Point(sf_dat_cur, labels = labels_final,
                           metric = cur_metric, grpid = cur_grpid) %>%
          clearGroup(prev_grpid)
      })
    }
    prev_grpid_all_reactive$val <- cur_grpid
    map
  })

  # change the legend when the map metric changes
  observeEvent(input$map_metric, {
    shiny::validate(need(input$map_metric, "Please select a metric."))

    # part for changing the map
    map <- leafletProxy("map_main")
    legend_params <- switch(input$map_metric,
      "rt" = list(cur_colors = colors_rt, cur_labels = rt_color_labels,
                  cur_title = "Rt"),
      "case" = list(cur_colors = colors_cases, cur_labels = cases_color_labels,
                    cur_title = "Cases per mil."),
      "death" = list(cur_colors = colors_cases, cur_labels = deaths_color_labels,
                     cur_title = "Deaths per mil.")
    )

    suppressWarnings({
      map <- map %>%
        addLegend(colors = legend_params$cur_colors,
                  labels = legend_params$cur_labels,
                  opacity = 0.7, title = legend_params$cur_title,
                  position = "bottomleft", layerId = "legend")
      map
    })
  })

  # set up series of event handlers to set the date based on clicking the
  # buttons
  observeEvent(input$map_latest, {
    set_date_input(session, "map_date", 0L, input$map_metric)
  })
  observeEvent(input$map_2week, {
    set_date_input(session, "map_date", 14L, input$map_metric)
  })
  observeEvent(input$map_1month, {
    set_date_input(session, "map_date", 30L, input$map_metric)
  })
  observeEvent(input$map_2month, {
    set_date_input(session, "map_date", 60L, input$map_metric)
  })

  # Rt over time based on click
  click_reactive <- reactiveValues(cur_uid = NULL)
  observeEvent(input$map_main_shape_click, {
    req(input$map_main_shape_click)
    req(input$map_main_shape_click$id)
    click_reactive$cur_uid <- input$map_main_shape_click$id
  })

  # set clicked UID to null every time the resolution changes or user hits the
  # reset_plot button
  observeEvent(eventExpr = {
    input$select_resolution
    input$reset_plot
  }, handlerExpr = {
    # select a random country to plot
    cur_resolution <- ifelse(input$select_resolution == "auto",
                             loc_info$resolution,
                             input$select_resolution)
    if (cur_resolution == "country") {
      updateActionButton(session, "reset_plot", label = "Random Country")
      click_reactive$cur_uid <- sample(country_uids, 1)
    } else {
      updateActionButton(session, "reset_plot", label = "Reset Plot")
      click_reactive$cur_uid <- NULL
    }
  })

  # get the current UID for click plot
  render_uid <- reactive({
    ret <- click_reactive$cur_uid
    cur_res <- ifelse(input$select_resolution == "auto", loc_info$resolution,
                      input$select_resolution)
    if (is.null(ret)) {
      if (isTRUE(startsWith(cur_res, "subnat_"))) {
        if (isTRUE(identical(cur_res, "subnat_USA"))) {
          ret <- 840
        } else {
          country_name <- substring(cur_res, first = 8)
          country_uid <- rt_long_all[dispID == country_name, UID]
          if (isTRUE(uniqueN(country_uid) == 1)) {
            ret <- unique(country_uid)
          }
        }
      } else if (isTRUE(startsWith(cur_res, "840")) ||
                isTRUE(identical(cur_res, "630"))) {
        ret <- as.integer(input$select_resolution)
      }
    }
    ret
  })

  # click plot plotting code
  click_plot_cur <- reactive({
    render_uid_cur <- render_uid()
    shiny::validate(need(render_uid_cur,
                         "Please click a location to show the plot"))
    plt_dat <- rt_long_all[UID == render_uid_cur, ]
    shiny::validate(need(nrow(plt_dat) > 0, "No data to plot."))
    suppressWarnings(click_plot(plt_dat))
  })

  # actually rendering click plot
  output$map_click_plot <- renderCachedPlot({
    click_plot_cur()
  }, cacheKeyExpr = { render_uid() })

  # click plot download handler
  output$click_plot_dl <- downloadHandler(
    filename = function() {
      cur_uid <- render_uid()
      dispID_cur <- sf_all %>%
        filter(UID == cur_uid) %>%
        pull(dispID)
      return(sprintf("%s_rt_case_death.png", dispID_cur))
    },
    content = function(file) {
      ggsave(filename = file, plot = click_plot_cur(),
             height = 8, width = 5)

    }
  )

  output$ip_addr <- renderText({
    with(loc_info$value,
         sprintf("Your location was automatically detected as: %s", place_str))
  })

  ########################################################################
  ## 1st tab, part 2: Heatmap
  ########################################################################

  # toggle showing more or less
  observeEvent(input$toggle_more, {
    if (input$toggle_more %%2 == 1) {
      shinyjs::show("heatmap_ui")
      shinyjs::show("forestplot_ui")
      shinyjs::show("heatmap_dl")
      shinyjs::show("forestplot_dl")
      updateActionButton(session, "toggle_more", label = "Show Less")
    } else {
      shinyjs::hide("heatmap_ui")
      shinyjs::hide("forestplot_ui")
      shinyjs::hide("heatmap_dl")
      shinyjs::hide("forestplot_dl")
      updateActionButton(session, "toggle_more", label = "Show More")
    }
  })

  # processing data for heatmap
  heatmap_data <- reactive({
    shiny::validate(need(input$select_resolution, message = "Please select a resolution"))
    shiny::validate(need(input$map_metric, "Please select a metric."))
    setup_plot_df(input$select_resolution, metric = input$map_metric,
                  sorted = "alphabetical")
  })

  # heatmap ggplot object
  heatmap_plot <- reactive({
    shiny::validate(need(input$select_resolution != "county",
                          "Heat Map not available for all US Counties"))
    heatmap_data_cur <- heatmap_data()
    shiny::validate(need(nrow(heatmap_data_cur$plt_df) > 0,
                        "Insufficient data."))
    heat_map(heatmap_data_cur)
  })

  # actually rendering the plot
  output$heatmap <- renderCachedPlot({
    shiny::validate(need(input$select_resolution, message = "Please select a resolution"))
    shiny::validate(need(input$map_metric, "Please select a metric."))
    heatmap_plot()
  }, cacheKeyExpr = { list(input$select_resolution, input$map_metric) })

  # UI for heatmap: height depends on number of rows
  output$heatmap_ui <- renderUI({
    if (input$select_resolution == "county") {
      num_rows <- 0
    } else {
      num_rows <- heatmap_data()$uniqn_dispIDs
    }
    #num_rows <- sum(is.na(heatmap_data()[, ..column_select]))
    plt_height <- sprintf("%dpx", max(20 * num_rows, 300))
    plotOutput("heatmap", height = plt_height)
  })

  output$heatmap_dl <- downloadHandler(
    filename = function() {
      dispID_cur <- dispID_from_res(input$select_resolution)
      fname <- sprintf("%s_%s_heatmap.png", dispID_cur, input$map_metric)
      return(fname)

    },
    content = function(file) {
      ggsave(filename = file, plot = heatmap_plot(),
             height = max(0.3 * heatmap_data()$uniqn_dispIDs, 6),
             width = 8, limitsize = FALSE)
    }
  )

  ########################################################################
  ## 1st tab, part 3: Rt Forest plot
  ########################################################################

  # data for forest plot
  forestPlot_data <- reactive({
    shiny::validate(need(input$map_date, "Please select a date."))
    shiny::validate(need(input$map_metric, "Please select a metric."))
    shiny::validate(need(input$select_resolution, "Please select a resolution."))
    date_select <- format(input$map_date, "%Y-%m-%d")
    setup_plot_df(input$select_resolution, date_select = date_select,
                  metric = input$map_metric, sorted = "metric")
  })

  # forestplot ggplot object
  forestplot_plot <- reactive({
    shiny::validate(need(input$select_resolution != "county",
                         "Forest Plot not available for all US Counties"))
    forestPlot_data_cur <- forestPlot_data()
    shiny::validate(need(nrow(forestPlot_data_cur$plt_df) > 0,
                        "Insufficient data."))
    forest_plot(forestPlot_data_cur)
  })

  # actually rendering the plot
  output$forestplot <- renderCachedPlot({
    forestplot_plot()
  }, cacheKeyExpr = { list(input$map_date, input$map_metric,
                           input$select_resolution) })

  # rendering the plot
  output$forestplot_ui <- renderUI({
    if (input$select_resolution == "county") {
      num_rows <- 0
    } else {
      num_rows <- forestPlot_data()$uniqn_dispIDs
    }
    plt_height <- sprintf("%dpx", max(20 * num_rows, 300))
    plotOutput("forestplot", height = plt_height)
  })

  output$forestplot_dl <- downloadHandler(
    filename = function() {
      dispID_cur <- dispID_from_res(input$select_resolution)
      if (startsWith(input$select_resolution, "840") ||
          input$select_resolution == "630") {
        cur_uid <- input$select_resolution
        dispID_cur <- sf_all %>%
          filter(UID == cur_uid) %>%
          pull(dispID) %>%
          sub(", [A-Za-z ]+", "", .)
      } else {
        dispID_cur <- substring(input$select_resolution, first = 8)
      }
      fname <- sprintf("%s_%s_forestPlot_%s.png",
                       dispID_cur, input$map_metric,
                       format(input$map_date, "%Y-%m-%d"))
      return(fname)

    },
    content = function(file) {
      ggsave(filename = file, plot = forestplot_plot(),
             height = max(0.3 * forestPlot_data()$uniqn_dispIDs, 6),
             width = 8, limitsize = FALSE)
    }
  )


  ########################################################################
  ## 2nd tab: Compare Rt
  ########################################################################

  # server side selectize
  updateSelectizeInput(session, "compare_sel_counties",
                       choices = place_choices$county, server = TRUE)

  # get data to plot for Rt comparison
  compare_plt_data <- eventReactive(input$compare_submit, {
    # get all the UIDs in a data frame and join them with rt long to select
    selected_states <- input$compare_sel_states
    selected_counties <- input$compare_sel_counties
    selected_countries <- input$compare_sel_countries
    selected_uids <- data.table(
      UID = as.double(c(selected_states, selected_counties, selected_countries))
    )

    shiny::validate(need(nrow(selected_uids) > 0, "Please select some locations."))

    # inner join
    rt_long_all[selected_uids, on = "UID", nomatch = NULL]
  })

  # observe to see if we should reset
  observeEvent(input$compare_reset, {
    shinyjs::reset("compare_sel_states")
    shinyjs::reset("compare_sel_counties")
    shinyjs::reset("compare_sel_countries")
    shinyjs::reset("compare_metric")
  })

  # reactive thingy to draw the plot
  compare_plt_render <- reactive({
    cur_dat <- compare_plt_data()
    shiny::validate(need(nrow(cur_dat) > 0,
                         "Insufficient data for selected locations."))

    # use isolate() to avoid dependency on input$compare_metric; dependency is
    # handled by compare_plt_data() when user hits submit.
    metric_lst_touse <- isolate(input$compare_metric)

    plt_lst <- compare_plot(cur_dat, metric_lst = metric_lst_touse)
    plot_grid(plotlist = plt_lst, ncol = 1, align = "v", axis = "l")
  })

  # boilerplate to show the plot
  output$compare_plt_out <- renderPlot({
    compare_plt_render()
  })

  # reactive thing for plot height, used for renderUI and downloadHandler
  compare_plt_height <- reactive({
    # need to take a dependency on compare_plt_data to trigger UI re-draw on
    # submit
    cur_dat <- compare_plt_data()
    metric_lst <- isolate(input$compare_metric)
    n_metrics <- length(metric_lst)
    plt_height <- max(1, n_metrics)
    plt_height
  })

  # UI for plot
  output$compare_plt_ui <- renderUI({
    height_touse <- sprintf("%dpx", 300 * compare_plt_height())
    plotOutput("compare_plt_out", height = height_touse)
  })

  # download handler for plot
  output$compare_dl <- downloadHandler(
    filename = function() {
      return("compare_plt.png")
    },
    content = function(file) {
      ggsave(filename = file, plot = compare_plt_render(),
             height = 3 * compare_plt_height(), width = 8)
    }
  )

  ########################################################################
  ## 3rd tab: Table of Rts and other metrics
  ########################################################################
  output$Rt_table_title <- renderText({
    date_actual <- format(input$table_date, "%B %d, %Y")
    req(date_actual)
    sprintf("Table of metrics for %s.", date_actual)
  })

  rt_table_render <- reactive({
    sel_resolution <- ifelse(input$table_select_resolution == "auto",
                             loc_info$resolution,
                             input$table_select_resolution)
    date_select <- format(input$table_date, "%Y-%m-%d")
    sel_cols <- input$table_cols
    shiny::validate(need(date_select, "Please select a date."))
    shiny::validate(need(sel_resolution, "Please select a resolution."))
    shiny::validate(need(sel_cols, "Please select some columns."))
    sel_cols_touse <- c("dispID", sel_cols)

    dat_subset <- subset_rt_by_res_date(sel_resolution,
                                        date_select)[, .SD, .SDcols = sel_cols_touse]

    shiny::validate(need(nrow(dat_subset) > 0, "This data has no rows."))

    numeric_cols <- unlist(table_col_choices, use.names = FALSE)
    numeric_cols_in_tab <- intersect(numeric_cols, sel_cols)
    dat_subset[, (numeric_cols_in_tab) := lapply(.SD, round, digits = 2),
               .SDcols = numeric_cols_in_tab]

    table_col_choices_vec <- unlist(table_col_choices, use.names = TRUE)
    idx <- table_col_choices_vec %in% sel_cols
    oldnames <- c("dispID", table_col_choices_vec[idx])
    newnames <- c("Location", names(table_col_choices)[idx])
    setnames(dat_subset, old = oldnames, new = newnames)
    order_col <- "Location"
    order_num <- 1
    try_order_cols <- c("Case rate", "Rt", "Daily new cases",
                        "Daily new cases per million")
    for (try_col in try_order_cols) {
      if (try_col %in% newnames) {
        order_col <- try_col
        order_num <- -1
        break
      }
    }
    setorderv(dat_subset, cols = order_col, order = order_num, na.last = TRUE)
  })

  observeEvent(input$table_reset, {
    shinyjs::reset("table_cols")
  })

  # Table of current Rts at current resolution
  output$Rt_table <- DT::renderDT({
    rt_table_render()
  }, server = FALSE, options = list(pageLength = 25), callback = dt_js_callback)

  output$table_download <- downloadHandler(
    filename = function() {
      dispID_cur <- dispID_from_res(input$select_resolution)
      return(sprintf("%s_table_%s.tsv", dispID_cur,
                     format(input$table_date, "%Y-%m-%d")))
    },
    content = function(file) {
      fwrite(rt_table_render(), file = file, sep = "\t")
    }
  )


  ########################################################################
  ## Housekeeping stuff
  ########################################################################

  # trigger bookmarking in URL every time an input changes
  # source: https://shiny.rstudio.com/articles/bookmarking-state.html
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })

  # don't remember the following parameters
  setBookmarkExclude(c("store", "map_main_groups",
                       "Rt_table_state", "sidebarItemExpanded",
                       "sidebarCollapsed",
                       "Rt_table_rows_selected", "Rt_table_columns_selected",
                       "remote_addr",
                       "Rt_table_cell_clicked",
                       "toggle_more", "reset_plot",
                       "Rt_table_rows_current",
                       "Rt_table_row_last_clicked",
                       "Rt_table_cells_selected",
                       "Rt_table_rows_all",
                       "table_reset", "table_cols",
                       "map_latest", "map_2week",
                       "map_1month", "map_2month",
                       "map_main_center", "map_main_zoom", "map_main_bounds",
                       "map_main_shape_click",
                       "map_main_shape_mouseover",
                       "map_main_shape_mouseout"))


  # Heroku disconnects the user from RShiny after 60 seconds of inactivity. Use
  # this to allow the user to be automatically connected
  session$allowReconnect("force")

}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
