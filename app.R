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
library(shinydashboard)
library(leaflet)
library(sf)
library(DT)
library(digest)
library(ggplot2)
library(cowplot)
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
colors_cases <- brewer.pal(7, "YlOrRd")

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

# get range of possible dates
dates <- unique(rt_long_all$date)
min_date <- min(dates)
max_date <- max(dates)

resolution_uniq <- unique(rt_long_all$resolution)
resolution_names <- data.table(orig = resolution_uniq)
resolution_names[, c("resolution", "country") := tstrsplit(orig, "_", fixed = TRUE)]
resolution_names[, pretty := fcase(orig == "country", "World",
                                   orig == "subnat_USA", "US States",
                                   orig == "county", "All US Counties",
                                   startsWith(orig, "subnat"),
                                   paste0(country, " Subnational"))]
setorderv(resolution_names, cols = "country")

resolution_choices <- list("World" = "country")
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
      ret <- list()
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

#' Preprocess data frame to be displayed by DT.
munge_for_dt <- function(df) {
  df_subset <- df[, .(dispID, positive, death, rt, rt_lower, rt_upper,
                      case_rate, case_lower, case_upper, death_rate,
                      death_lower, death_upper)]
  numeric_cols <- c("rt", "rt_lower", "rt_upper", "case_rate", "case_lower",
                    "case_upper", "death_rate", "death_lower", "death_upper")
  df_subset[, (numeric_cols) := lapply(.SD, round, digits = 2),
            .SDcols = numeric_cols]
  colnames(df_subset) <- c("Location", "Total Cases", "Total Deaths", "Rt", "Rt
                           CI Lwr", "Rt CI Upr", "Case Rate", "Case Lwr",
                           "Case Upr", "Death Rate", "Death Lwr", "Death Upr")
  setorderv(df_subset, cols = "Case Rate", order = -1, na.last = TRUE)
  return(df_subset)
}

#' Draw forest plot of current Rt and confidence interval
forest_plot <- function(df, resolution, date_lag,
                        metric = c("rt", "case", "death")) {
  metric <- match.arg(metric)
  plt_params <- switch(metric,
    "rt" = list(bins = bins_rt, var = "rt", lwr = "rt_lower", upr = "rt_upper",
                labels = rt_color_labels, color = colors_rt,
                title_str = "Rt"),
    "case" = list(bins = bins_cases, var = "case_rate", lwr = "case_lower",
                  upr = "case_upper", labels = cases_color_labels,
                  color = colors_cases,
                  title_str = "Case Rate per Million"),
    "death" = list(bins = bins_deaths, var = "death_rate", lwr = "death_lower",
                   upr = "death_upper", labels = deaths_color_labels,
                   color = colors_cases,
                   title_str = "Death Rate per Million")
  )
  df$range <- cut(df[[plt_params$var]], breaks = plt_params$bins,
                  labels = plt_params$labels, include.lowest = TRUE,
                  right = FALSE)

  # configure colors for Rt bins
  color_pal <- plt_params$color
  names(color_pal) <- levels(df$range)

  # change e.g. Texas, USA -> Texas; Kings, New York -> Kings
  setorderv(df, cols = plt_params$var, order = 1, na.last = TRUE)
  if (resolution == "county" || startsWith(resolution, "subnat_")) {
    df[, dispID_new := sub(", [A-Za-z ]+", "", dispID)]
    df[, dispID_ord := factor(dispID_new, levels = dispID_new)]
  } else {
    df[, dispID_ord := factor(dispID, levels = dispID)]
  }
  plt_df <- df[!is.na(get(plt_params$var)), ]
  if (nrow(plt_df) == 0) {
    # quit if there's no data
    p <- ggplot() +
      ggtitle("Insufficient data") +
      theme_dark() +
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
  title_str <- sprintf("%s on %s", plt_params$title_str, date_lag)
  xlab_str <- sprintf("%s and 95%% CI", plt_params$title_str)
  p <- ggplot(plt_df,
        aes_string(x = plt_params$var, y = "dispID_ord",
                    xmin = plt_params$lwr, xmax = plt_params$upr,
                    color = "range")) +
    geom_point(size = 3) + geom_errorbarh(size = 2) +
    scale_color_manual(drop = FALSE, values = color_pal) +
    xlab(xlab_str) + ylab("") +
    ggtitle(title_str) +
    theme_dark() +
    #coord_cartesian(xlim = c(0, 5)) +
    theme(axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          axis.title = element_text(size = 20),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          plot.title = element_text(size = 24),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.background = element_rect(fill = '#9e9e9e',
                                          colour = '#9e9e9e'))
  if (metric == "rt") {
    p <- p + geom_vline(xintercept = 1, lty = 2, color = "white", lwd = 1.5)
  }
  return(p)
}

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
    theme_cowplot() +
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

########################################################################
## Define UI
########################################################################

ui <- function(req) {
  dashboardPage(
  dashboardHeader(title = "Visualizing COVID-19 Spread Metrics",
                  titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "Map"),
      menuItem("Compare Rt", tabName = "compare_rt"),
      menuItem("Forest Plot", tabName = "forest_plot"),
      menuItem("Table", tabName = "table"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tags$head(tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}")),
    tags$head(tags$style(type = "text/css", "body {font-size: 16px} .aboutpage {font-size: 18px}")),
    tags$head(includeHTML("assets/google-analytics.html")),
    tags$head(tags$style(
        ".leaflet .legend {text-align: left;}",
        ".leaflet .legend i{float: left;}",
        ".leaflet .legend label{float:left; text-align: left;}"
    )),
    initStore("store", "shinyStore"),
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
            p("Use slider to adjust date. Click on an area to see its Rt over time."),
            p("Click play button to animate Rt over time."),
            p("Note the Rt is lagged by 5 days."),
          ),
          fluidRow(
            column(width = 4,
              sliderInput("map_date", label = "Date",
                          min = min_date, max = max_date,
                          value = max_date,
                          animate = animationOptions(interval = 3000))
            ), # end of column 1
            column(width = 4,
              radioButtons("map_metric", "Metric:",
                          choices = list("Rt (effective reproduction number)" = "rt",
                                          "Daily new cases per million" = "case",
                                          "Daily new deaths per million" = "death")),
            ), # end of column 2
            column(width = 4,
              selectInput("select_resolution", "Resolution:",
                          choices = resolution_choices,
                          selected = "subnat_USA")
            ) # end of column 3
          ), # end of fluidRow 1
          fluidRow(
            column(8, leafletOutput("map_main", height = "600px", width = "100%")),
            # plot of Rt over time
            column(4, plotOutput("map_click_plot", height = "600px"))
          ), # end of fluidRow 2
          # TODO: Testing out location detection
          fluidRow(
            textOutput("ip_addr")
          ),
        ), # end of tabItem
        # Second tab: Compare Rt across different regions.
        tabItem("compare_rt",
          fluidPage(
            column(width = 4,
              h4("Select areas to compare their Rt."),
              p("Note the Rt is lagged by 5 days."),
              p("Some areas may not appear in the plot for all time points because of insufficient data."),
              p("Occasionally, locations may have negative values for new cases because of reporting issues."),
              # break up the selection by state, county, and country
              selectizeInput("compare_sel_states", label = "States/Provinces",
                            choices = place_choices$state,
                            multiple = TRUE),
              selectizeInput("compare_sel_counties", label = "Counties (US)",
                            choices = place_choices$county,
                            multiple = TRUE),
              selectizeInput("compare_sel_countries", label = "Countries",
                            choices = place_choices$country,
                            multiple = TRUE),
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
              actionButton("compare_submit", label = "Submit")
            ), # end of column
            column(width = 8, uiOutput("compare_plt_ui"))
          ) # end of fluidPage
        ), # end of tabItem
        # 3rd tab: Forest plot of Rts
        tabItem("forest_plot",
          sidebarLayout(
            sidebarPanel(
              h4("Display a forest plot of a metric for a given resolution"),
              p("Use slider to adjust date."),
              p("Note the Rt is lagged by 5 days."),
              sliderInput("forestPlot_date", label = "Date",
                          min = min_date, max = max_date,
                          value = max_date),
              radioButtons("forestPlot_metric", "Metric:",
                          choices = list("Rt (effective reproduction number)" = "rt",
                                          "Daily new cases per million" = "case",
                                          "Daily new deaths per million" = "death")),
              selectInput("forestPlot_resolution", "Resolution:",
                          choices = resolution_choices_noallus,
                          selected = "subnat_USA")
            ), # end of sidebarPanel
            mainPanel(
              # Need UI output to dynamically set the size of the plots.
              uiOutput("RtForestPlot_ui"),
              p("Some locations might not be shown because of insufficient data.")
            ) # end of mainPanel
          ) # end of sidebarLayout
        ), # end of tabItem
        # 4th tab: table of Rts and other metrics
        tabItem("table",
          # TODO: Add column selector so user can pick which columns they want
          # to see.
          fluidRow(
            box(width = 6,
              sliderInput("table_date", label = "Date",
                          min = min_date, max = max_date,
                          value = max_date)
            ), # end of box 1
            box(width = 6,
              selectInput("table_select_resolution", "Resolution:",
                          choices = resolution_choices,
                          selected = "subnat_USA")
            ) # end of box 2
          ), # end of fluidRow 1
          fluidRow(
              DT::DTOutput("Rt_table"),
              br(),
              includeMarkdown("assets/Rt_table_footer.md")
          ) # end of fluidRow 2
        ), # end of tabItem for table
        # last tab: About page
        tabItem("about",
          withTags({
            div(class = "aboutpage",
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

  loc_info <- reactive({
    ipaddr <- strsplit(isolate(input$remote_addr), ", ", fixed = TRUE)[[1]][1]
    if (is.null(input$store$loc_info)) {
      ipinfo <- query_ip(ipaddr)
      updateStore(session, "loc_info", ipinfo)
    }
    input$store$loc_info
  })


  ########################################################################
  ## 1st tab: Big Rt Map
  ########################################################################

  # The basic big Rt map
  output$map_main <- renderLeaflet({
    suppressWarnings(
      leaflet(options = list(worldCopyJump = TRUE)) %>%
        setView(0, 30, 2) %>%
        setMaxBounds(-180, -90, 180, 90) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(minZoom = 1,
                                                       noWrap = TRUE)) %>%
        addLegend(colors = colors_rt, labels = rt_color_labels,
                  opacity = 0.7, title = "Rt", position = "bottomleft",
                  layerId = "legend")
    )
  })

  # change the zoom level only when the resolution changes.
  # update the data based on values
  sf_dat_update <- reactive({
    shiny::validate(need(input$map_date, "Please select a date."))
    shiny::validate(need(input$select_resolution, "Please select a resolution."))
    shiny::validate(need(input$map_metric, "Please select a metric."))

    # if selected resolution starts with 840 is 630 it's a US county
    if (startsWith(input$select_resolution, "840") || input$select_resolution == "630") {
      resolution <- "county"
      state_uid <- input$select_resolution
    } else {
      resolution <- input$select_resolution
      state_uid <- NULL
    }
    sf_by_date_res(input$map_date, metric = input$map_metric,
                   sel_resolution = resolution, state_uid = state_uid)
  })

  observe({
    # set a reactive dependency on select_resolution, but not on sf_dat_update
    res <- input$select_resolution
    isolate({
      sf_dat_cur <- sf_dat_update()
    })

    # by default, use bounding box, but these are exceptions where the bounding
    # box doesn't work
    cur_view <- switch(res,
                       "subnat_USA" = c(-96, 37.8, 4),
                       "county" = c(-96, 37.8, 4),
                       "subnat_Canada" = c(-100.78, 51.52, 3),
                       "84000002" = c(-147, 61.31395, 4),
                       NULL)

    if (is.null(cur_view)) {
      bbox <- as.numeric(st_bbox(sf_dat_cur))
      suppressWarnings(
        leafletProxy("map_main", session) %>%
          fitBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3],
                    lat2 = bbox[4])
      )
    } else {
      suppressWarnings(
        leafletProxy("map_main", session) %>%
          setView(cur_view[1], cur_view[2], cur_view[3])
      )
    }

  })


  # keep track of previous group ID so we can clear its shapes
  prev_grpid_all_reactive <- reactiveValues(val = "")

  # change the shapes on the map when the resolution or date changes
  observe({
    date_select <- format(input$map_date, "%Y-%m-%d")
    sel_resolution <- input$select_resolution
    shiny::validate(need(date_select, "Please select a date."))
    shiny::validate(need(sel_resolution, "Please select a resolution."))
    shiny::validate(need(input$map_metric, "Please select a metric."))
    sf_dat_cur <- sf_dat_update()
    labels_final <- master_labeller(sf_dat_cur, date_select, input$map_metric)
    cur_grpid <- digest::digest(c(date_select, sel_resolution,
                                  input$map_metric))
    prev_grpid <- prev_grpid_all_reactive$val
    map <- leafletProxy("map_main", data = sf_dat_cur)
    if (prev_grpid != cur_grpid) {
      suppressWarnings({
        map <- map %>%
          addPolygon_Point(sf_dat_cur, labels = labels_final,
                           metric = input$map_metric, grpid = cur_grpid) %>%
          clearGroup(prev_grpid)
      })
    }
    prev_grpid_all_reactive$val <- cur_grpid
    map
  })

  # change the legend when the map metric changes
  observe({
    shiny::validate(need(input$map_metric, "Please select a metric."))
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


  # Rt over time based on click
  output$map_click_plot <- renderCachedPlot({
    shiny::validate(need(input$map_main_shape_click,
                         message = "Click on a location to show Rt and new cases over time."))
    click <- input$map_main_shape_click
    plt_dat <- rt_long_all[UID == click$id, ]
    if (nrow(plt_dat) > 0) {
      suppressWarnings(click_plot(plt_dat))
    }
  }, cacheKeyExpr = { input$map_main_shape_click$id })

  output$ip_addr <- renderText({
    tryCatch({
      with(loc_info(),
          sprintf("lat: %0.2f, long: %0.2f, %s, %s",
                  latitude, longitude, place_str, ipaddr))
    }, error = function(e) {
      NULL
    })
  })

  ########################################################################
  ## 2nd tab: Compare Rt
  ########################################################################

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

  # generate the plot of Rt comparisons after user hits submit
  output$compare_plt_out <- renderPlot({
    cur_dat <- compare_plt_data()
    shiny::validate(need(nrow(cur_dat) > 0,
                  "Insufficient data for selected locations."))

    # use isolate() to avoid dependency on input$compare_metric; dependency is
    # handled by compare_plt_data() when user hits submit.
    metric_lst_touse <- isolate(input$compare_metric)

    plt_lst <- compare_plot(cur_dat, metric_lst = metric_lst_touse)
    plot_grid(plotlist = plt_lst, ncol = 1, align = "v", axis = "l")
  })

  output$compare_plt_ui <- renderUI({
    # need to take a dependency on compare_plt_data to trigger UI re-draw on
    # submit
    cur_dat <- compare_plt_data()
    metric_lst <- isolate(input$compare_metric)
    n_metrics <- length(metric_lst)
    plt_height <- 300 * max(1, n_metrics)

    plotOutput("compare_plt_out", height = plt_height)
  })

  ########################################################################
  ## 3rd tab: Rt Forest plot
  ########################################################################

  forestPlot_data <- reactive({
    shiny::validate(need(input$forestPlot_date, "Please select a date."))
    shiny::validate(need(input$forestPlot_metric, "Please select a metric."))
    shiny::validate(need(input$forestPlot_resolution, "Please select a resolution."))
    date_select <- format(input$forestPlot_date, "%Y-%m-%d")

    # if selected resolution starts with 840 is 630 it's a US county
    if (startsWith(input$forestPlot_resolution, "840") || input$forestPlot_resolution == "630") {
      county_uids <- get_county_uids(input$forestPlot_resolution)
      ret <- rt_long_all[resolution == "county" & date == date_select &
                         ((UID > county_uids$uid_lwr & UID < county_uids$uid_upr) |
                           UID %in% county_uids$extra_uids), ]
    } else {
      ret <- rt_long_all[resolution == input$forestPlot_resolution &
                         date == date_select, ]
    }
    ret
  })

  output$RtForestPlot <- renderCachedPlot({
    shiny::validate(need(input$forestPlot_date, "Please select a date."))
    shiny::validate(need(input$forestPlot_metric, "Please select a metric."))
    shiny::validate(need(input$forestPlot_resolution, "Please select a resolution."))
    date_select <- format(input$forestPlot_date, "%Y-%m-%d")

    # set resolution to county if we get a state UID
    resolution <- input$forestPlot_resolution
    if (startsWith(input$forestPlot_resolution, "840") || input$forestPlot_resolution == "630") {
      resolution <- "county"
    }

    forest_plot(forestPlot_data(), resolution, date_select,
                metric = input$forestPlot_metric)
  }, cacheKeyExpr = { list(input$forestPlot_date, input$forestPlot_metric,
                           input$forestPlot_resolution) })

  output$RtForestPlot_ui <- renderUI({
    shiny::validate(need(input$forestPlot_date, "Please select a date."))
    shiny::validate(need(input$forestPlot_metric, "Please select a metric."))
    shiny::validate(need(input$forestPlot_resolution, "Please select a resolution."))
    column_select <- switch(input$forestPlot_metric,
                            "rt" = "rt",
                            "case" = "case_rate",
                            "death" = "death_rate")
    num_rows <- sum(!is.na(forestPlot_data()[, ..column_select]))
    plt_height <- sprintf("%dpx", max(20 * num_rows, 300))
    plotOutput("RtForestPlot", height = plt_height)
  })

  output$Rt_table_title <- renderText({
    date_actual <- format(input$map_date, "%Y-%m-%d")
    req(date_actual)
    sprintf("Table of metrics for %s.", date_actual)
  })

  ########################################################################
  ## 4th tab: Table of Rts and other metrics
  ########################################################################

  # Table of current Rts at current resolution
  output$Rt_table <- DT::renderDT({
    date_select <- format(input$table_date, "%Y-%m-%d")
    sel_resolution <- input$table_select_resolution
    shiny::validate(need(date_select, "Please select a date."))
    shiny::validate(need(sel_resolution, "Please select a resolution."))

    # if selected resolution starts with 840 is 630 it's a US county
    if (startsWith(input$table_select_resolution, "840") || input$table_select_resolution == "630") {
      county_uids <- get_county_uids(input$table_select_resolution)
      ret_df <- rt_long_all[resolution == "county" & date == date_select &
                           ((UID > county_uids$uid_lwr & UID < county_uids$uid_upr) |
                             UID %in% county_uids$extra_uids), ]
    } else {
      ret_df <- rt_long_all[resolution == input$table_select_resolution &
                            date == date_select, ]
    }

    shiny::validate(need(nrow(ret_df) > 0, "This data has no rows."))
    munge_for_dt(ret_df)
  }, server = FALSE, options = list(pageLength = 25), callback = dt_js_callback)

  # TODO: Put this on the main map page
  # render heatmap of counties over time
  output$explore_states_counties <- renderCachedPlot({
    shiny::validate(need(input$state_select, message = "Please select a state."))
    plt_data_pruned <- county_rt_long_update() %>%
      dplyr::filter(Rt_plot > 0) %>%
      dplyr::mutate(`Rt Range` = cut(Rt_plot, breaks = bins[-c(1, 2)],
                                    labels = color_labels[-c(1, 2)],
                                    include.lowest = TRUE, right = FALSE),
                    dispID_new = sub(", [A-Za-z ]+", "", dispID),
                    County = factor(dispID_new)) %>%
      dplyr::rename(Rt = Rt_plot)
    color_pal <- colors_default[-c(1, 2)]
    names(color_pal) <- levels(plt_data_pruned$`Rt Range`)

    plt_title <- sprintf("Rt for %s Counties Over Time",
                         state_uid_to_place[[input$state_select]])
    if (nrow(plt_data_pruned) > 0) {
      p <- plt_data_pruned %>%
        ggplot(aes(x = date, y = County, fill = `Rt Range`, labels = Rt)) +
        geom_tile() +
        scale_fill_manual(drop = FALSE, values = color_pal) +
        xlab("Date (lagged 5 days)") + ylab("County") + ggtitle(plt_title) +
        theme_dark() +
        scale_y_discrete(limits = rev(levels(plt_data_pruned$County))) +
        theme(axis.text.y = element_text(size = 16),
              axis.text.x = element_text(size = 16),
              axis.title = element_text(size = 20),
              legend.title = element_text(size = 18),
              legend.text = element_text(size = 16),
              plot.title = element_text(size = 24),
              panel.background = element_rect(fill = '#9e9e9e',
                                              colour = '#9e9e9e'))
    } else {
      # draw empty plot
      p <- ggplot()
    }
    p
  }, cacheKeyExpr = { input$state_select })

  # Heroku disconnects the user from RShiny after 60 seconds of inactivity. Use
  # this to allow the user to be automatically connected
  session$allowReconnect("force")

}

# Run the application
shinyApp(ui = ui, server = server)
