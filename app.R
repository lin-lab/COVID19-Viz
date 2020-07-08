#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(DT)
library(digest)
library(ggplot2)
library(cowplot)
library(htmltools)
library(purrr)
library(dplyr)

########################################################################
## Load data files
########################################################################

# shape file: wide data that has 1 row per location with all Rts, Rt CI's, and
# shapes
sf_all <- readRDS("clean_data/sf_all.rds")

# long data frame of Rts
rt_long_all <- readRDS("clean_data/rt_long_all.rds")

# choices for each place
place_choices <- readRDS("clean_data/names_list.rds")

# state centers
state_centers <- readRDS("clean_data/state_centers.rds")

# map state UIDs to place name
state_uid_to_place <- as.list(names(place_choices$us_state))
names(state_uid_to_place) <- unlist(place_choices$us_state, use.names = FALSE)

########################################################################
## Define globals
########################################################################

# bins and colors for the map
bins <- c(-Inf, -100, 0, 0.5, 0.75, 1.0, 1.25, 1.5, 2, Inf)
rt_range <- range(rt_long_all$Rt_plot, na.rm = TRUE)
colors_default <- c("#696969", "#9e9e9e",
                    rev(c('#d73027','#fc8d59','#fee090','#ffffbf','#e0f3f8',
                          '#91bfdb','#4575b4')))
color_labels <- c("Insufficient total cases", "Insufficient new cases",
                  "0.00 - 0.50", "0.50 - 0.75", "0.75 - 1.00", "1.00 - 1.25",
                  "1.25 - 1.50", "1.50 - 2.00", "2+")
pal_default <- purrr::partial(colorBin, palette = colors_default, bins = bins)

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
dates <- unique(rt_long_all$date_lag)
min_date <- min(dates)
max_date <- max(dates)

# This javascript callback re-numbers DT::datatables rows after sorting.
# Source: https://stackoverflow.com/questions/35502931/automatic-row-numbers-after-filtering-dt-in-shiny
dt_js_callback = JS("table.on( 'order.dt search.dt', function () {
                                table.column(0, {search:'applied', order:'applied'}).nodes().each( function (cell, i) {
                                      cell.innerHTML = i+1;});}).draw();")

########################################################################
## Define helper functions
########################################################################

#' Generate labels for Rt.
#'
#' @param sf_dat An sf object with columns "Rt", "Rt_lwr", "Rt_upr", "UID",
#' "dispID", "geometry"
rt_labeller <- function(sf_dat) {
  stopifnot(names(sf_dat) == c("Rt", "Rt_lwr", "Rt_upr", "UID",
                               "dispID", "geometry"))
  rt_col <- sf_dat[["Rt"]]
  total_case_idx <- which(rt_col <= -100)
  new_case_idx <- which(0 > rt_col & rt_col > -100)
  rt_idx <- which(rt_col >= 0)
  rt_str <- rep(NA, length(rt_col))
  rt_str[total_case_idx] <- "Insufficient total cases"
  rt_str[new_case_idx] <- "Insufficient new cases"
  rt_str[rt_idx] <- sprintf("Rt: %0.2f (%0.2f - %0.2f)",
                            rt_col[rt_idx],
                            sf_dat$Rt_lwr[rt_idx],
                            sf_dat$Rt_upr[rt_idx])
  return(rt_str)
}

#' Generate labels for the map
#'
#' @param sf_dat An sf object with columns "Rt", "Rt_lwr", "Rt_upr", "UID",
#' "dispID", "geometry"
#' @param date_select The selected date
master_labeller <- function(sf_dat, date_select) {
  stopifnot(names(sf_dat) == c("Rt", "Rt_lwr", "Rt_upr", "UID",
                               "dispID", "geometry"))
  rt_labels <- rt_labeller(sf_dat)
  labels_final <- sprintf("<strong>%s</strong><br/>Date: %s<br/>%s",
                          sf_dat$dispID, date_select, rt_labels) %>%
      lapply(htmltools::HTML)
  return(labels_final)
}

#' Get UIDs for all counties in a state, given a state UID.
#'
#' @param state_uid_str State UID as a string.
get_county_uids <- function(state_uid_str) {
  # MA, UT, MO have extra counties
  extra_states <- c("84000025", "84000049", "84000029")
  state_id <- as.numeric(state_uid_str) %% 100

  # based on the UID lookup table logic from JHU data
  state_uid_lwr <- 84000000 + state_id * 1e3
  state_uid_upr <- state_uid_lwr + 1e3
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
sf_by_date_res <- function(date_select,
                           sel_resolution = c("state_USA", "state_Canada",
                                              "state_China", "state_Australia",
                                              "county", "country"),
                           state_uid = NULL) {
  sel_resolution <- match.arg(sel_resolution)
  date_str <- format(date_select, "%Y-%m-%d")
  rt_col <- paste0("Rt_plot_", date_str)
  rtlwr_col <- paste0("Rt_lwr_", date_str)
  rtupr_col <- paste0("Rt_upr_", date_str)
  select_cols <- c(rt_col, rtlwr_col, rtupr_col, "UID", "dispID")

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
  names(ret_sf) <- c("Rt", "Rt_lwr", "Rt_upr", "UID", "dispID", "geometry")
  return(ret_sf)
}

#' Add polygons and points to a leaflet object.
#'
#' @param .map A leaflet object
#' @param data An sf object containing the geometries to put on the map.
#' @param labels Labels for each row in the sf data.
#' @param grpid Group ID for the added polygons and points. Allows one to use
#' clearGroup to remove the added polygons and points.
addPolygon_Point <- function(.map, .data, labels, grpid = "default") {
  pal <- pal_default(domain = .data$Rt)
  stopifnot(names(.data) == c("Rt", "Rt_lwr", "Rt_upr", "UID", "dispID",
                             "geometry"))
  polygon_idx <- st_is(.data, "POLYGON") | st_is(.data, "MULTIPOLYGON")
  data_polygons <- .data[polygon_idx, ]
  data_points <- .data[!polygon_idx, ]
  map_ret <- addPolygons_default(.map, data = data_polygons,
                                 group = grpid,
                                 fillColor = ~pal(Rt), layer = ~UID,
                                 label = labels[polygon_idx])

  if (sum(polygon_idx) < nrow(.data)) {
    map_ret <- addCircles_default(map_ret, data = data_points,
                                  group = grpid,
                                  fillColor = ~pal(Rt), layer = ~UID,
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
  ylim_max <- ceiling(max(plt_dat$Rt_upr))
  xlim_max <- max(plt_dat$date)
  xlim_min <- min(plt_dat$date_lag)
  rt_plt <- plt_dat %>%
    ggplot(aes(x = date_lag)) +
    geom_ribbon(aes(ymin = Rt_loess_lwr, ymax = Rt_loess_upr, y = Rt_loess_fit),
                fill = "#d6d6d6") +
    geom_point(aes(y = Rt_plot), color = "black") +
    geom_errorbar(aes(ymin = Rt_lwr, ymax = Rt_upr, y = Rt_plot), color = "black") +
    geom_line(aes(y = Rt_loess_fit), colour = "#3366ff", lwd = 1.5) +
    coord_cartesian(ylim = c(0, ylim_max)) +
    geom_hline(yintercept = 1, lty = 2) +
    xlab("Date (lagged 5 days)") + ylab("") + ggtitle(rt_plt_title) +
    xlim(xlim_min, xlim_max) +
    theme_cowplot() +
    background_grid(major = "xy", minor = "xy") +
    theme(text = element_text(size = 18),
          axis.text = element_text(size = 15))

  newcases_plt_title <- sprintf("New Cases for %s", place_name)
  newcases_plt <- plt_dat %>%
    select(date, positiveIncrease, positive_7day) %>%
    ggplot(aes(x = date)) +
    geom_line(aes(y = positive_7day, linetype = "positive_7day")) +
    geom_line(aes(y = positiveIncrease, linetype = "positiveIncrease")) +
    scale_linetype_discrete(name = "",
                            breaks = c("positive_7day", "positiveIncrease"),
                            labels = c("7-day average of new cases",
                                       "Daily new cases")) +
    xlab("Date") + ylab("") + ggtitle(newcases_plt_title) +
    theme_cowplot() +
    xlim(xlim_min, xlim_max) +
    coord_cartesian(ylim = c(0, NA)) +
    background_grid(major = "xy", minor = "xy") +
    theme(text = element_text(size = 18),
          axis.text = element_text(size = 15),
          legend.position = "bottom") +
    guides(linetype = guide_legend(nrow = 2))
  final_plt <- plot_grid(rt_plt, newcases_plt, nrow = 2, align = "v",
                          axis = "l")
  return(final_plt)
}

#' Preprocess data frame to be displayed by DT.
munge_for_dt <- function(df) {
  ret <- df %>%
    dplyr::mutate(Rt = ifelse(Rt_plot > 0, round(Rt_plot, 2), NA),
                  Rt_lwr = round(Rt_lwr, 2),
                  Rt_upr = round(Rt_upr, 2),
                  positive_percapita = round(positive_percapita, 0),
                  positiveIncr_percapita = round(positiveIncr_percapita, 2),
                  death_percapita = round(death_percapita, 0),
                  deathIncr_percapita = round(deathIncr_percapita, 2)) %>%
    dplyr::select(Location = dispID, Rt,
                  `CI Lower` = Rt_lwr,
                  `CI Upper` = Rt_upr,
                  `New Cases` = positiveIncrease,
                  `New Cases per Million` = positiveIncr_percapita,
                  `Cum. Cases` = positive,
                  `Cum. Cases per Million` = positive_percapita,
                  `New Deaths` = deathIncrease,
                  `New Deaths per Million` = deathIncr_percapita,
                  `Cum. Deaths` = death,
                  `Cum. Deaths per Million` = death_percapita) %>%
    dplyr::arrange(desc(Rt), desc(`Cum. Cases`))
  return(ret)
}

#' Draw forest plot of current Rt and confidence interval
forest_plot <- function(df, resolution, date_lag) {
  bins_pos <- bins[-c(1, 2)]
  color_labels_pos <- color_labels[-c(1, 2)]
  tmp <- df %>%
    dplyr::filter(Rt_plot > 0) %>%
    dplyr::arrange(Rt_plot) %>%
    dplyr::mutate(`Rt Range` = cut(Rt_plot, breaks = bins_pos,
                                  labels = color_labels_pos,
                                  include.lowest = TRUE, right = FALSE))
  # configure colors for Rt bins
  color_pal <- colors_default[-c(1, 2)]
  names(color_pal) <- levels(tmp$`Rt Range`)

  # change e.g. Texas, USA -> Texas; Kings, New York -> Kings
  if (resolution == "county" || startsWith(resolution, "state_")) {
    plt_dat <- tmp %>%
      dplyr::mutate(dispID_new = sub(", [A-Za-z ]+", "", dispID),
                    dispID_ord = factor(dispID_new, levels = dispID_new))
  } else {
    plt_dat <- tmp %>%
      dplyr::mutate(dispID_ord = factor(dispID, levels = dispID))
  }

  if (nrow(plt_dat) == 0) {
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
  } else {
    title_str <- sprintf("Rt on %s", date_lag)
    p <- ggplot(plt_dat,
          aes(x = Rt_plot, y = dispID_ord, xmin = Rt_lwr, xmax = Rt_upr,
              color = `Rt Range`)) +
      geom_point(size = 3) + geom_errorbarh(size = 2) +
      scale_color_manual(drop = FALSE, values = color_pal) +
      geom_vline(xintercept = 1, lty = 2, color = "white", lwd = 1.5) +
      xlab("Rt and 95% CI") + ylab("") +
      ggtitle(title_str) +
      theme_dark() +
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
  }
  return(p)
}


########################################################################
## Define UI
########################################################################

ui <- fluidPage(
  tags$head(tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}")),
  tags$head(tags$style(type = "text/css", "body {font-size: 16px} .aboutpage {font-size: 18px}")),
  tags$head(includeHTML("assets/google-analytics.html")),
  tags$head(tags$style(
      ".leaflet .legend {width:200px; text-align: left;}",
      ".leaflet .legend i{float: left;}",
      ".leaflet .legend label{float:left; text-align: left;}"
  )),
  titlePanel("Visualizing COVID-19's Effective Reproduction Number (Rt)"),
  tabsetPanel(
    # first panel: big Rt map with multiple resolutions.
    tabPanel("Rt Map",
      sidebarLayout(
        sidebarPanel(
          p("Use slider to adjust date. Click on an area to see its Rt over time."),
          p("Click play button to animate Rt over time."),
          p("Note the Rt is lagged by 5 days."),
          sliderInput("RtDate", label = "Date",
                      min = min_date, max = max_date,
                      value = max_date,
                      animate = animationOptions(interval = 3000)),
          selectInput("select_resolution", "Resolution:",
                      choices = list("World" = "country",
                                     "US States" = "state_USA",
                                     "US Counties" = "county",
                                     "Canadian Provinces" = "state_Canada",
                                     "Australian Provinces" = "state_Australia",
                                     "Chinese Provinces" = "state_China")),
          # plot of Rt over time
          plotOutput("RtOverTime", height = "600px")
        ), # end of sidebarPanel
        mainPanel(
          leafletOutput("RtMap", height = "80vh", width = "100%"),
          h4(textOutput("Rt_table_title")),
          DT::DTOutput("Rt_table"),
          br(),
          includeMarkdown("assets/Rt_table_footer.md")
        )
      ) # end of sidebarLayout
    ), # end of tabPanel
    # Second tab: Compare Rt across different regions.
    tabPanel("Compare Rt",
      sidebarLayout(
        sidebarPanel(
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
          actionButton("compare_submit", label = "Submit")
        ),
        mainPanel(
          plotOutput("compare_plt_out", height = "1500px", width = "100%")
        )
      ) # end of sideBarLayout
    ), # end of tabPanel
    # 3rd tab: Forest plot of Rts
    tabPanel("Forest Plot",
      sidebarLayout(
        sidebarPanel(
          h4("Display a forest plot of Rt for a given resolution"),
          p("Use slider to adjust date."),
          p("Note the Rt is lagged by 5 days."),
          sliderInput("forestPlot_date", label = "Date",
                      min = min_date, max = max_date,
                      value = max_date),
          selectInput("forestPlot_resolution", "Resolution:",
                      choices = list("World" = "country",
                                     "US States" = "state_USA",
                                     "Canadian Provinces" = "state_Canada",
                                     "Australian Provinces" = "state_Australia",
                                     "Chinese Provinces" = "state_China"))
        ), # end of sidebarPanel
        mainPanel(
          # Need UI output to dynamically set the size of the plots.
          uiOutput("RtForestPlot_ui"),
          p("Some locations might not be shown because of insufficient data.")
        ) # end of mainPanel
      ) # end of sidebarLayout
    ),
    # 4th tab: explore states
    tabPanel("Explore States",
      # controls at the top
      fluidRow(
        column(6,
          h4("Select a state to explore."),
          selectizeInput("state_select", label = "State",
                         selected = "84000025",
                         choices = place_choices$us_states_w_counties,
                         multiple = FALSE)
        ),
        column(6, align = "center",
          p("Note the Rt is lagged by 5 days."),
          sliderInput("state_select_date", label = "Select date",
                      min = min_date, max = max_date,
                      value = max_date, animate = TRUE)
        )
      ),
      # output at the bottom
      fluidRow(
        # These guys can have fixed height
        column(6, align = "center",
          plotOutput("RtOverTime_exploreState", height = "500px")
        ),
        column(6, align = "center",
          leafletOutput("explore_states_out", height = "500px")
        )
      ),
      uiOutput("explorestate_row2_ui"),
      fluidRow(
        h4(textOutput("Rt_table_explore_states_title")),
        DT::DTOutput("Rt_table_explore_states"),
        br(),
        includeMarkdown("assets/Rt_table_footer.md")
      )
    ), # end of tabPanel
    # last tab: About page
    tabPanel("About",
      withTags({
        div(class = "aboutpage",
          includeMarkdown("assets/about.md")
        )
      }) # end of withTags
    ) # end of tabPanel
  ) # end of tabsetPanel
) # end of fluidPage

########################################################################
## Define Server function
########################################################################

server <- function(input, output, session) {
  cdata <- session$clientData

  ########################################################################
  ## 1st tab: Big Rt Map
  ########################################################################

  # The basic big Rt map
  output$RtMap <- renderLeaflet({
    suppressWarnings(
      leaflet(options = list(worldCopyJump = TRUE)) %>%
        setView(0, 30, 2) %>%
        setMaxBounds(-180, -90, 180, 90) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(minZoom = 1,
                                                       noWrap = TRUE)) %>%
        addLegend(colors = colors_default, labels = color_labels,
                  opacity = 0.7, title = "Rt", position = "bottomleft")
    )
  })

  # change the zoom level only when the resolution changes.
  observe({
    sel_resolution <- input$select_resolution
    cur_view <- switch(sel_resolution,
      "state_USA" = c(-96, 37.8, 4),
      "state_Canada" = c(-100.78, 51.52, 3),
      "state_China" = c(104.4, 34.7, 4),
      "state_Australia" = c(135.5, -26.1, 4),
      "county" = c(-96, 37.8, 4),
      "country" = c(0, 30, 2)
    )
    suppressWarnings(
      leafletProxy("RtMap") %>%
        setView(cur_view[1], cur_view[2], cur_view[3])
    )
  })

  # update the data based on values
  sf_dat_update <- reactive({
    sel_resolution <- input$select_resolution
    validate(need(input$RtDate, "Please select a date."))
    validate(need(sel_resolution, "Please select a resolution."))
    sf_by_date_res(input$RtDate, sel_resolution)
  })

  # keep track of previous group ID so we can clear its shapes
  prev_grpid_all_reactive <- reactiveValues(val = "")

  # change the shapes on the map when the resolution or date changes
  observe({
    date_select <- format(input$RtDate, "%Y-%m-%d")
    sel_resolution <- input$select_resolution
    validate(need(date_select, "Please select a date."))
    validate(need(sel_resolution, "Please select a resolution."))
    sf_dat_cur <- sf_dat_update()
    labels_final <- master_labeller(sf_dat_cur, date_select)
    cur_grpid <- digest::digest(c(date_select, sel_resolution))
    prev_grpid <- prev_grpid_all_reactive$val
    map <- leafletProxy("RtMap", data = sf_dat_cur)
    if (prev_grpid != cur_grpid) {
      suppressWarnings({
        map <- map %>%
          addPolygon_Point(sf_dat_cur, labels_final, cur_grpid) %>%
          clearGroup(prev_grpid)
      })
    }
    prev_grpid_all_reactive$val <- cur_grpid
    map
  })

  # Rt over time based on click
  output$RtOverTime <- renderCachedPlot({
    validate(need(input$RtMap_shape_click,
                  message = "Click on a location to show Rt and new cases over time."))
    click <- input$RtMap_shape_click
    plt_dat <- dplyr::filter(rt_long_all, UID == click$id)
    if (nrow(plt_dat) > 0) {
      suppressWarnings(click_plot(plt_dat))
    }
  }, cacheKeyExpr = { input$RtMap_shape_click$id })

  output$Rt_table_title <- renderText({
    date_lag <- format(input$RtDate, "%Y-%m-%d")
    date_actual <- format(input$RtDate + 5, "%Y-%m-%d")
    req(date_lag)
    sprintf("Table of metrics for %s. Rt calculated for %s (5-day lag).",
            date_actual, date_lag)
  })

  # Table of current Rts at current resolution
  output$Rt_table <- DT::renderDT({
    date_select <- format(input$RtDate, "%Y-%m-%d")
    sel_resolution <- input$select_resolution
    validate(need(date_select, "Please select a date."))
    validate(need(sel_resolution, "Please select a resolution."))

    ret_df <- rt_long_all %>%
      dplyr::filter(resolution == sel_resolution, date_lag == date_select,
                    positive >= 50) %>%
      munge_for_dt()
    validate(need(nrow(ret_df) > 0, "This data has no rows."))
    ret_df
  }, server = FALSE, options = list(pageLength = 25), callback = dt_js_callback)



  ########################################################################
  ## 2nd tab: Compare Rt
  ########################################################################

  # get data to plot for Rt comparison
  plt_dat_compare <- eventReactive(input$compare_submit, {
    # get all the UIDs in a data frame and join them with rt long to select
    selected_states <- input$compare_sel_states
    selected_counties <- input$compare_sel_counties
    selected_countries <- input$compare_sel_countries
    selected_uids <- data.frame(
      UID = as.double(c(selected_states, selected_counties, selected_countries))
    )

    req(nrow(selected_uids) > 0)

    dplyr::inner_join(rt_long_all, selected_uids, by = "UID")
  })

  # generate the plot of Rt comparisons after user hits submit
  output$compare_plt_out <- renderPlot({
    cur_dat <- plt_dat_compare() %>%
      dplyr::mutate(Rt_plot = case_when(
              Rt_plot < 0 ~ NA_real_,
              TRUE ~ Rt_plot))
    validate(need(nrow(cur_dat) > 0,
                  "Insufficient data for selected locations."))
    ylim_max <- ceiling(max(cur_dat$Rt_upr))
    xlim_max <- max(cur_dat$date)
    xlim_min <- min(cur_dat$date_lag)

    suppressWarnings({
      rt_plt <- cur_dat %>%
        ggplot(aes(x = date_lag, y = Rt_plot, color = dispID, fill = dispID)) +
        geom_ribbon(aes(ymin = Rt_lwr, ymax = Rt_upr), alpha = 0.2) +
        geom_line() + geom_point() + xlab("Date (lagged 5 days)") + ylab("Rt") +
        ggtitle("Comparison of Rt") + ylim(0, ylim_max) +
        xlim(xlim_min, xlim_max) +
        geom_hline(yintercept = 1, lty = 2) +
        scale_color_discrete(name = "Location") +
        scale_fill_discrete(name = "Location") +
        theme_cowplot() +
        theme(text = element_text(size = 18),
              axis.text = element_text(size = 15))

      newcases_plt <- cur_dat %>%
        ggplot(aes(x = date, y = positiveIncrease, color = dispID)) +
        geom_line() + geom_point() + xlab("Date") + ylab("New Cases") +
        ggtitle("Comparison of Daily New Cases") +
        xlim(xlim_min, xlim_max) +
        scale_color_discrete(name = "Location") +
        scale_fill_discrete(name = "Location") +
        theme_cowplot() +
        coord_cartesian(ylim = c(0, NA)) +
        theme(text = element_text(size = 18),
              axis.text = element_text(size = 15))

      newcases_percapita_plt <- cur_dat %>%
        ggplot(aes(x = date, y = positiveIncr_percapita, color = dispID)) +
        geom_line() + geom_point() + xlab("Date") + ylab("") +
        ggtitle("Comparison of New Cases per Million Population") +
        xlim(xlim_min, xlim_max) +
        scale_color_discrete(name = "Location") +
        scale_fill_discrete(name = "Location") +
        theme_cowplot() +
        coord_cartesian(ylim = c(0, NA)) +
        theme(text = element_text(size = 18),
              axis.text = element_text(size = 15))

      plt_out <- plot_grid(rt_plt, newcases_plt, newcases_percapita_plt,
                           ncol = 1, align = "v", axis = "l")
      plt_out
    })
  })

  ########################################################################
  ## 3rd tab: Rt Forest plot
  ########################################################################

  output$RtForestPlot <- renderCachedPlot({
    validate(need(input$forestPlot_date, "Please select a date."))
    validate(need(input$forestPlot_resolution, "Please select a resolution."))
    date_select <- format(input$forestPlot_date, "%Y-%m-%d")

    p <- rt_long_all %>%
      dplyr::filter(resolution == input$forestPlot_resolution,
                    date_lag == date_select) %>%
      forest_plot(input$forestPlot_resolution, date_select)
    p
  }, cacheKeyExpr = { c(input$forestPlot_date, input$forestPlot_resolution) })

  output$RtForestPlot_ui <- renderUI({
    validate(need(input$forestPlot_date, "Please select a date."))
    validate(need(input$forestPlot_resolution, "Please select a resolution."))
    plt_height <-
      switch(input$forestPlot_resolution,
             country = "1800px",
             state_USA = "900px",
             state_Canada = "300px",
             state_Australia = "300px",
             state_China = "300px")
    plotOutput("RtForestPlot", height = plt_height)
  })

  ########################################################################
  ## 4th tab: Explore states tab
  ########################################################################

  # change map polygons when state or date changes
  prev_grpid_state_reactive <- reactiveValues(val = "")

  # explore states map
  output$explore_states_out <- renderLeaflet({
    ma_uid <- "84000025"
    counties_sf <- sf_by_date_res(max_date, "county", ma_uid)
    labels_final <- master_labeller(counties_sf, max_date)
    pal <- pal_default(domain = counties_sf$Rt)
    cur_grpid <- digest::digest(c(max_date, ma_uid))
    suppressWarnings(
      map_default <- leaflet(options = list(worldCopyJump = FALSE)) %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(minZoom = 3)) %>%
        setView(-71.72, 42.06, 7) %>%
        setMaxBounds(-180, -90, 180, 90) %>%
        addPolygon_Point(counties_sf, labels_final, cur_grpid) %>%
        addLegend(colors = colors_default, labels = color_labels,
                  opacity = 0.7, title = "Rt", position = "bottomright")
    )
    prev_grpid_state_reactive$val <- cur_grpid
    map_default
  })

  # change zoom level only when state changes
  observe({
    state_input <- input$state_select
    zoom_level <- set_state_zoom(state_input)
    lnglat <- state_centers[[state_input]]
    leafletProxy("explore_states_out") %>%
      setView(lnglat[1], lnglat[2], zoom_level)
  })

  # change state county polygons when state or date changes
  observe({
    date_select <- format(input$state_select_date, "%Y-%m-%d")
    state_input <- input$state_select
    validate(need(input$state_select_date,
                  "Please choose a date using the slider."))
    validate(need(input$state_select, "Please choose a state."))

    counties_sf_cur <- sf_by_date_res(input$state_select_date, "county",
                                      state_input)
    labels_final <- master_labeller(counties_sf_cur, date_select)
    pal <- pal_default(domain = counties_sf_cur$Rt)
    cur_grpid <- digest::digest(c(date_select, state_input))
    prev_grpid <- prev_grpid_state_reactive$val
    map_state <- leafletProxy("explore_states_out", data = counties_sf_cur)
    if (prev_grpid != cur_grpid) {
      suppressWarnings({
        map_state <- map_state %>%
          addPolygon_Point(counties_sf_cur, labels_final, cur_grpid) %>%
          clearGroup(prev_grpid)
      })
    }
    prev_grpid_state_reactive$val <- cur_grpid
    map_state
  })

  # Render plot of Rt over time on click
  output$RtOverTime_exploreState <- renderCachedPlot({
    validate(need(input$explore_states_out_shape_click,
                  message = "Click on a county to show Rt and new cases over time."))
    click <- input$explore_states_out_shape_click
    plt_dat <- dplyr::filter(rt_long_all, UID == click$id)
    if (nrow(plt_dat) > 0) {
      suppressWarnings(click_plot(plt_dat))
    }
  }, cacheKeyExpr = { input$explore_states_out_shape_click$id })


  # update county_rt_long data frame
  county_rt_long_update <- reactive({
    validate(need(input$state_select, message = "Please select a state."))
    county_uids <- get_county_uids(input$state_select)
    rt_long_all %>%
      dplyr::filter((UID > county_uids$uid_lwr & UID < county_uids$uid_upr) |
                    UID %in% county_uids$extra_uids)
  })

  num_counties <- reactive({
    df_with_rts <- county_rt_long_update() %>%
      filter(Rt_plot > 0)
    length(unique(df_with_rts$UID))
  })

  output$explorestate_row2_ui <- renderUI({
    validate(need(input$state_select, message = "Please select a state."))
    row_height <- 200 + 20 * num_counties()
    fluidRow(
      column(6, align = "center",
        plotOutput("explore_states_counties", height = row_height)
      ),
      column(6, align = "center",
        plotOutput("RtForestPlot_exploreState", height = row_height)
      )
    )

  })


  # render heatmap of counties over time
  output$explore_states_counties <- renderCachedPlot({
    validate(need(input$state_select, message = "Please select a state."))
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

  # render forest plot of Rts on selected date.
  output$RtForestPlot_exploreState <- renderCachedPlot({
    validate(need(input$state_select, message = "Please select a state."))
    validate(need(input$state_select_date, message = "Please select a date."))
    date_select <- format(input$state_select_date, "%Y-%m-%d")
    county_rt_long_update() %>%
      dplyr::filter(date_lag == date_select) %>%
      forest_plot(resolution = "county", date_lag = date_select)
  }, cacheKeyExpr = { c(input$state_select, input$state_select_date) })


  # render table of county Rts at current date
  output$Rt_table_explore_states <- DT::renderDT({
    validate(need(input$state_select, message = "Please select a state."))
    validate(need(input$state_select_date, message = "Please select a date."))
    date_select <- format(input$state_select_date, "%Y-%m-%d")
    county_rt_long_update() %>%
      dplyr::filter(date_lag == date_select) %>%
      munge_for_dt()
  }, server = FALSE, rownames = TRUE, callback = dt_js_callback)

  # title for county Rt tables
  output$Rt_table_explore_states_title <- renderText({
    req(input$state_select_date)
    req(input$state_select)
    date_lag <- format(input$state_select_date, "%Y-%m-%d")
    date_actual <- format(input$state_select_date + 5, "%Y-%m-%d")
    state_uid <- input$state_select
    sprintf("Table of metrics on %s for %s. Rt calculated for %s (5-day lag).",
            date_actual, state_uid_to_place[[state_uid]], date_lag)
  })

  # Heroku disconnects the user from RShiny after 60 seconds of inactivity. Use
  # this to allow the user to be automatically connected
  session$allowReconnect("force")

}

# Run the application
shinyApp(ui = ui, server = server)
