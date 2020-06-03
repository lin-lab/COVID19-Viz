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
library(plotly)
library(withr)
library(data.table)
library(RColorBrewer)

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

# map state UIDs to place name
state_uid_to_place <- as.list(names(place_choices$us_state))
names(state_uid_to_place) <- unlist(place_choices$us_state, use.names = FALSE)

########################################################################
## Define globals
########################################################################

# bins and colors for the map
bins <- c(0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 2, 5, Inf)
rt_range <- range(rt_long_all$Rt_plot, na.rm = TRUE)
pal_default <- partial(colorBin, palette = "RdYlBu", bins = bins,
                       reverse = TRUE)

# set up defaults for adding stuff to leaflet maps
addPolygons_default <-
  partial(addPolygons, opacity = 1, weight = 0.5, color = "white",
          dashArray = "3", fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5, color = "#666", dashArray = "",
            fillOpacity = 0.7, bringToFront = TRUE),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px", direction = "auto"))

addCircles_default <-
  partial(addCircles, opacity = 1, weight = 1, radius = 17000, dashArray = "3",
          fillOpacity = 0.7, stroke = TRUE, color = "white",
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px", direction = "auto"),
          highlightOptions = highlightOptions(
            weight = 5, color = "#666", dashArray = "",
            fillOpacity = 0.7, bringToFront = TRUE))

addLegend_default <- partial(addLegend, position = "bottomright",
                             opacity = 0.75, title = "Rt")

# get range of possible dates
dates <- unique(rt_long_all$date)
min_date <- min(dates)
max_date <- max(dates)

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
  na_rt <- is.na(rt_col)
  rt_str <- ifelse(na_rt, "Insufficient data",
                   sprintf("Rt: %0.2f (%0.2f - %0.2f)",
                           rt_col, sf_dat[["Rt_lwr"]],
                           sf_dat[["Rt_upr"]]))
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
#' @param date_select Selcted date.
#' @param sel_resolution Selected resolution.
#' @param state_uid A state UID string. If non-null, uses the state UID to
#' select the counties in that state. See get_county_uids.
sf_by_date_res <- function(date_select,
                           sel_resolution = c("state_USA_Canada", "state_China",
                                              "state_Australia", "county",
                                              "country"), state_uid = NULL) {
  sel_resolution <- match.arg(sel_resolution)
  rt_col <- paste0("Rt_plot_", date_select)
  rtlwr_col <- paste0("Rt_lwr_", date_select)
  rtupr_col <- paste0("Rt_upr_", date_select)
  select_cols <- c(rt_col, rtlwr_col, rtupr_col, "UID", "dispID")

  if (is.null(state_uid)) {
    ret_sf <- sf_all %>%
      filter(resolution == sel_resolution) %>%
      select(!!select_cols)
  } else {
    stopifnot(sel_resolution == "county")
    county_uids <- get_county_uids(state_uid)
    ret_sf <- sf_all %>%
      filter(resolution == sel_resolution, (UID > county_uids$uid_lwr &
              UID < county_uids$uid_upr) | UID %in% county_uids$extra_uids) %>%
      select(!!select_cols)
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
  } else if (state_str %in% small_states) {
    return(default_zoom + 1)
  } else if (state_str %in% large_states) {
    return(default_zoom - 1)
  } else if (state_str %in% huge_states) {
    return(default_zoom - 2)
  } else {
    return(default_zoom)
  }
}

########################################################################
## Define UI
########################################################################

ui <- fluidPage(
  tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
  titlePanel("Visualizing COVID-19 Rate of Spread (Rt)"),
  tabsetPanel(
    # first panel: big Rt map with multiple resolutions.
    tabPanel("Rt Map",
      sidebarLayout(
        sidebarPanel(
          p("Use slider to adjust date. Click on an area to see its Rt over time."),
          p("Click play button to animate Rt over time."),
          sliderInput("RtDate", label = "Date",
                      min = min_date, max = max(dates),
                      value = max(dates),
                      animate = animationOptions(interval = 3000)),
          selectInput("select_resolution", "Resolution:",
                      choices = list("Country" = "country",
                                     "US States and Canadian Provinces" = "state_USA_Canada",
                                     "Australian Provinces" = "state_Australia",
                                     "Chinese Provinces" = "state_China",
                                     "County (US only)" = "county")),
          # plot of Rt over time
          plotOutput("RtOverTime")
        ), # end of sidebarPanel
        mainPanel(
          leafletOutput("RtMap", height = "90vh", width = "100%"),
          h4("Table of Rts for Current Date and Resolution"),
          DT::dataTableOutput("Rt_table")
        )
      ) # end of sidebarLayout
    ), # end of tabPanel
    # Second tab: Compare Rt across different regions.
    tabPanel("Compare Rt",
      sidebarLayout(
        sidebarPanel(
          h4("Select areas to compare their Rt."),
          p("Double click on an area in the legend to isolate its Rt curve."),
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
          plotlyOutput("compare_plt_out", height = "90vh", width = "100%")
        )
      ) # end of sideBarLayout
    ), # end of tabPanel
    # Third tab: explore states
    tabPanel("Explore States",
      # controls at the top
      fluidRow(
        column(6,
          h4("Select a state to explore"),
          selectizeInput("state_select", label = "State",
                         selected = "84000025",
                         choices = place_choices$us_state,
                         multiple = FALSE)
        ),
        column(6, align = "center",
          sliderInput("state_select_date", label = "Date",
                      min = min_date, max = max(dates),
                      value = max(dates), animate = TRUE)
        )
      ),
      # output at the bottom
      fluidRow(
        column(6, align = "center",
          plotOutput("explore_states_counties", height = "450px")
        ),
        column(6, align = "center",
          leafletOutput("explore_states_out", height = "450px")
        )
      ),
      fluidRow(
        h4("Table of Rts for selected date"),
        DT::dataTableOutput("Rt_table_explore_states")
      )
    ) # end of tabPanel
  ) # end of tabsetPanel
) # end of fluidPage

########################################################################
## Define Server function
########################################################################

server <- function(input, output, session) {
  cdata <- session$clientData

  # The basic big Rt map
  output$RtMap <- renderLeaflet({
    sf_dat_init <- sf_by_date_res(max_date, "country")
    pal <- pal_default(domain = sf_dat_init$Rt)
    suppressWarnings(
      leaflet() %>%
        setView(0, 0, 2) %>%
        addProviderTiles(providers$Stamen.TonerLite) %>%
        addLegend_default(pal = pal, values = sf_dat_init$Rt)
    )
  })

  # change the zoom level only when the resolution changes.
  observe({
    sel_resolution <- input$select_resolution
    cur_view <- switch(sel_resolution,
      "state_USA_Canada" = c(-96, 44, 4),
      "state_China" = c(104.4, 34.7, 4),
      "state_Australia" = c(135.5, -26.1, 4),
      "county" = c(-96, 37.8, 4),
      "country" = c(0, 0, 2)
    )
    suppressWarnings(
      leafletProxy("RtMap") %>%
        setView(cur_view[1], cur_view[2], cur_view[3])
    )
  })

  # update the data based on values
  sf_dat_update <- reactive({
    date_select <- format(input$RtDate, "%Y-%m-%d")
    sel_resolution <- input$select_resolution
    sf_by_date_res(date_select, sel_resolution)
  })

  # keep track of previous group ID so we can clear its shapes
  prev_grpid_all_reactive <- reactiveValues(val = "")

  # change the shapes on the map when the resolution or date changes
  observe({
    date_select <- format(input$RtDate, "%Y-%m-%d")
    sel_resolution <- input$select_resolution
    sf_dat_cur <- sf_dat_update()
    labels_final <- master_labeller(sf_dat_cur, date_select)
    cur_grpid <- digest::digest(c(date_select, sel_resolution))
    prev_grpid <- prev_grpid_all_reactive$val
    map <- leafletProxy("RtMap", data = sf_dat_cur)
    if (prev_grpid != cur_grpid) {
      suppressWarnings(
        map <- map %>%
          addPolygon_Point(sf_dat_cur, labels_final, cur_grpid) %>%
          clearGroup(prev_grpid)
      )
    }
    prev_grpid_all_reactive$val <- cur_grpid
    map
  })

  # Rt over time based on click
  output$RtOverTime <- renderCachedPlot({
    validate(need(input$RtMap_shape_click,
                  message = "Click on a location to show its Rt over time"))
    click <- input$RtMap_shape_click
    sel_resolution <- input$select_resolution
    plt_dat <- dplyr::filter(rt_long_all, UID == click$id)
    if (nrow(plt_dat) > 0) {
      plt_title <- sprintf("Rt for %s", unique(plt_dat$dispID))
      ylim_max <- ceiling(max(plt_dat$Rt_upr))
      p <- plt_dat %>%
        ggplot(aes(x = date, y = Rt_plot)) +
        geom_point(color = "black") +
        geom_errorbar(aes(ymin = Rt_lwr, ymax = Rt_upr), color = "black") +
        geom_smooth(method = "loess", se = TRUE, formula = y ~ x) +
        xlab("Date") + ylab("Rt") + ggtitle(plt_title) +
        ylim(0, ylim_max) + geom_hline(yintercept = 1, lty = 2) +
        theme_cowplot() + scale_x_date(date_minor_breaks = "1 day") +
        background_grid(major = "xy", minor = "xy")
      suppressWarnings(print(p))
    }
  }, cacheKeyExpr = { input$RtMap_shape_click$id })

  # Table of current Rts at current resolution
  output$Rt_table <- DT::renderDataTable({
    date_select <- format(input$RtDate, "%Y-%m-%d")
    sel_resolution <- input$select_resolution
    rt_long_all %>%
      filter(resolution == sel_resolution, date == date_select,
             !is.na(Rt_plot)) %>%
      mutate(Rt = round(Rt_plot, 2)) %>%
      select(Location = dispID, Rt, `Total Cases` = positive,
             `New Cases` = positiveIncrease, `Total Deaths` = death,
             `New Deaths` = deathIncrease) %>%
      arrange(desc(Rt))
  })

  # generate the plot of Rt comparisons after user hits submit
  compare_plt <- eventReactive(input$compare_submit, {
    # get all the UIDs in a data frame and join them with rt long to select
    selected_states <- input$compare_sel_states
    selected_counties <- input$compare_sel_counties
    selected_countries <- input$compare_sel_countries
    selected_uids <- data.frame(
      UID = as.double(c(selected_states, selected_counties, selected_countries))
    )
    if (nrow(selected_uids) == 0) {
      return()
    }

    plt_data_compare <- inner_join(rt_long_all, selected_uids, by = "UID") %>%
      rename(Rt = Rt_plot, Location = dispID)
    ylim_max <- ceiling(max(plt_data_compare$Rt_upr))
    p <- na.omit(plt_data_compare) %>%
      rename(Date = date) %>%
      ggplot(aes(x = Date, y = Rt, color = Location, fill = Location)) +
      geom_ribbon(aes(ymin = Rt_lwr, ymax = Rt_upr), alpha = 0.2) +
      geom_line() + geom_point() + xlab("Date") + ylab("Rt") +
      ggtitle("Comparison of Rt") + ylim(0, ylim_max) +
      geom_hline(yintercept = 1, lty = 2) +
      scale_x_date(date_minor_breaks = "1 day") +
      theme_cowplot() +
      background_grid(major = "xy", minor = "xy")
    p
  })

  # render it as a plotly object
  output$compare_plt_out <- renderPlotly({
    x <- suppressWarnings(compare_plt())
    if (!is.null(x)) {
      suppressWarnings(with_options(options(digits = 3),
                                    ggplotly(x, tooltip = c("x", "y", "fill"))))
    }
  })

  # change map polygons when state or date changes
  prev_grpid_state_reactive <- reactiveValues(val = "default")

  # explore states map
  output$explore_states_out <- renderLeaflet({
    counties_sf <- sf_by_date_res(max_date, "county", "84000025")
    labels_final <- master_labeller(counties_sf, max_date)
    pal <- pal_default(domain = counties_sf$Rt)
    suppressWarnings(
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite) %>%
        setView(-71.72, 42.06, 7) %>%
        addPolygon_Point(counties_sf_cur, labels_final, "default") %>%
        addLegend_default(pal = pal, values = counties_sf$Rt)
    )
  })

  # update county data
  county_sf_update <- reactive({
    date_select <- format(input$state_select_date, "%Y-%m-%d")
    state_input <- input$state_select
    validate(need(input$state_select, "Please choose a state"))
    sf_by_date_res(date_select, "county", state_input)
  })

  # change zoom level only when state changes
  observe({
    state_input <- input$state_select
    zoom_level <- set_state_zoom(state_input)
    counties_sf_cur <- county_sf_update()
    counties_bbox <- st_bbox(counties_sf_cur)
    lng <- mean(c(counties_bbox["xmin"], counties_bbox["xmax"]))
    lat <- mean(c(counties_bbox["ymin"], counties_bbox["ymax"]))
    if (state_input == "84000002") {
      lng <- -147
    }
    leafletProxy("explore_states_out") %>%
      setView(lng, lat, zoom_level)
  })

  observe({
    date_select <- format(input$state_select_date, "%Y-%m-%d")
    state_input <- input$state_select
    validate(need(input$state_select, "Please choose a state"))

    counties_sf_cur <- county_sf_update()
    labels_final <- master_labeller(counties_sf_cur, date_select)
    pal <- pal_default(domain = counties_sf_cur$Rt)
    cur_grpid <- digest::digest(c(date_select, state_input))
    prev_grpid <- prev_grpid_state_reactive$val
    map_state <- leafletProxy("explore_states_out", data = counties_sf_cur)
    if (prev_grpid != cur_grpid) {
      suppressWarnings(
        map_state <- map_state %>%
        addPolygon_Point(counties_sf_cur, labels_final, cur_grpid) %>%
        clearGroup(prev_grpid)
      )
    }
    prev_grpid_state_reactive$val <- cur_grpid
    map_state
  })

  # update county_rt_long data frame
  county_rt_long_update <- reactive({
    validate(need(input$state_select, message = "Please select a state"))
    county_uids <- get_county_uids(input$state_select)
    rt_long_all %>%
      filter((UID > county_uids$uid_lwr & UID < county_uids$uid_upr) |
               UID %in% county_uids$extra_uids)
  })

  # render heatmap of counties over time
  output$explore_states_counties <- renderCachedPlot({
    validate(need(input$state_select, message = "Please select a state"))
    plt_data_pruned <- county_rt_long_update() %>%
      filter(!is.na(Rt_plot)) %>%
      mutate(County = factor(dispID),
             `Rt Bins` = cut(Rt_plot, breaks = bins,
                             include.lowest = TRUE, right = FALSE)) %>%
      rename(Rt = Rt_plot)
    color_pal <- rev(brewer.pal(9, "RdYlBu"))
    names(color_pal) <- levels(plt_data_pruned$`Rt Bins`)

    plt_title <- sprintf("Rt for %s Counties Over Time",
                         state_uid_to_place[[input$state_select]])
    if (nrow(plt_data_pruned) > 0) {
      p <- plt_data_pruned %>%
        ggplot(aes(x = date, y = County, fill = `Rt Bins`, labels = Rt)) +
        geom_tile(alpha = 0.7) +
        scale_fill_manual(drop = FALSE, values = color_pal) +
        xlab("Date") + ylab("County") + ggtitle(plt_title) +
        theme_cowplot() + scale_x_date(date_minor_breaks = "1 day") +
        scale_y_discrete(limits = rev(levels(plt_data_pruned$County))) +
        background_grid(major = "xy", minor = "xy") +
        theme(axis.text.y = element_text(size = 10),
              legend.text = element_text(size = 12))
    } else {
      # draw empty plot
      p <- ggplot()
    }
    p
  }, cacheKeyExpr = { input$state_select })

  # render table of county Rts at current date
  output$Rt_table_explore_states <- DT::renderDataTable({
    validate(need(input$state_select, message = "Please select a state"))
    validate(need(input$state_select_date, message = "Please select a state"))
    date_select <- format(input$state_select_date, "%Y-%m-%d")
    county_rt_long_update() %>%
      filter(date == date_select) %>%
      mutate(Rt = round(Rt_plot, 2)) %>%
      select(Location = dispID, Rt, `Total Cases` = positive,
             `New Cases` = positiveIncrease, `Total Deaths` = death,
             `New Deaths` = deathIncrease) %>%
      arrange(desc(Rt))
  })

  # Heroku disconnects the user from RShiny after 60 seconds of inactivity. Use
  # this to allow the user to be automatically connected
  session$allowReconnect("force")

}

# Run the application
shinyApp(ui = ui, server = server)
