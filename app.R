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

sf_all <- readRDS("clean_data/sf_all.rds")
rt_long_all <- readRDS("clean_data/rt_long_all.rds")
place_choices <- readRDS("clean_data/names_list.rds")

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
  partial(addCircles, opacity = 1, weight = 1, radius = 17000,
          fillOpacity = 0.7, stroke = TRUE, color = "white",
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px", direction = "auto"))

addLegend_default <- partial(addLegend, position = "bottomright",
                             opacity = 0.75, title = "Rt")

dates <- unique(rt_long_all$date)
min_date <- min(dates)
max_date <- max(dates)

########################################################################
## Define Functions
########################################################################

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

master_labeller <- function(sf_dat, date_select) {
  stopifnot(names(sf_dat) == c("Rt", "Rt_lwr", "Rt_upr", "UID",
                               "dispID", "geometry"))
  rt_labels <- rt_labeller(sf_dat)
  labels_final <- sprintf("<strong>%s</strong><br/>Date: %s<br/>%s",
                          sf_dat$dispID, date_select, rt_labels) %>%
      lapply(htmltools::HTML)
  return(labels_final)
}

sf_by_id <- function(uid) {
  ret_sf <- dplyr::filter(rt_long_all, UID == uid)
  return(ret_sf)
}

get_county_uids <- function(state_uid_str) {
  # MA, UT, MO have extra counties
  extra_states <- c("84000025", "84000049", "84000029")
  state_id <- as.numeric(state_uid_str) %% 100
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

sf_by_date_res <- function(date_select,
                           sel_resolution = c("state", "county", "country"),
                           state_uid = NULL) {
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

addPolygon_Point <- function(.map, .data, labels, add_legend = FALSE) {
  pal <- pal_default(domain = .data$Rt)
  stopifnot(names(.data) == c("Rt", "Rt_lwr", "Rt_upr", "UID", "dispID",
                             "geometry"))
  polygon_idx <- st_is(.data, "POLYGON") | st_is(.data, "MULTIPOLYGON")
  data_polygons <- .data[polygon_idx, ]
  data_points <- .data[!polygon_idx, ]
  map_ret <- addPolygons_default(.map, data = data_polygons,
                                 fillColor = ~pal(Rt),
                                 label = labels[polygon_idx],
                                 layer = ~UID)

  if (sum(polygon_idx) < nrow(.data)) {
    map_ret <- addCircles_default(map_ret, data = data_points,
                                  fillColor = ~pal(Rt), layer = ~UID,
                                  label = labels[!polygon_idx])
  }
  if (add_legend) {
    map_ret <- addLegend_default(map_ret, pal = pal, values = .data$Rt)
  }
  return(map_ret)
}

ui <- fluidPage(
  #tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
  titlePanel("Visualizing COVID-19 Rate of Spread (Rt)"),
  tabsetPanel(
    # first panel: output for all states
    tabPanel("Rt Map",
      sidebarLayout(
        sidebarPanel(
          p("Use slider to adjust date. Click on an area to see its Rt over time."),
          p("Click play button to animate Rt over time."),
          sliderInput("RtDate", label = "Date",
                      min = min_date, max = max(dates),
                      value = max(dates), animate = TRUE),
          selectInput("select_resolution", "Resolution:",
                      choices = list("States/Provinces (US, CAN, CHN, AUS)" = "state",
                                     "County (US only)" = "county",
                                     "Country" = "country")),
          plotOutput("RtOverTime")
        ), # end of sidebarPanel
        mainPanel(
          leafletOutput("RtMap", height = "90vh", width = "100%")
        )
      ) # end of sidebarLayout
    ), # end of tabPanel
    tabPanel("Compare Rt",
      sidebarLayout(
        sidebarPanel(
          h4("Select areas to compare their Rt."),
          p("Double click on an area in the legend to isolate its Rt curve."),
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
          #plotOutput("compare_plt_out")
        )
      ) # end of sideBarLayout
    ), # end of tabPanel
    tabPanel("Explore States",
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
      fluidRow(
        column(6, align = "center",
          plotOutput("explore_states_counties", height = "500px")
        ),
        column(6, align = "center",
          leafletOutput("explore_states_out", height = "500px")
        )
      )
    )
  ) # end of tabsetPanel
) # end of fluidPage

########################################################################
## Define Server function
########################################################################

server <- function(input, output, session) {
  cdata <- session$clientData

  output$RtMap <- renderLeaflet({
    sf_dat_init <- sf_by_date_res(max_date, "state")
    labels_final <- master_labeller(sf_dat_init, max_date)
    pal <- pal_default(domain = sf_dat_init$Rt)
    suppressWarnings(
      leaflet() %>%
        setView(-96, 37.8, 4) %>%
        addProviderTiles(providers$Stamen.TonerLite) %>%
        addLegend_default(pal = pal, values = sf_dat_init$Rt)
    )
  })

  observe({
    date_select <- format(input$RtDate, "%Y-%m-%d")
    sel_resolution <- input$select_resolution
    sf_dat_cur <- sf_by_date_res(date_select, sel_resolution)
    labels_final <- master_labeller(sf_dat_cur, date_select)
    suppressWarnings(
      leafletProxy("RtMap", data = sf_dat_cur) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygon_Point(sf_dat_cur, labels_final, add_legend = FALSE)
    )
  })

  output$RtOverTime <- renderPlot({
    click <- input$RtMap_shape_click
    sel_resolution <- input$select_resolution
    if (!is.null(click)) {
      plt_dat <- sf_by_id(click$id)
      if (nrow(plt_dat) > 0) {
        plt_title <- sprintf("Rt for %s", unique(plt_dat$dispID))
        ylim_max <- ceiling(max(plt_dat$Rt_upr))
        p <- plt_dat %>%
          ggplot(aes(x = date, y = Rt_plot)) +
          geom_ribbon(aes(ymin = Rt_lwr, ymax = Rt_upr),
                      fill = "darkgray") +
          geom_line(color = "black") + geom_point() +
          xlab("Date") + ylab("Rt") + ggtitle(plt_title) +
          ylim(0, ylim_max) + geom_hline(yintercept = 1, lty = 2) +
          theme_cowplot() + scale_x_date(date_minor_breaks = "1 day") +
          background_grid(major = "xy", minor = "xy")
        suppressWarnings(print(p))
      }
    }
  })

  compare_plt <- eventReactive(input$compare_submit, {
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
    print(plt_data_compare)
    ylim_max <- ceiling(max(plt_data_compare$Rt_upr))
    p <- na.omit(plt_data_compare) %>%
      ggplot(aes(x = date, y = Rt, color = Location, fill = Location)) +
      geom_ribbon(aes(ymin = Rt_lwr, ymax = Rt_upr), alpha = 0.2) +
      geom_line() + geom_point() + xlab("Date") + ylab("Rt") +
      ggtitle("Comparison of Rt") + ylim(0, ylim_max) +
      geom_hline(yintercept = 1, lty = 2) +
      scale_x_date(date_minor_breaks = "1 day") +
      scale_color_discrete(name = "State/County") + theme_cowplot() +
      background_grid(major = "xy", minor = "xy")
    p
  })

  output$compare_plt_out <- renderPlotly({
    x <- suppressWarnings(compare_plt())
    if (!is.null(x)) {
      suppressWarnings(with_options(options(digits = 3),
                                    ggplotly(x, tooltip = c("x", "y", "fill"))))
    }
  })

  output$explore_states_out <- renderLeaflet({
    counties_sf <- sf_by_date_res(max_date, "county", "84000025")
    labels_final <- master_labeller(counties_sf, max_date)
    pal <- pal_default(domain = counties_sf$Rt)
    suppressWarnings(
      leaflet(data = counties_sf) %>%
        addProviderTiles(providers$Stamen.TonerLite) %>%
        addPolygons_default(fillColor = ~pal(Rt), label = labels_final,
                           layer = ~UID) %>%
        addLegend_default(pal = pal, values = ~Rt)
    )
  })

  observe({
    date_select <- format(input$state_select_date, "%Y-%m-%d")
    state_input <- input$state_select
    validate(need(input$state_select, "Please choose a state"))

    counties_sf_cur <- sf_by_date_res(date_select, "county", state_input)
    labels_final <- master_labeller(counties_sf_cur, date_select)
    pal <- pal_default(domain = counties_sf_cur$Rt)
    counties_bbox <- st_bbox(counties_sf_cur)
    lng <- mean(c(counties_bbox["xmin"], counties_bbox["xmax"]))
    lat <- mean(c(counties_bbox["ymin"], counties_bbox["ymax"]))
    if (state_input == "84000002") {
      lng <- -147
    }
    suppressWarnings(
      leafletProxy("explore_states_out", session, data = counties_sf_cur) %>%
        clearShapes() %>%
        clearMarkers() %>%
        flyTo(lng, lat, 6) %>%
        #flyToBounds(counties_bbox["xmin"], counties_bbox["ymin"],
        #            counties_bbox["xmax"], counties_bbox["ymax"]) %>%
        addPolygon_Point(counties_sf_cur, labels_final, add_legend = FALSE)
    )
  })

  output$explore_states_counties <- renderPlot({
    if (input$state_select == "") {
      return()
    }
    county_uids <- get_county_uids(input$state_select)
    plt_data_pruned <- rt_long_all %>%
      filter((UID > county_uids$uid_lwr & UID < county_uids$uid_upr) |
               UID %in% county_uids$extra_uids,
             !is.na(Rt_plot)) %>%
      mutate(County = factor(dispID),
             `Rt Bins` = cut(Rt_plot, breaks = bins,
                             include.lowest = TRUE, right = FALSE)) %>%
      rename(Rt = Rt_plot)
    color_pal <- rev(brewer.pal(9, "RdYlBu"))
    names(color_pal) <- levels(plt_data_pruned$`Rt Bins`)

    plt_title <- sprintf("Rt for %s Counties",
                         state_uid_to_place[[input$state_select]])
    p <- plt_data_pruned %>%
      #ggplot(aes(x = date, y = county, color = Rt_plot, size = `Total Cases`)) +
      ggplot(aes(x = date, y = County, fill = `Rt Bins`, labels = Rt)) +
      geom_tile(alpha = 0.7) +
      scale_fill_manual(drop = FALSE, values = color_pal) +
      xlab("Date") + ylab("County") + ggtitle(plt_title) +
      theme_cowplot() + scale_x_date(date_minor_breaks = "1 day") +
      scale_y_discrete(limits = rev(levels(plt_data_pruned$County))) +
      background_grid(major = "xy", minor = "xy") +
      theme(axis.text.y = element_text(size = 10),
            legend.text = element_text(size = 12))
    p
  })

  session$allowReconnect("force")

}

# Run the application
shinyApp(ui = ui, server = server)
