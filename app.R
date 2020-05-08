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
library(sp)
library(ggplot2)
library(cowplot)
library(htmltools)
library(purrr)
library(dplyr)
library(plotly)
library(withr)
library(data.table)
library(RColorBrewer)

state_merged <- readRDS("clean_data/state_merged.rds")
county_merged <- readRDS("clean_data/county_merged.rds")
state_rt_long <- readRDS("clean_data/state_rt_long.rds")
county_rt_long <- readRDS("clean_data/county_rt_long.rds")
base_state <- leaflet(state_merged) %>%
  setView(-96, 37.8, 4) %>%
  addTiles()
base_county <- leaflet(county_merged) %>%
  setView(-96, 37.8, 4) %>%
  addTiles()

state_county_choices <- readRDS("clean_data/state_county_choices.rds")
state_choices <- unique(state_rt_long$State)

addPolygons_default <-
  partial(addPolygons,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))

addLegend_default <-
  partial(addLegend,
          position = "bottomright", opacity = 0.7, title = "Rt")

dates <- names(state_merged)[startsWith(names(state_merged), "Rt_")] %>%
  substring(first = 4) %>%
  as.Date(format = "%Y-%m-%d")
min_date <- as.Date("2020-03-19", format = "%Y-%m-%d")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
  titlePanel("Rt"),
  tabsetPanel(
    # first panel: output for all states
    tabPanel("State-level Rt",
      sidebarLayout(
        sidebarPanel(
          p("Use slider to adjust date. Click on a state to see its Rt over time."),
          sliderInput("stateDate", label = "Date",
                      min = min_date, max = max(dates),
                      value = max(dates), animate = TRUE),
          plotOutput("state_rt")
        ),
        mainPanel(
          leafletOutput("us_states", height = "90vh", width = "100%")
        )
      ) # end of sidebarLayout
    ), # end of tabPanel
    # second tab: county-level Rt.
    tabPanel("County-level Rt",
      sidebarLayout(
        sidebarPanel(
          p("Use slider to adjust date. Click on a county to see its Rt over time."),
          sliderInput("countyDate", label = "Date",
                      min = min_date, max = max(dates),
                      value = max(dates), animate = FALSE),
          plotOutput("county_rt")
        ),
        mainPanel(
          leafletOutput("us_counties", height = "90vh", width = "100%")
        )
      ) # end of sideBarLayout
    ), # end of second tabPanel
    tabPanel("Compare Rt",
      sidebarLayout(
        sidebarPanel(
          h4("Select states/counties to compare their Rt"),
          selectizeInput("compare_select", label = "States/Counties",
                         choices = state_county_choices,
                         multiple = TRUE),
          actionButton("compare_submit", label = "Submit")
        ),
        mainPanel(
          plotlyOutput("compare_plt_out", height = "90vh", width = "100%")
        )
      ) # end of sideBarLayout
    ), # end of third tabPanel
    tabPanel("Explore States",
      fluidRow(
        column(7,
          h4("Select a state to explore"),
          selectizeInput("state_select", label = "State",
                         selected = "Massachusetts",
                         choices = state_choices, multiple = FALSE)
        ),
        column(5, align = "center",
          sliderInput("state_select_date", label = "Date",
                      min = min_date, max = max(dates),
                      value = max(dates), animate = TRUE)
        )
      ),
      fluidRow(style = "height:1000px",
        column(7, align = "center",
          plotlyOutput("explore_states_counties", height = "600px")
        ),
        column(5, align = "center",
          leafletOutput("explore_states_out", height = "600px")
        )
      )
    )
  ) # end of tabsetPanel
) # end of fluidPage

server <- function(input, output, session) {
  cdata <- session$clientData
  output$us_states <- renderLeaflet({
    date_select <- format(input$stateDate)
    rt_col <- paste0("Rt_", date_select)
    bins <- c(0, 0.5, 0.8, 1.0, 1.2, 1.5, 2, 5, Inf)
    # color palette
    pal <- colorBin("YlOrRd", domain = state_merged[[date_select]],
                    bins = bins)
    # format for labels
    labels_states <- sprintf("<strong>%s</strong><br/>Date: %s<br/>Rt: %0.2f",
                             state_merged$NAME,
                             date_select,
                             state_merged[[rt_col]]) %>%
      lapply(htmltools::HTML)
    base_state %>%
      addPolygons_default(
        fillColor = pal(state_merged[[rt_col]]),
        weight = 2, label = labels_states,
        layer = state_merged$NAME) %>%
      addLegend_default(pal = pal, values = state_merged[[rt_col]])
  })

  state_df <- reactive({
    click <- input$us_states_shape_click
    if (!is.null(click)) {
      click_state <- click$id
      state_rt_long %>%
        dplyr::filter(State == click_state)
    } else {
      NULL
    }
  })

  output$state_rt <- renderPlot({
    plt_data_state <- state_df()
    if (!is.null(plt_data_state) && nrow(plt_data_state) > 0) {
      state_name <- unique(plt_data_state$State)[1]
      p <- plt_data_state %>%
        ggplot(aes(x = Date, y = Rt)) + geom_line() +
        geom_point() +
        xlab("Date") + ylab("Rt") +
        ggtitle(sprintf("Rt for %s", state_name)) +
        ylim(0, ceiling(max(plt_data_state$Rt))) +
        geom_hline(yintercept = 1, lty = 2) +
        theme_cowplot() +
        scale_x_date(date_minor_breaks = "1 day") +
        background_grid(major = "xy", minor = "xy")
      suppressWarnings(print(p))
    }
  })


  output$us_counties <- renderLeaflet({
    date_select <- format(input$countyDate)
    rt_col <- paste0("Rt_", date_select)
    bins <- c(0, 0.5, 0.8, 1.0, 1.2, 1.5, 2, 5, Inf)
    # color palette
    pal <- colorBin("YlOrRd", domain = county_merged[[rt_col]],
                    bins = bins)
    # format for labels
    rts <- county_merged[[rt_col]]
    rt_to_str <- ifelse(is.na(rts), "Insufficient data", sprintf("%0.2f", rts))
    labels_counties <- sprintf("<strong>%s, %s</strong><br/>Rt: %s",
                               county_merged$County,
                               county_merged$State,
                               rt_to_str) %>%
      lapply(htmltools::HTML)
    base_county %>%
      addPolygons_default(
        fillColor = pal(county_merged[[rt_col]]),
        weight = 0.5, label = labels_counties,
        layer = county_merged$GEOID) %>%
      addLegend_default(pal = pal, values = county_merged[[rt_col]])
  })

  county_df <- reactive({
    click <- input$us_counties_shape_click
    if (!is.null(click)) {
      click_county_geo <- click$id
      county_rt_long %>%
        dplyr::filter(FIPS_str == click_county_geo)
    } else {
      NULL
    }
  })

  output$county_rt <- renderPlot({
    plt_data_county <- county_df()
    if (!is.null(plt_data_county) && nrow(plt_data_county) > 0) {
      county_name <- unique(plt_data_county$County)[1]
      state_name <- unique(plt_data_county$State)[1]
      plt_title <- sprintf("Rt for %s, %s", county_name,
                           state_name)
      p <- plt_data_county %>%
        ggplot(aes(x = Date, y = Rt)) +
        geom_line() + geom_point() +
        xlab("Date") + ylab("Rt") +
        ggtitle(plt_title) +
        ylim(0, ceiling(max(plt_data_county$Rt, na.rm = TRUE))) +
        geom_hline(yintercept = 1, lty = 2) +
        theme_cowplot() +
        scale_x_date(date_minor_breaks = "1 day") +
        background_grid(major = "xy", minor = "xy")
      suppressWarnings(print(p))
    }
  })

  compare_plt <- reactive({
    if (input$compare_submit == 0) {
      return()
    }
    selected <- isolate(input$compare_select)
    if (is.null(selected)) {
      return()
    }
    states_selected <- tibble(State = gsub(" State", "", selected))
    df_county <- tibble()
    df_state <- state_rt_long %>%
      right_join(states_selected, by = "State") %>%
      select(Location = State, Date, Rt)
    counties_states <- Filter(function(x) { length(x) == 2 },
                              strsplit(selected, split = ", ",
                                       fixed = TRUE))
    if (length(counties_states) > 0) {
      counties_selected <- tibble(
        County = vapply(counties_states, function(x) { x[1] }, "a"),
        State = vapply(counties_states, function(x) { x[2] }, "a")
      )
      df_county <- county_rt_long %>%
        right_join(counties_selected, by = c("County", "State")) %>%
        mutate(Location_str = paste(County, State, sep = ", ")) %>%
        mutate(Location = factor(Location_str,
                                 levels = rev(unique(Location_str)))) %>%
        select(Location, Date, Rt)
    }
    plt_data_compare <- rbind(df_state, df_county)
    p <- plt_data_compare %>%
      ggplot(aes(x = Date, y = Rt, color = Location)) +
      geom_line() + geom_point() +
      xlab("Date") + ylab("Rt") +
      ggtitle("Comparison of Rt") +
      ylim(0, ceiling(max(plt_data_compare$Rt))) +
      geom_hline(yintercept = 1, lty = 2) +
      scale_color_discrete(name = "State/County") +
      theme_cowplot() +
      scale_x_date(date_minor_breaks = "1 day") +
      background_grid(major = "xy", minor = "xy")
    p
  })

  output$compare_plt_out <- renderPlotly({
    x <- suppressWarnings(compare_plt())
    if (!is.null(x)) {
      suppressWarnings(with_options(options(digits = 3), ggplotly(x)))
    }
  })

  output$explore_states_out <- renderLeaflet({
    if (input$state_select == "") {
      return()
    }
    counties_plt <- subset(county_merged,
                           county_merged$State == input$state_select)
    date_select <- format(input$state_select_date)
    bins <- c(0, 0.5, 0.8, 1.0, 1.2, 1.5, 2, 5, Inf)
    # color palette
    pal <- colorBin("YlOrRd", domain = counties_plt[[date_select]],
                    bins = bins)
    # format for labels
    rt_col <- paste0("Rt_", date_select)
    rts <- counties_plt[[rt_col]]
    rt_to_str <- ifelse(is.na(rts), "Insufficient data", sprintf("%0.2f", rts))
    labels_counties <- sprintf("<strong>%s, %s</strong><br/>Date: %s<br/>Rt: %s",
                               counties_plt$County,
                               counties_plt$State,
                               date_select,
                               rt_to_str) %>%
      lapply(htmltools::HTML)
    boundary <- bbox(counties_plt)
    leaflet(counties_plt) %>%
      addTiles() %>%
      fitBounds(lng1 = boundary[1, 1], lng2 = boundary[1, 2],
                   lat1 = boundary[2, 1], lat2 = boundary[2, 2]) %>%
      addPolygons_default(
        fillColor = pal(rts),
        weight = 0.5, label = labels_counties,
        layer = counties_plt$GEOID) %>%
      addLegend_default(pal = pal, values = rts)
  })

  output$explore_states_counties <- renderPlotly({
    if (input$state_select == "") {
      return()
    }
    plt_data_pruned <- county_rt_long %>%
      filter(State == input$state_select, !is.na(Rt)) %>%
      mutate(County = factor(County)) %>%
      rename(`Total Cases` = totalPos)

    plt_title <- sprintf("Rt for %s Counties", input$state_select)
    max_rt_round <- ceiling(max(plt_data_pruned$Rt, na.rm = TRUE))
    p <- plt_data_pruned %>%
      ggplot(aes(x = Date, y = County, color = Rt, size = `Total Cases`)) +
      geom_point() +
      scale_color_gradientn(limits = c(0, max_rt_round),
                            breaks = seq(0, max_rt_round, 1),
                            colors = brewer.pal(9, "YlOrRd")) +
      xlab("Date") + ylab("County") +
      ggtitle(plt_title) +
      theme_cowplot() +
      scale_x_date(date_minor_breaks = "1 day") +
      scale_y_discrete(limits = rev(levels(plt_data_pruned$County))) +
      background_grid(major = "xy", minor = "xy") +
      theme(axis.text.y = element_text(size = 10),
            legend.text = element_text(size = 8))
    suppressWarnings(
      with_options(options(digits = 3),
                 ggplotly(p, width = cdata$output_pid_width,
                          height = cdata$output_pid_height))
    )
  })

  session$allowReconnect("force")

}

# Run the application
shinyApp(ui = ui, server = server)
