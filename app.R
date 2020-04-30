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

dates <- names(state_merged)[startsWith(names(state_merged), "20")] %>%
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
        column(6,
          h4("Select a state to explore"),
          selectizeInput("state_select", label = "State",
                         selected = "Massachusetts",
                         choices = state_choices, multiple = FALSE)
        ),
        column(6, align = "center",
          sliderInput("state_select_date", label = "Date",
                      min = min_date, max = max(dates),
                      value = max(dates), animate = TRUE)
        )
      ),
      fluidRow(
        column(6, align = "center",
          plotlyOutput("explore_states_counties", height = "90%")
        ),
        column(6, align = "center",
          leafletOutput("explore_states_out")
        )
      )
    )
  ) # end of tabsetPanel
) # end of fluidPage

server <- function(input, output) {
  output$us_states <- renderLeaflet({
    date_select <- format(input$stateDate)
    bins <- c(0, 0.5, 0.8, 1.0, 1.2, 1.5, 2, 5, Inf)
    # color palette
    pal <- colorBin("YlOrRd", domain = state_merged[[date_select]],
                    bins = bins)
    # format for labels
    labels_states <- sprintf("<strong>%s</strong><br/>Rt: %0.2f",
                             state_merged$NAME,
                             state_merged[[date_select]]) %>%
      lapply(htmltools::HTML)
    base_state %>%
      addPolygons_default(
        fillColor = pal(state_merged[[date_select]]),
        weight = 2, label = labels_states,
        layer = state_merged$NAME) %>%
      addLegend_default(pal = pal, values = state_merged[[date_select]])
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
      plt_data_state %>%
        ggplot(aes(x = Date, y = Rt)) + geom_line() +
        geom_point() +
        xlab("Date") + ylab("Rt") +
        ggtitle(sprintf("Rt for %s", state_name)) +
        ylim(0, ceiling(max(plt_data_state$Rt))) +
        geom_hline(yintercept = 1, lty = 2) +
        theme_cowplot() +
        scale_x_date(date_minor_breaks = "1 day") +
        background_grid(major = "xy", minor = "xy")
    }
  })


  output$us_counties <- renderLeaflet({
    date_select <- format(input$countyDate)
    bins <- c(0, 0.5, 0.8, 1.0, 1.2, 1.5, 2, 5, Inf)
    # color palette
    pal <- colorBin("YlOrRd", domain = county_merged[[date_select]],
                    bins = bins)
    # format for labels
    labels_counties <- sprintf("<strong>%s, %s</strong><br/>Rt: %0.2f",
                               county_merged$county,
                               county_merged$state,
                               county_merged[[date_select]]) %>%
      lapply(htmltools::HTML)
    base_county %>%
      addPolygons_default(
        fillColor = pal(county_merged[[date_select]]),
        weight = 0.5, label = labels_counties,
        layer = county_merged$GEOID) %>%
      addLegend_default(pal = pal, values = county_merged[[date_select]])
  })

  county_df <- reactive({
    click <- input$us_counties_shape_click
    if (!is.null(click)) {
      click_county_geo <- click$id
      county_rt_long %>%
        dplyr::filter(GEOID == click_county_geo)
    } else {
      NULL
    }
  })

  output$county_rt <- renderPlot({
    plt_data_county <- county_df()
    if (!is.null(plt_data_county) && nrow(plt_data_county) > 0) {
      county_name <- unique(plt_data_county$county)[1]
      state_abb <- unique(plt_data_county$state)[1]
      plt_title <- sprintf("Rt for %s, %s", county_name,
                           state_abb)
      plt_data_county %>%
        ggplot(aes(x = Date, y = Rt)) +
        geom_line() + geom_point() +
        xlab("Date") + ylab("Rt") +
        ggtitle(plt_title) +
        ylim(0, ceiling(max(plt_data_county$Rt))) +
        geom_hline(yintercept = 1, lty = 2) +
        theme_cowplot() +
        scale_x_date(date_minor_breaks = "1 day") +
        background_grid(major = "xy", minor = "xy")
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
        county = vapply(counties_states, function(x) { x[1] }, "a"),
        state = vapply(counties_states, function(x) { x[2] }, "a")
      )
      df_county <- county_rt_long %>%
        right_join(counties_selected, by = c("county", "state")) %>%
        mutate(Location_str = paste(county, state, sep = ", ")) %>%
        mutate(Location = factor(Location_str,
                                 levels = rev(unique(Location_str)))) %>%
        select(Location, Date, Rt)
    }
    plt_data_compare <- rbind(df_state, df_county)
    plt_data_compare %>%
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
  })

  output$compare_plt_out <- renderPlotly({
    x <- compare_plt()
    if (!is.null(x)) {
      with_options(options(digits = 3), ggplotly(x))
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
    labels_counties <- sprintf("<strong>%s, %s</strong><br/>Rt: %0.2f",
                               counties_plt$county,
                               counties_plt$state,
                               counties_plt[[date_select]]) %>%
      lapply(htmltools::HTML)
    boundary <- bbox(counties_plt)
    leaflet(counties_plt) %>%
      addTiles() %>%
      setMaxBounds(lng1 = boundary[1, 1], lng2 = boundary[1, 2],
                   lat1 = boundary[2, 1], lat2 = boundary[2, 2]) %>%
      addPolygons_default(
        fillColor = pal(counties_plt[[date_select]]),
        weight = 0.5, label = labels_counties,
        layer = counties_plt$GEOID) %>%
      addLegend_default(pal = pal,
                        values = counties_plt[[date_select]])
  })

  output$explore_states_counties <- renderPlotly({
    if (input$state_select == "") {
      return()
    }
    plt_data_cur <- county_rt_long %>%
      filter(State == input$state_select) %>%
      select(Date, Rt, county)
    plt_title <- sprintf("Rt for %s Counties", input$state_select)
    p <- plt_data_cur %>%
      ggplot(aes(x = Date, y = Rt, color = county)) +
      geom_line() + geom_point() +
      xlab("Date") + ylab("Rt") +
      ggtitle(plt_title) +
      ylim(0, ceiling(max(plt_data_cur$Rt))) +
      geom_hline(yintercept = 1, lty = 2) +
      scale_color_discrete(name = "County") +
      theme_cowplot() +
      scale_x_date(date_minor_breaks = "1 day") +
      background_grid(major = "xy", minor = "xy") +
      theme(legend.position = "none")
    with_options(options(digits = 3),  ggplotly(p))

  })

}

# Run the application
shinyApp(ui = ui, server = server)
