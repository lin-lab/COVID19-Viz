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

state_merged <- readRDS("clean_data/state_merged.rds")
county_merged <- readRDS("clean_data/county_merged.rds")
state_rt_long <- readRDS("clean_data/state_rt_long.rds")
county_rt_long <- readRDS("clean_data/county_rt_long.rds")
state_county_choices <- readRDS("clean_data/state_county_choices.rds")
state_choices <- readRDS("clean_data/state_choices.rds")

########################################################################
## Define globals
########################################################################

base_state <- leaflet(data = state_merged) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.TonerLite)
base_county <- leaflet(county_merged) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.TonerLite)


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
          position = "bottomright", opacity = 0.75, title = "Rt")

dates <- names(state_merged)[startsWith(names(state_merged), "death_")] %>%
  substring(first = 7) %>%
  as.Date(format = "%Y-%m-%d")
min_date <- as.Date("2020-03-19", format = "%Y-%m-%d")

rt_labeller <- function(rt_mean, rt_lwr, rt_upr) {
  na_rt <- is.na(rt_mean)
  rt_str <- ifelse(na_rt, "Insufficient data",
                   sprintf("Rt: %0.2f (%0.2f - %0.2f)",
                           rt_mean, rt_lwr, rt_upr))
  return(rt_str)
}

# bins and colors for the map
bins <- c(0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 2, 5, Inf)
rt_range <- range(county_rt_long$Rt_plot, na.rm = TRUE)
pal_default <- partial(colorBin, domain = rt_range, palette = "RdYlBu",
                       bins = bins, reverse = TRUE)

########################################################################
## Define UI
########################################################################

ui <- fluidPage(
  #tags$style(type="text/css", "div.info.legend.leaflet-control br {clear: both;}"),
  titlePanel("Rt"),
  tabsetPanel(
    # first panel: output for all states
    tabPanel("State-level Rt",
      sidebarLayout(
        sidebarPanel(
          p("Use slider to adjust date. Click on an area to see its Rt over time."),
          p("Click play button to animate Rt over time.")
          sliderInput("RtDate", label = "Date",
                      min = min_date, max = max(dates),
                      value = max(dates), animate = TRUE),
          selectInput("select_resolution", "Resolution:",
                      choices = c("state" = "State/Province", "county" = "County (US only)",
                                  "country" = "Country"))
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
          h4("Select states/counties to compare their Rt."),
          p("Double click on a state in the legend to isolate its Rt curve."),
          p("Single click on a state in the legend to hide its Rt curve."),
          selectizeInput("compare_select", label = "States/Counties",
                         choices = state_county_choices,
                         multiple = TRUE),
          actionButton("compare_submit", label = "Submit")
        ),
        mainPanel(
          plotlyOutput("compare_plt_out", height = "90vh", width = "100%")
        )
      ) # end of sideBarLayout
    ), # end of tabPanel
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

  # reactive dataframe to get data for map.
  map_df <- reactive({
    date_select <- format(input$RtDate, "%Y-%m-%d")
    rt_col <- paste0("Rt_plot_", date_select)
    rtlwr_col <- paste0("Rt_lwr_", date_select)
    rtupr_col <- paste0("Rt_upr_", date_select)

    selected_res <- input$select_resolution
    if (selected_res == "state") {
      # do something
      
    } else if (selected_res == "county") {
      # do something
    } else {
      stopifnot(selected_res == "country")
      # not implemented yet
    }
  })

  rt_df <- reactive({
  })

  # state-level plot
  callModule(mapviz, state_merged, "stateName", "stateName")

  state_df <- reactive({
    click <- input$us_states_shape_click
    if (!is.null(click)) {
      click_state <- click$id
      state_rt_long %>%
        dplyr::filter(stateName == click_state)
    } else {
      NULL
    }
  })

  output$state_rt <- renderPlot({
    plt_data_state <- state_df()
    if (!is.null(plt_data_state) && nrow(plt_data_state) > 0) {
      state_name <- unique(plt_data_state$stateName)[1]
      ylim_max <- ceiling(max(plt_data_state$Rt_upr))
      p <- plt_data_state %>%
        ggplot(aes(x = date, y = Rt_plot)) +
        geom_ribbon(aes(ymin = Rt_lwr, ymax = Rt_upr),
                    fill = "darkgray") +
        geom_line(color = "black") +
        geom_point() +
        xlab("Date") + ylab("Rt") +
        ggtitle(sprintf("Rt for %s", state_name)) +
        ylim(0, ylim_max) +
        geom_hline(yintercept = 1, lty = 2) +
        theme_cowplot() +
        scale_x_date(date_minor_breaks = "1 day") +
        background_grid(major = "xy", minor = "xy")
      suppressWarnings(print(p))
    }
  })


  output$us_counties <- renderLeaflet({
    date_select <- format(input$countyDate)
    rt_col <- paste0("Rt_plot_", date_select)
    rtlwr_col <- paste0("Rt_lwr_", date_select)
    rtupr_col <- paste0("Rt_upr_", date_select)
    pal <- pal_default(domain = state_merged[[date_select]])
    # format for labels
    rt_labels <- rt_labeller(county_merged[[rt_col]],
                             county_merged[[rtlwr_col]],
                             county_merged[[rtupr_col]])
    labels_counties <- sprintf("<strong>%s, %s</strong><br/>%s",
                               county_merged$county,
                               county_merged$stateName,
                               rt_labels) %>%
      lapply(htmltools::HTML)
    base_county %>%
      addPolygons_default(
        fillColor = pal(county_merged[[rt_col]]),
        weight = 0.5, label = labels_counties,
        layer = county_merged$UID) %>%
      addLegend_default(pal = pal, values = county_merged[[rt_col]])
  })

  county_df <- reactive({
    click <- input$us_counties_shape_click
    if (!is.null(click)) {
      click_county_UID <- click$id
      county_rt_long %>%
        dplyr::filter(UID == click_county_UID)
    } else {
      NULL
    }
  })

  output$county_rt <- renderPlot({
    plt_data_county <- county_df()
    if (!is.null(plt_data_county) && nrow(plt_data_county) > 0) {
      county_name <- unique(plt_data_county$county)[1]
      state_name <- unique(plt_data_county$stateName)[1]
      plt_title <- sprintf("Rt for %s, %s", county_name,
                           state_name)
      ylim_max <- ceiling(max(plt_data_county$Rt_upr))
      p <- plt_data_county %>%
        ggplot(aes(x = date, y = Rt_plot)) +
        geom_ribbon(aes(ymin = Rt_lwr, ymax = Rt_upr),
                    fill = "darkgray") +
        geom_line(color = "black") +
        geom_point() +
        xlab("Date") + ylab("Rt") +
        ggtitle(plt_title) +
        ylim(0, ylim_max) +
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
    states_selected <- tibble(stateName = gsub(" State", "", selected))
    df_county <- tibble()
    df_state <- state_rt_long %>%
      right_join(states_selected, by = "stateName") %>%
      select(Location = stateName, date, Rt_plot, Rt_lwr, Rt_upr)
    counties_states <- Filter(function(x) { length(x) == 2 },
                              strsplit(selected, split = ", ",
                                       fixed = TRUE))
    if (length(counties_states) > 0) {
      counties_selected <- tibble(
        county = vapply(counties_states, function(x) { x[1] }, "a"),
        stateName = vapply(counties_states, function(x) { x[2] }, "a")
      )
      df_county <- county_rt_long %>%
        right_join(counties_selected, by = c("county", "stateName")) %>%
        mutate(Location_str = paste(county, stateName, sep = ", ")) %>%
        mutate(Location = factor(Location_str,
                                 levels = rev(unique(Location_str)))) %>%
        select(Location, date, Rt_plot, Rt_lwr, Rt_upr)
    }
    plt_data_compare <- rbind(df_state, df_county)
    ylim_max <- ceiling(max(plt_data_compare$Rt_upr))
    p <- na.omit(plt_data_compare) %>%
      ggplot(aes(x = date, y = Rt_plot, color = Location, fill = Location)) +
      geom_ribbon(aes(ymin = Rt_lwr, ymax = Rt_upr), alpha = 0.2) +
      geom_line() + geom_point() +
      xlab("Date") + ylab("Rt") +
      ggtitle("Comparison of Rt") +
      ylim(0, ylim_max) +
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
      suppressWarnings(with_options(options(digits = 3),
                                    ggplotly(x, tooltip = c("x", "y", "fill"))))
    }
  })

  output$explore_states_out <- renderLeaflet({
    if (input$state_select == "") {
      return()
    }
    counties_plt <- subset(county_merged,
                           county_merged$stateName == input$state_select)
    date_select <- format(input$state_select_date)
    rt_col <- paste0("Rt_plot_", date_select)
    rtlwr_col <- paste0("Rt_lwr_", date_select)
    rtupr_col <- paste0("Rt_upr_", date_select)
    pal <- pal_default(domain = counties_plt[[rt_col]])
    # format for labels
    rt_labels <- rt_labeller(counties_plt[[rt_col]],
                             counties_plt[[rtlwr_col]],
                             counties_plt[[rtupr_col]])
    labels_counties <- sprintf("<strong>%s, %s</strong><br/>%s",
                               counties_plt$county,
                               counties_plt$stateName,
                               rt_labels) %>%
      lapply(htmltools::HTML)
    #boundary <- st_bbox(counties_plt)
    leaflet(counties_plt) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addPolygons_default(
        fillColor = pal(counties_plt[[rt_col]]),
        weight = 0.5, label = labels_counties,
        layer = counties_plt$geoid) %>%
      addLegend_default(pal = pal, values = counties_plt[[rt_col]])
  })

  output$explore_states_counties <- renderPlot({
  #output$explore_states_counties <- renderPlot({
    if (input$state_select == "") {
      return()
    }
    plt_data_pruned <- county_rt_long %>%
      filter(stateName == input$state_select, !is.na(Rt_plot)) %>%
      filter(county != "Unassigned", !startsWith(county, "Out of")) %>%
      mutate(County = factor(county),
             `Rt Bins` = cut(Rt_plot, breaks = bins,
                             include.lowest = TRUE, right = FALSE)) %>%
      rename(Rt = Rt_plot)
    color_pal <- rev(brewer.pal(9, "RdYlBu"))
    names(color_pal) <- levels(plt_data_pruned$`Rt Bins`)

    plt_title <- sprintf("Rt for %s Counties", input$state_select)
    p <- plt_data_pruned %>%
      #ggplot(aes(x = date, y = county, color = Rt_plot, size = `Total Cases`)) +
      ggplot(aes(x = date, y = County, fill = `Rt Bins`, labels = Rt)) +
      geom_tile(alpha = 0.7) +
      scale_fill_manual(drop = FALSE, values = color_pal) +
      xlab("Date") + ylab("County") +
      ggtitle(plt_title) +
      theme_cowplot() +
      scale_x_date(date_minor_breaks = "1 day") +
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
