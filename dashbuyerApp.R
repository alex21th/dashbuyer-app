
# Dependencies ####

# If a package is installed, it will be loaded. If any 
# are not, the missing package(s) will be installed 
# from CRAN and then loaded.

# First specify the packages of interest
packages = c("shiny",
             "shinydashboard",
             "dplyr",
             "tidyr",
             "purrr",
             "glue",
             "tictoc",
             "stringr",
             "ggplot2",
             "plotly",
             "RColorBrewer",
             "dygraphs",
             "xts",
             "tidyverse",
             "lubridate")

# Now load or install & load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Source files ####

source("functions.R")

## Load datasets, data, functions

DATAFRAMES_PATHS <- "~/datbuyer/dashbuyer/dataframes/"

if (length(dir(DATAFRAMES_PATHS)) == 0) {
  print("Creating RDS files...")
  # LoadObject("~/datbuyer/data/preprocessed/tickets_processed.csv") # Raw data (preprocessed)
  # LoadObject("~/datbuyer/data/preprocessed/analytics_processed.csv")
  # LoadObject("~/datbuyer/output/matches/matches_frac_nomargin.csv") # Final matches
} else {
  print("Reading from previously saved RDS files.")
}

# Always load saved RDS data
tickets_processed <- LoadObject(paste0(DATAFRAMES_PATHS, "tickets_processed.RDS"))
analytics_processed <- LoadObject(paste0(DATAFRAMES_PATHS, "analytics_processed.RDS"))
matches <- LoadObject(paste0(DATAFRAMES_PATHS, "matches_frac_nomargin.RDS"))

# Aggregated dataframe
aggregated_tickets <- matches %>%
  left_join(tickets_processed, by = c("Client_id" = "client")) %>% 
  select(-Prob_cond_client, -Prob_cond_hash) %>%
  arrange(datetime)

aggregated_cycles <- aggregated_tickets %>%
  left_join(analytics_processed, by = c("Hash" = "hash", "store" = "local")) %>% 
  drop_na() %>% 
  removeDuplicates() %>%
  arrange(first_timestamp)

# Segmentation
gps <- LoadObject(paste0(DATAFRAMES_PATHS, "gps-weighted.RDS"))
matches_classified <- LoadObject(paste0(DATAFRAMES_PATHS, "matches_classified.RDS"))

# stores list
stores <- aggregated_tickets %>% pull(store) %>% unique() %>% sort()

# app.R ####

# ui ####
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Dashbuyer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("My store", tabName = "my_store", icon = icon("shopping-cart")),
      menuItem("Geo-info", tabName = "maps", icon = icon("map-marked-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "my_store",
              h2(textOutput("selected_store")),
              fluidRow(
                sidebarPanel(
                  selectInput("store", "Select your store:", choices = stores, selected = 7606)
                ),
                valueBoxOutput("matches_local"),
                sidebarPanel(
                  uiOutput("choose_client")
                )
              ),
              tabsetPanel(
                selected = "Information of selected client",
                tabPanel("Store information",
                  h2("Overview of store"),
                  
                  fluidRow(
                    box(
                      width = 12,
                      collapsible = T,
                      collapsed = T,
                      title = "Location Insights", solidHeader = TRUE, status = "info",
                      infoBoxOutput("attractions"),
                      infoBoxOutput("hotels"),
                      infoBoxOutput("restaurants"),
                      "*based on Tripadvisor location data"
                    )
                  ),
                 
                  # Info boxes
                  fluidRow(
                    infoBoxOutput("store_registered_tickets"),
                    infoBoxOutput("store_stay_time"),
                    infoBoxOutput("store_times_nobuying")
                  ),
                
                  fluidRow(
                    tabBox(
                      width = 12,
                      title = tagList(icon("chart-bar"), "Purchases overview"),
                      tabPanel("Plot", plotlyOutput("store_tickets_plot")),
                      tabPanel("Data", DT::dataTableOutput("store_tickets_table"))
                    )
                  ),
                  
                  fluidRow(
                    tabBox(
                      width = 12,
                      title = tagList(icon("chart-bar"), "Accesses overview"),
                      tabPanel("Plot", plotlyOutput("store_cycles_plot")),
                      tabPanel("Data", DT::dataTableOutput("store_cycles_table"))
                    )
                  )
                ),
                
                
                tabPanel("Information of selected client",
                  h2(textOutput("selected_client")),
                  
                  fluidRow(
                    box(
                      width = 12,
                      collapsible = T,
                      collapsed = T,
                      title = "Client Type", solidHeader = TRUE, status = "warning",
                      infoBoxOutput("resident"),
                      infoBoxOutput("tourist")
                    )
                  ),
                  
                  fluidRow(
                    infoBoxOutput("registered_tickets"),
                    infoBoxOutput("stay_time"),
                    infoBoxOutput("times_nobuying")
                  ),
                  
                  fluidRow(
                    tabBox(
                      width = 12,
                      title = tagList(icon("chart-bar"), "Purchases overview"),
                      tabPanel("Plot", plotlyOutput("tickets_plot")),
                      tabPanel("Data", DT::dataTableOutput("tickets_table"))
                    )
                  ),
                  
                  fluidRow(
                    tabBox(
                      width = 12,
                      title = tagList(icon("chart-bar"), "Accesses overview"),
                      tabPanel("Plot", plotlyOutput("cycles_plot")),
                      tabPanel("Data", DT::dataTableOutput("cycles_table"))
                    )
                  )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "maps",
              h2("Geo-information about your local zone"),
              fluidRow(
                tabBox(
                  width = 12,
                  title = tagList(icon("map-marked-alt"), "Interactive maps"),
                  tabPanel("Attractions", htmlOutput("attractions_frame")),
                  tabPanel("Hotels", htmlOutput("hotels_frame")),
                  tabPanel("Restaurants", htmlOutput("restaurants_frame"))
                )
              )
      )

    )
  )
)

server <- function(input, output) {
  
  # HTML renders
  
  output$attractions_frame <- renderUI({
    tags$iframe(style="height:600px; width:100%", src="weights-attractions.html")
  })
  
  output$hotels_frame <- renderUI({
    tags$iframe(style="height:600px; width:100%", src="weights-hotels.html")
  })
  
  output$restaurants_frame <- renderUI({
    tags$iframe(style="height:600px; width:100%", src="weights-restaurants.html")
  })
  
  # Data filtering
  
  aggregated_tickets_by_store <- reactive({
    aggregated_tickets %>% filter(store == input$store)
  })
  
  aggregated_cycles_by_store <- reactive({
    aggregated_cycles %>% filter(store == input$store)
  })
  
  clients_list <- reactive({
    # aggregated_tickets_by_store() %>% pull(Client_id) %>% unique() %>% sort()
    # aggregated_cycles_by_store() %>% drop_na() %>% pull(Client_id) %>% unique() %>% sort()
    aggregated_cycles_by_store() %>% pull(Client_id) %>% unique() %>% sort()
  })
  
  gps_by_store <- reactive({
    gps %>% filter(id == input$store)
  })
  
  output$choose_client <- renderUI({
    selectInput("client", "Select a client:", choices = clients_list(), selected = 174897)
  })
  
  output$selected_client <- renderText({
    glue("Overview of client: {req(input$client)}")
  })
  
  output$selected_store <- renderText({
    glue("My store: {input$store}")
  })
  
  output$matches_local <- renderValueBox({
    valueBox(
      format(clients_list() %>% length(), big.mark = ".", decimal.mark = ","), "MATCHES", icon = icon("random"),
      color = "orange"
    )
  })
  
  # Information of selected client
  
  output$registered_tickets <- renderInfoBox({
    infoBox(title = "Registered purchases",
            subtitle = paste0("since ", aggregated_tickets_by_store_and_client() %>% first() %>% pull(datetime) %>% as.character()),
            value = aggregated_tickets_by_store_and_client() %>% nrow() %>% format(big.mark = ".", decimal.mark = ",") %>% paste("tickets"),
            icon = icon("receipt"), 
            color = "green"
    )
  })
  
  output$stay_time <- renderInfoBox({
    infoBox(title = "Average stay-time",
            subtitle = "in the shop",
            value = aggregated_cycles_by_store_and_client() %>% pull(duration) %>% mean() %>% round(0) %>% paste("min"),
            icon = icon("stopwatch"),
            color = "blue"
    )
  })
  
  n_times_nobuying <- reactive({
    last_ticket <- aggregated_tickets_by_store_and_client() %>% 
      mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")) %>% 
      pull(datetime) %>% last()
    aggregated_cycles_by_store_and_client() %>% 
      mutate(first_timestamp = as.POSIXct(first_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")) %>% 
      filter(first_timestamp > last_ticket) %>% nrow()
  })
  
  output$times_nobuying <- renderInfoBox({
    infoBox(title = "Times seen without buying",
            subtitle = "since last purchase",
            value = paste(n_times_nobuying(), "out of", aggregated_cycles_by_store_and_client() %>% nrow(), "access(es)"),
            icon = icon("eye"),
            color = "red"
    )
  })
  
  ## Store information
  
  # Location Insights
  
  output$attractions <- renderValueBox({
    valueBox(
      subtitle = "ATTRACTIONS relevance",
      value = paste(gps_by_store() %>% pull(attractions) %>% `*`(100) %>% round(2) %>% format(big.mark = ".", decimal.mark = ","), "%"),
      icon = icon("archway"),
      color = "light-blue"
    )
  })
  
  output$hotels <- renderValueBox({
    valueBox(
      subtitle = "HOTELS relevance",
      value = paste(gps_by_store() %>% pull(hotels) %>% `*`(100) %>% round(2) %>% format(big.mark = ".", decimal.mark = ","), "%"),
      icon = icon("hotel"),
      color = "teal"
    )
  })
  
  output$restaurants <- renderValueBox({
    valueBox(
      subtitle = "RESTAURANTS relevance",
      value = paste(gps_by_store() %>% pull(hotels) %>% `*`(100) %>% round(2) %>% format(big.mark = ".", decimal.mark = ","), "%"),
      icon = icon("utensils"),
      color = "aqua"
    )
  })
  
  ## Matches part
  
  # Tourist or resident
  
  output$resident <- renderValueBox({
    valueBox(
      subtitle = "Probability of being a RESIDENT",
      value = if_else(nrow(matches_classified_by_hash()) > 0, paste(matches_classified_by_hash() %>% pull(prob_resident) %>% `*`(100) %>% round(2) %>% format(big.mark = ".", decimal.mark = ","), "%"), "not available"),
      icon = icon("home"),
      color = "purple"
    )
  })
  
  output$tourist <- renderValueBox({
    valueBox(
      subtitle = "Probability of being a TOURIST",
      value = if_else(nrow(matches_classified_by_hash()) > 0, paste(matches_classified_by_hash() %>% pull(prob_tourist) %>% `*`(100) %>% round(2) %>% format(big.mark = ".", decimal.mark = ","), "%"), "not available"),
      icon = icon("plane-departure"),
      color = "olive"
    )
  })
  
  # Client metrics
  
  output$store_registered_tickets <- renderInfoBox({
    infoBox(title = "Registered purchases",
            subtitle = paste0("since ", aggregated_tickets_by_store() %>% first() %>% pull(datetime) %>% as.character()),
            value = aggregated_tickets_by_store() %>% nrow() %>% format(big.mark = ".", decimal.mark = ",") %>% paste("tickets"),
            icon = icon("receipt"), 
            color = "green"
    )
  })
  
  output$store_stay_time <- renderInfoBox({
    infoBox(title = "Average stay-time",
            subtitle = "in the shop",
            value = aggregated_cycles_by_store() %>% pull(duration) %>% mean() %>% round(0) %>% paste("min"),
            icon = icon("stopwatch"),
            color = "blue"
    )
  })
  
  output$store_times_nobuying <- renderInfoBox({
    infoBox(title = "Clients seen without buying",
            subtitle = "since last purchase",
            # value = paste(clients_nobuying(aggregated_tickets_by_store(), aggregated_cycles_by_store(), clients_list()) %>% round(2) %>% format(big.mark = ".", decimal.mark = ","), "%"),
            value = paste(gps_by_store() %>% pull(pct_nobuying) %>% round(2) %>% format(big.mark = ".", decimal.mark = ","), "%"),
            icon = icon("user-friends"),
            color = "yellow"
    )
  })
  
  # Aggregated data by CLIENT
  
  aggregated_tickets_by_store_and_client <- reactive({
    # aggregated_tickets_by_store() %>% filter(Client_id == req(input$client)) %>% arrange(datetime)
    aggregated_tickets_by_store() %>% filter(Client_id == req(input$client))
  })
  
  aggregated_cycles_by_store_and_client <- reactive({
    aggregated_cycles_by_store() %>% filter(Client_id == req(input$client))
  })
  
  matches_classified_by_hash <- reactive({
    matches_classified %>% filter(hash == (aggregated_cycles_by_store_and_client() %>% first() %>% pull(Hash)))
  })
  
  # Tickets of store information
  
  output$store_tickets_table = DT::renderDataTable({
    aggregated_tickets_by_store()
  })
  
  output$store_tickets_plot <- renderPlotly({
    TicketsPlot(aggregated_tickets_by_store())
  })
  
  # Cycles of store information
  
  output$store_cycles_table = DT::renderDataTable({
    aggregated_cycles_by_store()
  })
  
  output$store_cycles_plot <- renderPlotly({
    CyclesPlot(aggregated_cycles_by_store())
  })
  
  # Tickets of selected client
  
  output$tickets_table = DT::renderDataTable({
    aggregated_tickets_by_store_and_client()
  })
  
  output$tickets_plot <- renderPlotly({
    TicketsPlot(aggregated_tickets_by_store_and_client())
  })
  
  # Cycles of selected client
  
  output$cycles_table = DT::renderDataTable({
    aggregated_cycles_by_store_and_client()
  })
  
  output$cycles_plot <- renderPlotly({
    CyclesPlot(aggregated_cycles_by_store_and_client())
  })
}

shinyApp(ui, server)