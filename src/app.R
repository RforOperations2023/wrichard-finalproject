library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(sf)
library(dplyr)
library(countrycode)
library(htmltools)
library(scales)
library(plotly)
library(DT)

# functions ----

make_ratings_year_subset <- function(data, input) {
  req(input$Byear_range) # make sure selected
  data |> 
    filter(
      Byear >= input$Byear_range[[1]],
      Byear <= input$Byear_range[[2]]
    ) %>%
    return()
}

make_country_subset <- function(data, input) {
  # only filter if group is Fed
  if (input$group == 'Fed') {
    data <- data |> 
      filter(
        is.element(
          Fed, input$fed_selected
        )
      ) 
  }
  
  return(data)
}

make_data_subset <- function(data, input) {
  data <- data |> 
    make_ratings_year_subset(input = input) |>
    make_country_subset(input = input) 
  
  return(data)
}

get_top_players <- function(data, input) {
  top_tbl <- data |> 
    arrange(desc(!!sym(input$time))) |> 
    head(100) 
  
  return(top_tbl)
}

make_top_datatable <- function(top_tbl) {
  dt <- top_tbl |> 
    DT::datatable(
      options = list(lengthChange = FALSE)
    )
  
  return(dt)
}

get_top_countries <- function(data, input) {
  top_tbl <- data |> 
    arrange(desc(!!sym(input$time)))
  
  return(top_tbl)
}

make_country_datatable <- function(top_tbl) {
  dt <- top_tbl |> 
    DT::datatable(
      options = list(lengthChange = FALSE)
    ) |> 
    formatRound('players', digits = 0)
}

# viz ----

## labels ----
pretty_labels <- list(
  'Bdecade' = 'Birth decade',
  'Sex' = 'Sex',
  'Fed' = 'Federation',
  'None' = NULL,
  'SRtng' = 'Standard Time Control Elo Score',
  'RRtng' = 'Rapid Time Control Elo Score',
  'BRtng' = 'Bullet Time Control Elo Score'
)

## distributions ----
make_dist_plot <- function(data, input) {
  # save options for easy access
  .plot_type <- input$plot_type
  .group <- input$group
  .bins <- 50
  .time <- input$time
  
  # save string of number of players
  player_count_string <- 
    scales::comma_format()(
      nrow(data)
    )
  
  # settings for all
  plot_settings <- list(
    theme_classic(),
    labs(
      caption = 'Data: FIDE, January 2023\nViz: @bristowrichards'
    ),
    xlab(pretty_labels[.time]),
    theme(plot.title = element_text(size=18))
  )
  
  # list options
  plot_list <- list(
    # histogram
    'hist' = list(
      geom_histogram(
        # if fill is null, defaults to ggplot grey, which is fine
        aes(fill = if(.group != 'None') {.data[[.group]]}),
        bins = .bins,
        color = 'black'
      ),
      labs(
        title = 'Chess ELO Rating Histogram Plot',
        fill = pretty_labels[.group]
      ),
      scale_fill_viridis_d(),
      ylab('Frequency')
    ),
    
    # density
    'dens' = list(
      geom_density(
        aes(color = if(.group != 'None') .data[[.group]]),
        linewidth = 1
      ),
      labs(
        title = 'Chess ELO Rating Density Plot',
        color = pretty_labels[.group]
      ),
      scale_color_viridis_d(end = 0.9),
      ylab('Density'),
      ggtitle('hello')
    ),
    
    # frequency
    'freq' = list(
      geom_freqpoly(
        aes(color = if(.group != 'None') .data[[.group]]),
        bins = .bins,
        linewidth = 1
      ),
      labs(
        title = 'Chess ELO Rating Frequency Plot',
        color = pretty_labels[.group]
      ),
      scale_color_viridis_d(end = 0.9),
      ylab('Frequency')
    )
  )
  
  plt <- 
    ggplot(
      data = data,
      aes(x = .data[[.time]])
    ) + list(
      plot_settings,
      plot_list[.plot_type]
    )
  
  return(plt)
}

## bubble ----
make_scatter_plot <- function(data, input) {
  # save options for easy access
  .time <- input$time
  .highlight_elo <- input$highlight_elo
  
  # save string of number of players
  player_count_string <- 
    scales::comma_format()(
      nrow(data)
    )
  
  # settings for all
  plot_settings <- list(
    theme_classic(),
    labs(
      caption = 'Data: FIDE, January 2023\nViz: @bristowrichards'
    ),
    xlab(paste('Average', pretty_labels[.time])),
    theme(plot.title = element_text(size=18))
  )
  
  # data
  data <- make_ratings_year_subset(data, input) |> 
    group_by(Fed) |> 
    summarize(
      player_count = n(),
      average_elo = mean(!!sym(input$time), na.rm = TRUE),
      average_age = mean(Age, na.rm = TRUE),
      has_top_players = factor(any(!!sym(input$time) > .highlight_elo))
    ) 
  
  plt <- ggplot(
    data, 
    aes(
      Federation = Fed,
      x = average_elo, 
      y = average_age, 
      size = player_count,
      color = has_top_players
    )
  ) + 
    geom_point(alpha = 0.7) +
    scale_color_manual(
      values = c('FALSE' = "grey40",
                 'TRUE' = 'green4')
    ) +
    scale_size(range = c(1, 10), name = 'Player Count') +
    labs(
      color = 'Has Top Performer'
    ) +
    ylab('Average Age') +
    plot_settings
  
  return(plt)
}

## map data ----
make_fed_summary <- function(data) {
  data <- data |> 
    group_by(Country, iso2c) |> 
    summarize(
      players = n(),
      SRtng = mean(tail(sort(SRtng), 10)),
      RRtng = mean(tail(sort(RRtng), 10)),
      BRtng = mean(tail(sort(BRtng), 10))
    ) |> 
    rename(ISO = iso2c)
  
  return(data)
}

## map palette ----
make_pal <- function(data, input) {
  pal <- colorBin(
    palette = 'Blues',
    domain = data[[input$time]],
    bin = 5,
    na.color = 'grey'
  )
}

# ui ----
ui <- dashboardPage(
  
  ## header ----
  dashboardHeader(
    title = 'test title re: ELO'
  ),
  
  ## sidebar ----
  dashboardSidebar(
    ### menu ----
    sidebarMenu(
      menuItem(
        'Country Map', 
        tabName = 'tab_map', 
        icon = icon('globe', lib = 'glyphicon')
      ),
      menuItem(
        'Top Countries',
        tabName = 'tab_countries',
        icon = icon('queen', lib = 'glyphicon')
      ),
      menuItem(
        'Elo Distributions', 
        tabName = 'tab_dist', 
        icon = icon('pawn', lib = 'glyphicon')
      ),
      menuItem(
        'Country Scatterplot', 
        tabName = 'tab_scatter', 
        icon = icon('knight', lib = 'glyphicon')
      ),
      menuItem(
        'Top Players', 
        tabName = 'tab_players', 
        icon = icon('king', lib = 'glyphicon')
      )
    ),
    
    ### user inputs ----
    # time control
    radioButtons(
      inputId = 'time',
      label = 'Time control:',
      choices = c(
        'Standard' = 'SRtng',
        'Rapid' = 'RRtng',
        'Bullet' = 'BRtng'
      )
    ),
    
    # birth year range
    sliderInput(
      inputId = 'Byear_range',
      label = 'Birth year:',
      min = 1910,
      max = 2020,
      value = c(1930, 2020),
      sep = ''
    )
  ),
  ## dashboard body ----
  dashboardBody(
    tabItems(
      
      ### 1. Map ----
      tabItem(
        tabName = 'tab_map',
        box(
          # fill
          width = 3
        ),
        box(
          leafletOutput('map'),
          width = 9
        )
      ),
      
      ### 2. Top Countries ----
      tabItem(
        tabName = 'tab_countries',
        DT::dataTableOutput(outputId ='dt_countries')
      ),
    
      ### 3. Elo Dashboard ----
      tabItem(
        tabName = 'tab_dist',
        box(
          title = 'Distribution Plot', status = 'primary', solidHeader = TRUE,
          plotlyOutput('distribution_plot', height = 350),
          width = 9
        ),
        
        valueBoxOutput('dist_total', width = 3),
        
        box(
          title = 'Inputs', status = 'warning', solidHeader = TRUE,
          width = 3,
          
          # select grouping
          radioButtons(
            inputId = 'group',
            label = 'Select grouping',
            choices = c(
              'Birth decade' = 'Bdecade',
              'Sex' = 'Sex',
              'Federation' = 'Fed',
              'None' = 'None'
            )
          ),
          
          # select country if country is selected
          conditionalPanel(
            condition = "input.group == 'Fed'",
            selectizeInput(
              inputId = 'fed_selected',
              label = 'Select Federations (max 7)',
              choices = readRDS('data/federations.Rds'),
              selected = c('USA', 'RUS', 'IND'),
              multiple = TRUE,
              options = list(maxItems = 7)
            )
          ),
          
          # graph type
          radioButtons(
            inputId = 'plot_type',
            label = 'Select graph type',
            choices = c(
              'Histogram' = 'hist',
              'Density Curve' = 'dens',
              'Frequency Curve' = 'freq'
            )
          )
        )
      ),
      
      ### 4. Scatter Plot ----
      tabItem(
        tabName = 'tab_scatter',
        
        # show plot
        box(
          title = 'Federation Scatter Plot', 
          status = 'primary',
          solidHeader = TRUE,
          plotlyOutput('federation_scatter_plot', height = 350),
          width = 8
        ),
        
        # inputs
        box(
          title = 'Inputs', status = 'warning', solidHeader = TRUE,
          width = 4,
          
          # birth year range
          sliderInput(
            inputId = 'highlight_elo',
            label = 'Highlight federations with players above:',
            min = 1000,
            max = 2800,
            value = 2700,
            sep = ''
          )
        ),
        
        valueBoxOutput('v_fed_total'),
        valueBoxOutput('v_fed_highlight')
      ),
      
      ### 5. Top Players ----
      tabItem(
        tabName = 'tab_players',
        h2('Top Players'),
        h4(paste('This table contains the highest rated players, ',
                 'filtered by your input and arranged by your ',
                 'time control selection')),
        DT::dataTableOutput(outputId = 'dt_players')
      )
    )
  )
)

# server ----
server <- function(input, output) {
  ## data static ----
  data = list()
  data$ratings <- readRDS('data/ratings.Rds')
  data$countries_sf <- readRDS('data/countries_sf.Rds')
  
  # data reactive ----
  data$dist_ratings_subset <- reactive(
    make_data_subset(
      data = data$ratings,
      input = input
    )
  )
  
  data$fed_summary <- reactive(
    make_fed_summary(
      data = data$dist_ratings_subset()
    )
  )
  
  ## spatial join ----
  data$map_data <- reactive({
    map_data <- data$countries_sf |> 
        left_join(data$fed_summary(), by = c('iso' = 'ISO'))
    
    return(map_data)
  })
  
  ## spatial palette ----
  data$pal <- reactive(
    make_pal(data = data, input = input)
  )
  
  ## plots ----
  # distribution
  output$distribution_plot <- renderPlotly({
    make_dist_plot(
      data = data$dist_ratings_subset(),
      input = input
    )
  })
  
  # bubble
  output$federation_scatter_plot <- renderPlotly({
    make_scatter_plot(
      data = data$ratings,
      input = input
    )
  })
  
  ## datatables ----
  output$dt_players <- renderDataTable(
    make_top_datatable(
      get_top_players(
        data$dist_ratings_subset(),
        input
      )
    )
  )
  
  output$dt_countries <- renderDataTable(
    make_country_datatable(
      get_top_countries(
        data$fed_summary(),
        input
      )
    )
  )
  
  ## value boxes ----
  # federations total
  output$v_fed_total <- renderValueBox({
    n_fed <- data$dist_ratings_subset() |> 
      select(Fed) |> 
      table() |> 
      names() |> 
      length()
    
    valueBox(
      n_fed, 
      'Total Federations Shown', 
      icon = icon('globe', lib = 'glyphicon')
    )
  })
  
  # federations highlighted
  output$v_fed_highlight <- renderValueBox({
    n_highlight <- data$dist_ratings_subset() |> 
      group_by(Fed) |> 
      summarize(
        has_top_player = any(!!sym('SRtng') > input$highlight_elo)
      ) |> summarize(n = sum(has_top_player))
    
    valueBox(
      n_highlight, 
      'Total Federations Highlighted', 
      icon = icon('star', lib = 'glyphicon')
    )
  })
  
  # distribution total 
  output$dist_total <- renderValueBox({
    n_dist <- data$dist_ratings_subset() |> 
      nrow()
    
    valueBox(
      label_comma()(n_dist), 
      'Total Players Shown', 
      icon = icon('user', lib = 'glyphicon')
    )
  })
  
  ## map ----
  output$map <- renderLeaflet({
    leaflet(data$map_data()) |> 
      addTiles() |> 
      addPolygons(
        fillColor = 'blue',
        stroke = TRUE,
        fillOpacity = 1,
        color = 'white',
        weight = 0.5,
        label = ~htmlEscape(country),
        popup = ~paste0('<b>', country, '</b><br>',
                        'highest rating: ', SRtng, '<br>',
                        'players: ', label_comma()(players)),
        highlightOptions = highlightOptions(
          weight = 5,
          color = 'green',
          opacity = 1,
          bringToFront = TRUE)
      ) |>
      # addLegend('bottomright', pal = my_pal, values = ~highest_rating,
      #           title = 'Highest Elo',
      #           opacity = 1
      # ) |> 
      setView(lat= 20, lng = 10 , zoom = 1.5)
    
  })
}

shinyApp(ui, server)
