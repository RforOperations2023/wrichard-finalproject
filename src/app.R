library(shiny)
library(shinythemes)
library(leaflet)

ui <- navbarPage(
  title = 'test title re: ELO',
  tabPanel(
    title = 'world map',
    sidebarLayout(
      # sidebar
      sidebarPanel(
        selectInput(
          inputId = 'test_country_select',
          label = 'test country selection',
          choices = c('USA', 'India', 'Russia', 'Brazil', 'Nigeria'),
          selected = 'USA'
        )
      ),
      # main page
      mainPanel(
        leafletOutput('map')
      )
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles()
  })
}

shinyApp(ui, server)
