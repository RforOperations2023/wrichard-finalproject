# countries
library(leaflet)
library(sf)
library(dplyr)
library(countrycode)
library(htmltools)
library(scales)

# get data
ratings <- readRDS('src/data/ratings.Rds')
countries_sf <- readRDS('src/data/countries_sf.Rds')

# basic data summary for join attempt
fed_counts <- ratings |> 
  group_by(iso2c) |> 
  summarize(
    players = n(),
    highest_rating = max(SRtng, na.rm = TRUE)
  )

# attempt merge and visualize
map_data <- countries_sf |> 
  left_join(fed_counts, by = c('iso' = 'iso2c'))

# create custom palette
my_pal <- colorBin(
  palette = 'Blues',
  domain = map_data$highest_rating,
  bin = 5,
  na.color = 'grey'
)

# simple viz
leaflet(map_data) |> 
  addPolygons(
    fillColor = ~my_pal(highest_rating),
    stroke = TRUE,
    fillOpacity = 1,
    color = 'white',
    weight = 0.5,
    label = ~htmlEscape(country),
    popup = ~paste0('<b>', country, '</b><br>',
                    'highest rating: ', highest_rating, '<br>',
                    'players: ', label_comma()(players)),
    highlightOptions = highlightOptions(
      weight = 5,
      color = 'green',
      opacity = 1,
      bringToFront = TRUE)
  ) |>
  addLegend('bottomright', pal = my_pal, values = ~highest_rating,
            title = 'Highest Elo',
            opacity = 1
  ) |> 
  setView(lat= 20, lng = 10 , zoom = 1.5)
