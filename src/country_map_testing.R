# countries
library(leaflet)
library(sf)
library(dplyr)
library(countrycode)
library(htmltools)
library(glue)

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

# # dummy selection data
# test_country_data <- data.frame(
#   countries = c('US', 'IN', 'RU'),
#   selected = as.factor('selected')
# )
# 
# # add dummy selection data
# countries <- countries |> 
#   left_join(
#     test_country_data,
#     by = c('ISO' = 'countries')
#   )

# create custom palette
my_pal <- colorQuantile(
  palette = 'Blues',
  domain = map_data$highest_rating,
  n = 5,
  na.color = 'grey'
)

# simple viz
leaflet(map_data) |> 
  # addProviderTiles(providers$CartoDB.Positron) |> 
  addPolygons(
    fillColor = ~my_pal(highest_rating),
    stroke = TRUE,
    fillOpacity = 0.7,
    color = 'white',
    weight = 0.5,
    label = 'test_label'
  ) |>
  setView(lat= 20, lng = 10 , zoom = 1.5)
