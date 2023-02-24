# countries
library(leaflet)
library(httr)
library(sf)
library(dplyr)

# handle rest api for geojson output of world countries service from esri
url <- parse_url('https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services')
url$path <- paste0(url$path, '/World_Countries_(Generalized)/FeatureServer/0/query')
url$query <- list(outFields = '*',
                  f = 'geojson',
                  where = '1=1')
request <- build_url(url)

# get data
countries <- st_read(request)

# dummy selection data
test_country_data <- data.frame(
  countries = c('US', 'IN', 'RU'),
  selected = as.factor('selected')
)

# add dummy selection data
countries <- countries |> 
  left_join(
    test_country_data,
    by = c('ISO' = 'countries')
  )

# create custom palette
my_pal <- colorFactor(
  palette = 'RdYlBu',
  domain = countries$selected,
  na.color = NA
)

# simple viz
leaflet(countries) |> 
  # addProviderTiles(providers$CartoDB.Positron) |> 
  addPolygons(
    fillColor = ~my_pal(selected),
    stroke = TRUE,
    fillOpacity = 0.7,
    color = 'white',
    weight = 0.5,
    label = 'test_label'
  ) |>
  setView(lat= 0, lng = 10 , zoom = 1.3)
