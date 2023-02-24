# countries
library(leaflet)
library(sf)

# handle rest api for geojson output of world countries service from esri
url <- parse_url('https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services')
url$path <- paste0(url$path, '/World_Countries_(Generalized)/FeatureServer/0/query')
url$query <- list(outFields = '*',
                  f = 'geojson',
                  where = '1=1')
request <- build_url(url)

# get data
countries <- st_read(request)

# simple viz
leaflet(countries) |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addPolygons(
    fillColor = 'red',
    stroke = TRUE,
    fillOpacity = 0.2,
    color = 'white',
    weight = 0.5,
    label = 'test_label'
  ) |>
  setView(lat= 0, lng = 10 , zoom = 1.3)
