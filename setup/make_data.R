library(httr)
library(sf)

# countries layer ----

## api call ----

### esri url for all rest calls
esri_url <- 'https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/'
url <- parse_url(esri_url)

### add request for the service of interest
url$path <- paste0(
  url$path, 
  'World_Countries_(Generalized)/FeatureServer/0/query'
)

### define query
url$query <- list(
  outFields = '*',
  f = 'geojson',
  where = '1=1' # I don't know what this does but request breaks without it :)
)

### and put together the link
request <- build_url(url)

## convert to sf object ----
countries_sf <- st_read(request)

## save object ----
saveRDS(countries_sf, 'src/data/countries_sf')
