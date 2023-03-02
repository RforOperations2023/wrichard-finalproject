# trying to link two-letter ISO country codes to three-letter codes from FIDE
library(dplyr)
library(countrycode)
library(openxlsx)

# country codes
codes <- countrycode::codelist

# ratings tabulated by federation
ratings <- readRDS('src/data/ratings.Rds')
fed_counts <- ratings |> 
  group_by(Fed) |> 
  summarize(players = n())
rm(ratings)

write.xlsx(codes, 'setup/countries.xlsx')
write.xlsx(fed_counts, 'setup/fed_counts.xlsx')

# crosswalk file
# fed_crosswalk <- readRDS('src/data/fed_crosswalk.Rds')


# joined <- fuzzyjoin::stringdist_full_join(
#   codes, 
#   fed_crosswalk, 
#   by = c('country.name.en' = 'Country')
# ) |> select(Fed, Country, country.name.en)


# joined <- fed_counts |>
#   full_join(codes, by = c('Fed' = 'iso3c'), keep = TRUE) |>
#   select(Fed, players, iso3c, iso.name.en) |>
#   arrange(desc(players))
# 
# joined_feds <- fed_counts |> 
#   full_join(fed_crosswalk, by = 'Fed') |> 
#   arrange(desc(players))
# 
# problems <- joined |> 
#   filter(if_any(.fns = is.na))

