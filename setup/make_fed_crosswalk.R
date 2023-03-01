library(dplyr)
library(rvest)

# using https://www.mark-weeks.com/aboutcom/mw15b16.htm
url <- 'http://www.mark-weeks.com/aboutcom/mw15b16.htm'
page <- rvest::read_html(url)
tbls <- html_nodes(page, "table")

# get crosswalk
fed_crosswalk <- html_table(tbls[[2]])

# save crosswalk as .Rds object
saveRDS(fed_crosswalk, 'src/data/fed_crosswalk.Rds')
