library(dplyr)
library(plyr)

setwd('C:/RedfinHousingDatasets')

temp <- list.files(pattern="*.csv")
tables <- lapply(temp, read.csv)
full <- do.call(rbind, tables)

df <- full %>%
  filter(LIST.PRICE >= 150000, LIST.PRICE <= 400000, BEDS >= 2, BATHS >= 2, !is.na(ADDRESS), HOME.TYPE %in% "Single Family Residential") %>%
  mutate(ppsf = LIST.PRICE / SQFT) %>%
  mutate(Address = paste(ADDRESS,CITY,STATE)) %>%
  arrange(ppsf) %>%
  select(Address)

addressList <- head(df$Address,10)

for (i in addressList) {
  browseURL(paste0('https://www.google.com/search?q=',i))
}
