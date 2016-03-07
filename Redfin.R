library(dplyr)
library(plyr)
library(RCurl)
library(ggplot2)

setwd('~/HouseFinder/HousingDatasets')

temp = list.files(pattern="*.csv")

tables <- lapply(temp, read.csv)
full <- do.call(rbind, tables)

df <- full %>%
  select(ADDRESS,
         CITY,
         STATE,
         LOT.SIZE,
         LIST.PRICE,
         SQFT,
         LAST.SALE.DATE,
         LAST.SALE.PRICE, 
         HOME.TYPE, 
         BEDS, 
         BATHS,
         YEAR.BUILT) %>%
  mutate(Address = paste(ADDRESS, CITY, STATE)) %>%
  mutate(AcreLot = LOT.SIZE * 2.2957e-5) %>%
  mutate(ppsfcurr = LIST.PRICE / SQFT) %>%
  mutate(ppsfprev = LAST.SALE.PRICE / SQFT) %>%
  mutate(ppsfdiff = ppsfcurr - ppsfprev) %>%
  mutate(lastsaledate = as.numeric(substr(LAST.SALE.DATE,1,4))) %>%
  mutate(markup = LIST.PRICE / LAST.SALE.PRICE) %>%
  mutate(yrsheld = 2016 - lastsaledate) %>%
  mutate(ppsfyrinc = ppsfdiff / yrsheld) %>%
  mutate(fiveyrnewprice = ((ppsfcurr + (ppsfyrinc * 5)) * SQFT)) %>%
  mutate(fiveyrprof = fiveyrnewprice - LIST.PRICE) %>%
  filter(HOME.TYPE %in% "Single Family Residential",!is.na(ppsfprev),YEAR.BUILT != 2016) %>%
         yrsheld >= 10, 
         fiveyrprof > 30000,
         markup < 10,
         YEAR.BUILT + yrsheld < 2016 - 10) %>%
  select(Address,
         CITY,
         LIST.PRICE,
         AcreLot,
         ppsfcurr,
         ppsfprev,
         ppsfdiff,
         markup,
         yrsheld,
         YEAR.BUILT,
         ppsfyrinc,
         fiveyrnewprice,
         fiveyrprof) %>%
  arrange(desc(fiveyrprof))

addressList <- head(df$Address,15)

for (i in addressList) {
  browseURL(paste0('https://www.google.com/search?q=',i))
}

qplot(yrsheld,
      LIST.PRICE,
      data=df, 
      color = CITY, 
      geom= c('point','smooth'))
