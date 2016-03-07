library(rvest)
library(dplyr)

statelist <- c('ma','ri')

for (i in statelist) {

  url <- read_html(paste0("http://zipatlas.com/us/",'ri',"/city-comparison/median-household-income.htm"))
  
  table <- url %>%
    html_nodes("table") %>% 
    .[[13]] %>%
    html_table(header=T)
  
  table$'#' <- NULL
  table$`City Report` <- NULL
  
  table1 <- table %>% 
    mutate(population = as.numeric(gsub(",","", Population))) %>%
    mutate(avginchh = as.numeric(gsub("\\$|,|\\.00","", `Avg. Income/H/hold`))) %>%
    mutate(location = gsub("\\(|\\)|[0-9]+", "", `Location (# Zip Codes)`)) %>%
    mutate(nationalRank = as.numeric(gsub("#|,",'',`National Rank`))) %>%
    filter(avginchh >= 80000,avginchh <= 106000, population >= 1000, population <= 21000, nationalRank < 300) %>%
    select(location,population,avginchh,nationalRank) %>%
    arrange(nationalRank)
  
  citylist <- head(table1$location,10)
  
  
  for (loc in citylist) 
    {
    browseURL(paste0('https://www.google.com/#q=',loc,'redfin'))
  }

}
