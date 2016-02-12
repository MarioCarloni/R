library(rvest)
library(dplyr)

statelist <- c('ma','ri')

for (i in statelist) {

  url <- read_html(paste0("http://zipatlas.com/us/",i,"/city-comparison/median-household-income.htm"))
  
  table <- url %>%
    html_nodes("table") %>% 
    .[[13]] %>%
    html_table(header=T)
  
  table$'#' <- NULL
  table$`City Report` <- NULL
  table$`National Rank` <- NULL
  
  table$Population <- as.numeric(gsub(",","", table$Population))
  table$`Avg. Income/H/hold` <- as.numeric(gsub("\\$|,|\\.00","", table$`Avg. Income/H/hold`))
  table$`Location (# Zip Codes)` <- gsub("\\(|\\)|[0-9]+", "", table$`Location (# Zip Codes)`)
  
  
  table1 <- table %>% 
    filter(`Avg. Income/H/hold` >= 70000,`Avg. Income/H/hold` <= 106000, Population >= 1000, Population <= 15000) %>%
    mutate(percapita.index = `Avg. Income/H/hold` / Population) %>%
    arrange(desc(percapita.index))
  
  citylist <- head(table1$`Location (# Zip Codes)`,10)
  
  
  for (loc in citylist) 
    {
    browseURL(paste0('https://www.google.com/#q=',loc,' redfin'))
  }

}
