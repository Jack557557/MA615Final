


library(rvest)
library(tidyverse)
library(data.table)
library(DT)
library(magrittr)
library(digest)
library(RPostgreSQL)
library(tidytext)
library(config)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)
library(stringr)
library(zoo)
library(knitr)

listings <- data.frame(title=character(),
                       company=character(), 
                       location=character(), 
                       summary=character(), 
                       link=character(), 
                       description = character(),
                       stringsAsFactors=FALSE) 


## you can put in a bigger number than 100

for (i in seq(0, 1000, 10)){
  url_ds <- paste0('https://www.indeed.com/jobs?q=data+scientist%2C+statistics&start=',i)
  var <- read_html(url_ds)

  
  #job title
  title <-  var %>% 
    html_nodes('#resultsCol .jobtitle') %>%
    html_text() %>%
    str_extract("(\\w+.+)+") 
  
  #company
  company <- var %>% 
    html_nodes('#resultsCol .company') %>%
    html_text() %>%
    str_extract("(\\w+).+") 
  
  #location
  location <- var %>%
    html_nodes('#resultsCol .location') %>%
    html_text() %>%
    str_extract("(\\w+.)+,.[A-Z]{2}")   
  #summary
  summary <- var %>%
    html_nodes('#resultsCol .summary') %>%
    html_text() %>%
    str_extract(".+")
  
  #link
  link <- var %>%
    html_nodes('#resultsCol .jobtitle .turnstileLink, #resultsCol a.jobtitle') %>%
    html_attr('href') 
  link <- paste0("https://www.indeed.com",link)
  
  listings <- rbind(listings, as.data.frame(cbind(title,
                                                  company,
                                                  location,
                                                  summary,
                                                  link)))
}




datatable(listings)


listings$uniqueid <- mapply(function(x, y, z) digest(paste0(x,y,z)), listings$title, listings$location, listings$company)
#remove duplicate unique ids
listings %<>%
  distinct(uniqueid, .keep_all = TRUE)
#remove duplicate links
listings %<>%
  distinct(link, .keep_all = TRUE)
datatable(listings)




for (i in (1:length(listings$link))){
  desciption <- tryCatch(
    html_text(html_node(read_html(as.character(listings$link[i])),'.jobsearch-JobComponent-description')),
    error=function(e){NA}
  )
  if (is.null(desciption)){
    desc <- NA
  }
  listings$description[i] <- desciption
}



datatable(listings)


#output your data
#write.csv(listings, file = "C:/Users/Jack-/Desktop/615final project/listings1.csv")





