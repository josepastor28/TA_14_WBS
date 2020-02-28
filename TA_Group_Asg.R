rm(list=ls())
library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(RSQLite)
library(readr)

options(stringsAsFactors = FALSE)

#get url
airbnb_file_list <- "http://insideairbnb.com/get-the-data.html"

#read html
airbnb_web <- read_html(airbnb_file_list)

#Each city; get each node (for looping urls)
city_list_node <- airbnb_web %>% html_nodes('div.contentContainer table')

#Each city split into: city, province, country
#city_names <- airbnb_web %>% html_nodes('div.contentContainer h2') %>% html_text()

#province_names <- NULL
#country_names <- NULL
#for(i in 1:length(city_names)) {
#  split_city_name <- strsplit(city_names[i], split=',')[[1]]
#  city_names[i] <- split_city_name[1]
#  province_names[i] <- split_city_name[2]
#  country_names[i] <- split_city_name[3]
#}

conn <- dbConnect(RSQLite::SQLite(), "airbnb.db")

for(i in 1:2){ #length(city_list_node)
  tryCatch({
    urls <- city_list_node[i] %>% html_nodes('tbody td a') %>% html_attr('href')
    ############################
    #listing
    ############################
    url_listing <- urls[grepl("listings.csv.gz", urls)]
    url_listing_size <- length(url_listing)
    for (j in 1:2) { #url_listing_size
      #get download path
      urlsplit_listing <- strsplit(url_listing[j], split='/')[[1]]
      prefix_listing <- urlsplit_listing[6] #city name
      suffix_listing <- urlsplit_listing[length(urlsplit_listing)]
      filename_listing <- paste(prefix_listing, suffix_listing, sep = "_")
      finalpath_listing <- paste0(getwd(),"/", filename_listing)
      #download listing_file
      download.file(url_listing[j],finalpath_listing)
      #Read csv.gz file
      
      listing_file <- read.csv(filename_listing)
      #Transformation
      listing_file <- listing_file %>% select(c(1,5,6,7,8,10,11,12,13,14,15,20,22,23,24,26,27,29,32,33,34,36,37,38,39,42,43,44,48,52,53,54,55,56,57,58,59,61,66,68,69,83,84,87,88,89,90,91,92,93,97,98,99))
      #Load to database
      dbWriteTable(conn,"Listing", listing_file, append = TRUE)
      
      #Free space
      if (file.exists(filename_listing)) 
        #Delete file if it exists
        file.remove(filename_listing)
    }
    
    ############################
    #calendar
    ############################
    url_calendar <- urls[grepl("calendar.csv.gz", urls)]
    url_calendar_size <- length(url_calendar)
    for(j in 1:2) { #url_calendar_size
      #get download path
      urlsplit_calendar <- strsplit(url_calendar[j], split='/')[[1]]
      prefix_calendar <- urlsplit_calendar[6] #city name
      suffix_calendar <- urlsplit_calendar[length(urlsplit_calendar)]
      filename_calendar <- paste(prefix_calendar, suffix_calendar, sep = "_")
      finalpath_calendar <- paste0(getwd(),"/", filename_calendar)
      #download calendar_file
      download.file(url_calendar[j],finalpath_calendar)
      #Read csv.gz file
      
      calendar_file <- read.csv(filename_calendar)
      #Transformation
      
      #Load to database
      dbWriteTable(conn,"Calendar", calendar_file, append = TRUE)
      
      #Free space
      if (file.exists(filename_calendar)) 
        #Delete file if it exists
        file.remove(filename_calendar)
    }
    
    ############################
    #review
    ############################
    url_review <- urls[grepl("reviews.csv.gz", urls)]
    url_review_size <- length(url_review)
    for(j in 1:2) {#url_review_size
      #get download path
      urlsplit_review <- strsplit(url_review[j], split='/')[[1]]
      prefix_review <- urlsplit_review[6] #city name
      suffix_review <- urlsplit_review[length(urlsplit_review)]
      filename_review <- paste(prefix_review, suffix_review, sep = "_")
      finalpath_review <- paste0(getwd(),"/", filename_review)
      #download review_file
      download.file(url_review[j],finalpath_review)
      #Read csv.gz file
      
      review_file <- read.csv(filename_review)
      #Transformation
      
      review_content <- data.frame(review_file$listing_id, review_file$id, review_file$date, review_file$comments)
      reviewer <- data.frame(review_file$id, review_file$reviewer_name)
      #Load to database
      dbWriteTable(conn,"Reviews", review_content, append = TRUE)
      dbWriteTable(conn,"Reviewer", reviewer, append = TRUE)
      #Free space
      if (file.exists(filename_review)) 
        #Delete file if it exists
        file.remove(filename_review)
    }
  }, error=function(e){
    print(0)
  })
}

dbListTables(conn)
#dbRemoveTable(conn,"Listing")
#dbRemoveTable(conn,"Reviewer")
#dbRemoveTable(conn,"Reviews")