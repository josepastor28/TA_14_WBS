##=================      AIRBNB ASSIGNMENT        =================##
##                           Group 14                              ##
##-----------------------------------------------------------------##

##=================      1. load Libraries        =================##
##-----------------------------------------------------------------##
rm(list=ls())
options(message=FALSE)
options(warning=FALSE)
options(stringsAsFactors = FALSE)
library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(RSQLite)
library(readr)
library(gutenbergr)
library(tidytext)
library(forcats)
library(ggplot2)
library(rJava)
library(qdap)
library(SnowballC)
library(tm)
library(tidyr)
library(igraph)
library(ggraph)
library(textcat)
library(stringdist)
library(factoextra)
library(udpipe)

#### Parallel Cores

library(parallel)
library(doParallel)
numCores <- detectCores()
registerDoParallel(numCores)

#### Stop Words

data("stop_words")


##=================         2. Get Data           =================##
##-----------------------------------------------------------------##
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

##=================    3. Generate Dataframe      =================##
##-----------------------------------------------------------------##

names(listing_file)[names(listing_file) == 'id'] <- 'listing_id' #Rename columns
names(review_content)[names(review_content) == 'review_file.listing_id'] <- 'listing_id' #Rename columns
names(review_content)[names(review_content) == 'review_file.id'] <- 'review_id' #Rename columns
review_content$review_file.date <- NULL
review_content$review_file.comments <- NULL
review_file$reviewer_id <- NULL
review_file$reviewer_name <- NULL
review_file$listing_id <- NULL
names(review_file)[names(review_file) == 'id'] <- 'review_id' #Rename columns
airbnb_df <- left_join(review_content, review_file, by=c("review_id"))
airbnb_df <- left_join(airbnb_df, listing_file, by=c("listing_id"))

airbnb_df_copy <- data.frame(airbnb_df) #safe copy
airbnb_df_copy$language <- textcat(airbnb_df_copy$comments)
airbnb_df_copy <- airbnb_df_copy %>% 
  filter(language=="english") %>% 
  select(doc_id,comments)
  
##=================     4. Feature Selection      =================##
##-----------------------------------------------------------------##

filter_features <- function(df, vars_drop){
  return(df %>% select(-vars_drop))}

vars_drop <- c(
  "zipcode", 
  "number_of_reviews_ltm",
  "host_id"
)

airbnb_df_copy <- filter_features(airbnb_df_copy, vars_drop)

##=================   5. Text Pre-Processing      =================##
##-----------------------------------------------------------------##

text_preprocessing <- function(x){
  x <- iconv(x, "latin1", "ASCII", "")
  x <- tolower(x)
  x<-removePunctuation(x)
  x <- rm_stopwords(x, Top100Words, separate = F)
  x <- replace_contraction(x)
  character_removal = c("!", "\\?", "\\)", "\\(", '-')
  for (i in 1:length(character_removal)){
    x<-gsub(character_removal[i], "", as.character(x))}
  return(x)
}


airbnb_df_copy$comments <- text_preprocessing(airbnb_df_copy$comments)
airbnb_df_copy$comments[1]

##=================         5. Find Bi-Grams      =================##
##-----------------------------------------------------------------##
count_grams <- function(dataset, n = 2) {
  if (n == 2){dataset %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word,
             !word2 %in% stop_words$word) %>%
      count(word1, word2, sort = TRUE)
  } else {
    dataset %>%
      unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
      separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
      filter(!word1 %in% stop_words$word,
             !word2 %in% stop_words$word,
             !word3 %in% stop_words$word) %>%
      count(word1, word2, word3, sort = TRUE)
  }
  
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

separate_grams <- function(grams, n, min_f){
  if (n == 2){
    grams %>%
      filter(n > min_f,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d"))
  } else if (n == 3){
    grams %>%
      filter(n > min_f,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d"),
             !str_detect(word3, "\\d"))
  }  
}


df <- gutenberg_download(10)
df <- data.frame(text = airbnb_df_copy$comments)

min_f <-  400
n = 2

df_bigrams <- df %>%
  count_grams(n)

separate_grams(df_bigrams, n, min_f) %>%
  visualize_bigrams()

####

##=================  6. Word Frequency Analysis   =================##
##-----------------------------------------------------------------##

#Word Frequency Analysis

word_frequency <- function(df, vec_stopwords, character_removal, top_n, col_n){
  df_words <- df %>%
    unnest_tokens(word, text) %>%
    count(aggregation, word, sort = TRUE)
  
  stopwords <- tibble(word = vec_stopwords)
  
  df_words <- anti_join(df_words, stopwords, 
                        by = "word")
  
  plot_df <- df_words %>%
    bind_tf_idf(word, aggregation, n) %>%
    mutate(word = str_remove_all(word, character_removal)) %>%
    group_by(aggregation) %>% 
    top_n(top_n, tf_idf) %>%
    ungroup() %>%
    mutate(word = reorder_within(word, tf_idf, aggregation)) %>%
    mutate(author = factor(aggregation, levels = unique(df$aggregation)))
  
  ggplot(plot_df, aes(word, tf_idf, fill = aggregation)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~aggregation, ncol = col_n, scales = "free") +
    coord_flip() +
    scale_x_reordered()  
}

#Input
colnames(airbnb_df)
unique(airbnb_df$room_type)

#Define the dataframe
text <- 'comments'
aggregation <- 'room_type'
vec_subset <- c(text, aggregation)
df <- airbnb_df[,c(text, aggregation)] %>% 
  setnames(old = vec_subset, new = c('text','aggregation'))

#Pre-procesing

stop_words <- pull(stop_words, word)
vec_stopwords = c("eq", "co", "rc", "ac", "ak", "bn", 
                  "fig", "file", "cg", "cb", "cm",
                  "ab", "_k", "_k_", "_x")
character_removal = c("_")
top_n <- 15 #Top number for words
col_n <- 2 #Two columns in final wrap

#Execute function
word_frequency(df, vec_stopwords, character_removal, top_n, col_n)


##====   7. Diversity / Formality / Polarity / Dispersion   =======##
##-----------------------------------------------------------------##

plot_diversity <- function(df){
  plot(diversity(df$text, df$aggregation))
}

plot_formality <- function(df){
  plot(formality(df$text, df$aggregation))
}

plot_polarity <- function(df){
  plot(polarity(df$text, df$aggregation))
}

plot_dispersion <- function(df, vec){
  dispersion_plot(df$text, vec, df$aggregation)}

library(data.table)
#Define the dataframe
text <- 'comments'
aggregation <- 'room_type'
vec_subset <- c(text, aggregation)
df <- airbnb_df[,c(text, aggregation)] %>% 
    setnames(old = vec_subset, new = c('text','aggregation'))

plot_diversity(df)
plot_formality(df)
plot_polarity(df)
plot_dispersion(df)