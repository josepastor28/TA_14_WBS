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
library(maps)
library(ggplot2)
library(mapdata)
library(sp)
library(ggplot2)
library(data.table)
library(wordcloud)
library(stringr)
library(stringi)
library(readr)
library(dplyr)
library(data.table)
# Download Forms 10-K/10Q from SEC
library(edgar)
# For sentiment datasets
library(sentimentr)
library(tidytext)
library(rvest)
library(ggplot2)
library(tidyr)
library(reshape2)
library(remotes)
#install_github("EmilHvitfeldt/textdata")
#install_github("juliasilge/tidytext")
library(tidytext)
library(FactoMineR)
library(ggrepel)
library(SentimentAnalysis)
library(qdap)
library(dplyr)
library(lubridate)
library(ggmap)
library(XML)
library(cld2)
library(stm)
library(Rtsne)
library(rsvd)
library(geometry)

#ggmap(thismap)
register_google(key = "mykey")

#### Parallel Cores

library(parallel)
library(doParallel)
numCores <- detectCores()
registerDoParallel(numCores)

#### Stop Words

data("stop_words")
set.seed(1234)


##=================         1. Download Data           =================##
##-----------------------------------------------------------------##

url_fetch <- "http://insideairbnb.com/get-the-data.html"

get_the_data <- read_html(url_fetch)

tables <- get_the_data %>% html_nodes("table")

con <- dbConnect(SQLite(), "airbnb2cities.db")

for(i in 1:2){
  table_h <- tables[[i]] %>% html_table()
  
  all_links <- tables[[i]] %>% html_nodes("a") %>% html_attr("href")
  
  # Add it on the dataframe
  table_h$links <- all_links
  
  # Before doing anything lets tidy up the column names 
  colnames(table_h) <- gsub(" ","_",tolower(colnames(table_h)))
  colnames(table_h) <- gsub("/","_",colnames(table_h))
  
  # Convert the date to a date using the as.Date
  table_h$date_compiled <- as.Date(table_h$date_compiled,format = "%d %b,%Y")
  
  # Review Data 
  this_review_data <- table_h %>% arrange(desc(date_compiled)) %>% 
    filter(grepl("Detailed Review",description)) %>% 
    top_n(1)
  
  
  # Listings Data 
  this_listings_data <- table_h %>% arrange(desc(date_compiled)) %>% 
    filter(grepl("Detailed Listings",description)) %>% 
    top_n(1)
  
  # Calendar Data
  this_calendar_data <- table_h %>% arrange(desc(date_compiled)) %>% 
    filter(grepl("Detailed Calendar",description)) %>% 
    top_n(1)
  
  this_city <- tolower(this_listings_data$country_city[1])
  dir.create("temp")
  temp_city_folder <-paste0("temp/",this_city) 
  if(!dir.exists(temp_city_folder)){
    
    print(paste0("Ok now we are creating the folder for",temp_city_folder))
    dir.create(temp_city_folder)
    
    download.file(url = this_listings_data$links[1],destfile = paste0(temp_city_folder,"/listings.csv.gz"))
    download.file(url = this_review_data$links[1],destfile = paste0(temp_city_folder,"/reviews.csv.gz"))
    download.file(url = this_calendar_data$links[1],destfile = paste0(temp_city_folder,"/calendar.csv.gz"))
  }
  
  listings_df <- read_csv(paste0(temp_city_folder, "/listings.csv.gz"))
  reviews_df <- read_csv(paste0(temp_city_folder, "/reviews.csv.gz"))
  calendar_df <- read_csv(paste0(temp_city_folder, "/calendar.csv.gz"))
  
  listings_df <- listings_df %>% select(c("id", "description", "neighborhood_overview", "transit", "access", "host_name", "room_type", 
                                          "property_type", "price", "cleaning_fee", "review_scores_rating", "instant_bookable", "cancellation_policy",
                                          "longitude", "latitude")) %>%
    rename(listing_id = id) %>%
    mutate(city_typed = this_city) %>%
    mutate(deslanguage = detect_language(description)) %>%
    filter(deslanguage=="en")
  
  reviews_df <- reviews_df %>% select(c("listing_id", "date", "comments", "reviewer_id")) %>%
    mutate(deslanguage_review = detect_language(comments)) %>%
    filter(deslanguage_review=="en")
  
  calendar_df <- calendar_df %>% select(c("date", "price", "listing_id")) 
  
  dbWriteTable(con, "listings", listings_df, append=TRUE)
  dbWriteTable(con, "reviews", reviews_df, append=TRUE)
  dbWriteTable(con, "calendar", calendar_df, append=TRUE)
}



##=================         2. Get Data           =================##
##-----------------------------------------------------------------##
con <- dbConnect(SQLite(), "airbnb2cities.db")
listing <- dbSendQuery(con, "SELECT * FROM listings")
listing <- dbFetch(listing)
reviews <- dbSendQuery(con, "SELECT * FROM reviews")
reviews <- dbFetch(reviews)

dbDisconnect(con)

nrow(listing)
nrow(reviews)

##=================    3. Generate Dataframe      =================##
##-----------------------------------------------------------------##

#names(listing_file)[names(listing_file) == 'id'] <- 'listing_id' #Rename columns
#names(review_content)[names(review_content) == 'review_file.listing_id'] <- 'listing_id' #Rename columns
#names(review_content)[names(review_content) == 'review_file.id'] <- 'review_id' #Rename columns
#review_content$review_file.date <- NULL
#review_content$review_file.comments <- NULL
#review_file$reviewer_id <- NULL
#review_file$reviewer_name <- NULL
#review_file$listing_id <- NULL
#names(review_file)[names(review_file) == 'id'] <- 'review_id' #Rename columns
#airbnb_df <- left_join(review_content, review_file, by=c("review_id"))
airbnb_df <- left_join(reviews, listing, by=c("listing_id"))

##=================     4. Feature Selection      =================##
##-----------------------------------------------------------------##

#Preprocessing
#Listing
#features_listing <- c("id", "description", "neighborhood_overview", "transit", "access", "host_name", "room_type", "property_type", "price", "cleaning_fee", "review_scores_rating", "instant_bookable", "cancellation_policy", "longitude", "latitude")
#listing$description_combined <- paste0(listing$description, listing$neighborhood_overview, listing$transit, listing$access)
#listing <- listing[,features_listing]
#listing$city_typed <-"Amsterdam"
airbnb_df <-airbnb_df[airbnb_df$property_type %in% c("Apartment", "House", "Townhouse", "Loft", "Boat"),]
#names(listing)[names(listing) == 'id'] <- 'listing_id' #Rename columns
#Reviews
#features_reviews <- c("listing_id", "date", "comments")
#reviews <- reviews[,features_reviews]
#Calendar
#features_calendar <- c("date", "price", "listing_id")




#names(listing)[names(listing) == 'id'] <- 'listing_id'
#listing_tr <- listing[c("listing_id", "description", "price", "review_scores_rating")]

#reviews_tr <- reviews[c("listing_id","comments")]

#reviews_tr %>% 
#  group_by(listing_id = listing_id) %>% 
#  summarise(comments = paste0(comments, collapse = " ")) -> reviews_tr

#airbnb_df_db <- left_join(reviews_tr, listing_tr, by=c("listing_id"))

#airbnb_df_db$comments <- text_preprocessing(airbnb_df_db$comments)
#airbnb_df_db$description <- text_preprocessing(airbnb_df_db$description)



#filter_features <- function(df, vars_drop){
#  return(df %>% select(-vars_drop))}

#vars_drop <- c(
#  "zipcode", 
#  "number_of_reviews_ltm",
#  "host_id"
#)

#airbnb_df_copy <- filter_features(airbnb_df, vars_drop)

##=================   5. Text Pre-Processing      =================##
##-----------------------------------------------------------------##

text_preprocessing <- function(x){
  x <- iconv(x, "latin1", "ASCII", "")
  x <- tolower(x)
  x <- replace_contraction(x)
  x <- removePunctuation(x)
  x <- rm_stopwords(x, Top100Words, separate = F)
  return(x)
}

reviews$comments <- text_preprocessing(reviews$comments)
listing$description_combined <- text_preprocessing(listing$description)
airbnb_df <- left_join(reviews, listing, by=c("listing_id"))

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

df <- data.frame(text = airbnb_df$comments)


min_f <-  100
n = 3
separate_grams(count_grams(df, n), n, min_f) %>%
  visualize_bigrams()

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
  
  return(plot_df)
}

visualize_frequency <- function(plot_df){
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
vec_stopwords = c(c("eq", "co", "rc", "ac", "ak", "bn", 
                    "fig", "file", "cg", "cb", "cm",
                    "ab", "_k", "_k_", "_x"), stop_words$word)
character_removal = c("_")

#Get list of bi-grams, tri-grams
#data("stop_words")
#bi_grams <- na.omit(transform(separate_grams(count_grams(df, 2), 2, 100), newcol=paste(word1, word2, sep="_"))$newcol[1:20])
#tri_grams <- na.omit(transform(separate_grams(count_grams(df, 3), 3, 100), newcol=paste(word1, word2, word3, sep="_"))$newcol[1:20])
#keep <- c(bi_grams, tri_grams)
#df$text <- list(space_fill(df$text, keep))
#head(df)

top_n <- 15 #Top number for words
col_n <- 2 #Two columns in final wrap

#Execute function

plot_df <- word_frequency(df, vec_stopwords, character_removal, top_n, col_n)
visualize_frequency(plot_df)

plot_df$word <- data.frame(str_split_fixed(plot_df$word, "___", 2))$X1
wordcloud(words = plot_df$word, freq = plot_df$n, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
##===================  Part A.c.1 Readability    ===================##
##-----------------------------------------------------------------##

```{r}

readability_data <- listings %>%
  select(id,description,price,review_scores_rating)

readability_data$id <- as.factor(readability_data$id)
```


```{r}

readability_data$description <- iconv(readability_data$description, "latin1", "ASCII", "")
readability_data$description <- gsub("[^ -~]", "", readability_data$description)
readability_data <- na.omit(readability_data)


```


```{r}

readabilitydata_split <- sentSplit(readability_data,"description")

```

```{r old}

readability <- flesch_kincaid(readability_data$description, grouping.var = readability_data$id)



```

```{r new}

readability <- flesch_kincaid(readabilitydata_split$description, grouping.var = readabilitydata_split$id)



```



```{r flesch_kincaid}

readability_data$readability <- readability$Readability$FK_read.ease

```


```{r flesch_kincaid}

ggplot(readability_data) + geom_point(aes(x = readability,y = review_score))

ggplot(readability_data) + geom_jitter(aes(x = readability,y = review_score))

```


```{r flesch_kincaid}

readability_linear_regression <- lm(review_score ~ readability,data =readability_data)
summary(readability_linear_regression)
```



```{r automated_readability_index}

readability_automated_readability_index <- automated_readability_index(readability_data$description, grouping.var = readability_data$id)



```

```{r automated_readability_index new}

readability_automated_readability_index <- automated_readability_index(readabilitydata_split$description, grouping.var = readabilitydata_split$id)



```


```{r automated_readability_index}

readability <- left_join(readability_data,readability_automated_readability_index$Readability)
readability <- na.omit(readability)
```



```{r}
ggplot(readability) + geom_point(aes(x = Automated_Readability_Index,y = review_scores_rating))

```
```{r}
readability_wooutlinear <- readability %>% 
  filter(Automated_Readability_Index > -10 & Automated_Readability_Index < 60)


```

```{r}
scatter <- ggplot(readability_wooutlinear) + geom_jitter(aes(x = Automated_Readability_Index,y = review_scores_rating))

```

```{r}
heat <- ggplot(readability_wooutlinear) + geom_bin2d(aes(x = Automated_Readability_Index,y = review_scores_rating))

```

```{r}
scatter
heat
```


```{r}

readability_linear_regression_automated_readability_index <- lm(review_scores_rating ~ Automated_Readability_Index,data =readability)
summary(readability_linear_regression_automated_readability_index)
```

##===================  Part A.c2.name_mention   ===================##
##-----------------------------------------------------------------##

```{r}

mentionnames <- reviews %>%
  select(listing_id,comments) %>%
  filter(!str_detect(comments,"This is an automated posting."))



```


```{r}

names <- listings %>%
  select(id,host_name) %>%
  mutate(listing_id = id)

names <- names[,-1]


```


```{r}

detectmentionnames <- left_join(mentionnames,names)

```


```{r}
na.omit(detectmentionnames)
```



```{r}
mentioned <-  vector()

for (i in 1:length(detectmentionnames$listing_id)){
  exist <- str_detect(detectmentionnames$comments[i],detectmentionnames$host_name[i])
  mentioned <- append(mentioned,exist)
}
```


```{r temp}

load("mentioned.RData")

```


```{r}

detectmentionnames$mentioned <- mentioned

```


```{r}

mentionrate <- detectmentionnames %>%
  group_by(listing_id) %>%
  summarise(mentionrate = mean(mentioned))

```


```{r}

listing_neat <- listings %>%
  select(id,price,review_scores_rating) %>%
  mutate(listing_id = id)

listing_neat <- listing_neat[,-1]


```


```{r}

listing_mention <- left_join(listing_neat,mentionrate)
listing_mention <- na.omit(listing_mention)

```


```{r}

ggplot(listing_mention) + geom_jitter(aes(x = mentionrate,y = review_scores_rating))

```

```{r}

ggplot(listing_mention) + geom_bin2d(aes(x = mentionrate,y = review_scores_rating),bins = 40)

```



```{r}

ggplot(listing_mention) + stat_binhex(aes(x = mentionrate,y = review_scores_rating))

```

```{r}

mentionratelm <- lm(review_scores_rating ~ mentionrate,data = listing_mention) 
summary(mentionratelm)
```

##=================Part A.d.description sentiment==================##
##-----------------------------------------------------------------##

```{r}

polaritydata <- listings %>%
  select(id,description)

```

```{r}
polaritydata$description <- gsub('[0-9]+', '', polaritydata$description)
polaritydata <- na.omit(polaritydata)

```

```{r}
polaritydata_tokenised <- polaritydata %>%
  unnest_tokens(word, description)

```


```{r}

data(stop_words)
polaritydata_cleaned <- anti_join(polaritydata_tokenised,stop_words)

```



```{r}
bing <- get_sentiments(lexicon = "bing")  
pos_neg <- polaritydata_cleaned %>% inner_join(bing)

```

```{r}
pos_neg <- pos_neg %>% group_by(id) %>% count(sentiment) %>% spread(sentiment,n,fill=0)%>% mutate(polarity = positive - negative) 

```

```{r}

polarityprice <- listings %>%
  select(id,price,review_scores_rating) %>%
  left_join(pos_neg) %>%
  na.omit()

```
```{r}
polarityprice$price <- as.numeric(str_replace(polarityprice$price,"\\$",""))
polarityprice <- na.omit(polarityprice)
```



```{r}
ggplot(polarityprice) + geom_jitter(aes(x = polarity,y = price))
```


```{r}
ggplot(polarityprice) + geom_bin2d(aes(x = polarity,y = price,bins = 50))
```

```{r}
ggplot(polarityprice) + geom_jitter(aes(x = polarity,y = review_scores_rating))
```


```{r}
ggplot(polarityprice) + geom_bin2d(aes(x = polarity,y = review_scores_rating,bins = 50))
```



```{r}
polar
```


##===================  8. Sentiment Analysis    ===================##
##-----------------------------------------------------------------##

###################

############## Rating

names(listing)[names(listing) == 'id'] <- 'listing_id'
listing_tr <- listing[c("listing_id", "description", "price", "review_scores_rating")]

reviews_tr <- reviews[c("listing_id","comments")]

reviews_tr %>% 
  group_by(listing_id = listing_id) %>% 
  summarise(comments = paste0(comments, collapse = " ")) -> reviews_tr

airbnb_df_db <- left_join(reviews, listing, by=c("listing_id"))

airbnb_df_db$comments <- text_preprocessing(airbnb_df_db$comments)
airbnb_df_db$description <- text_preprocessing(airbnb_df_db$description)

#Define the dataframe
text <- 'comments'
aggregation <- 'listing_id'
vec_subset <- c(text, aggregation)
df <- airbnb_df_db[,c(text, aggregation)] %>% 
  setnames(old = vec_subset, new = c('text','id'))

plot_states_affective <- function(df){
  df%>% 
    unnest_tokens(word,text) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("nrc")) %>%
    filter(!(sentiment %in% c("positive","negative"))) %>% 
    group_by(sentiment) %>% 
    summarise(Total=n()) %>% 
    ggplot(.,aes(x=reorder(sentiment,-Total),y=Total,fill=sentiment))+geom_bar(stat = "identity") + xlab("Affective States")
}

plot_scatter_affective <- function(df){
  df %>% 
    unnest_tokens(word,text) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("nrc")) %>%
    group_by(id,sentiment) %>% 
    summarise(total_sentiment = n()) %>% 
    left_join(df%>% 
                unnest_tokens(word,text) %>% 
                anti_join(stop_words) %>% 
                inner_join(get_sentiments("nrc")) %>%
                group_by(id) %>% 
                summarise(total_words =n())) %>% 
    mutate(total_sentiment=total_sentiment/total_words)  %>% 
    acast(id~sentiment,
          value.var = "total_sentiment",fill=0) %>%
    as.data.frame() -> sent_transform
  p <- FactoMineR::CA(sent_transform)
  res_df_1 <- data.frame(p$row$coord)
  res_df_2 <- data.frame(p$col$coord)
  res_df_2 <- cbind(sentiment = rownames(res_df_2), res_df_2)
  rownames(res_df_2) <- 1:nrow(res_df_2)
  
  p <- ggplot(data = res_df_1, aes(x = Dim.1, y = Dim.2)) + geom_hex(bins = 50) +   scale_fill_continuous(type = "viridis")
  p <- p + geom_point(data = res_df_2, aes(x=Dim.1, y=Dim.2, color = "red")) + geom_label_repel(data = res_df_2, aes(x=Dim.1, y=Dim.2, label = sentiment, color = "red"), size = 5)
  p
}

plot_states_affective(df)
plot_scatter_affective(df)


## Relationship between price and sentiment

names(listing)[names(listing) == 'id'] <- 'listing_id'
listing_tr <- listing[c("listing_id", "description", "price", "review_scores_rating")]
colnames(listing)
listing$city[1:10]

reviews_tr <- reviews[1:10000,][c("listing_id","date", "comments")]
reviews_tr$date <- as.Date(reviews_tr$date)

reviews_tr %>% 
  group_by(listing_id = listing_id) %>% 
  summarise(Text = paste0(comments, collapse = " ")) -> reviews_tr

airbnb_df_db <- left_join(reviews_tr, listing_tr, by=c("listing_id"))

all_sentiment <- tibble()
for(i in 1:length(airbnb_df_db$Text)){
  this_sentiment <- analyzeSentiment(airbnb_df_db$Text[i])
  all_sentiment <- bind_rows(all_sentiment,
                             this_sentiment)
  print(i)
}

airbnb_df_db <- cbind(data.frame(airbnb_df_db), data.frame(all_sentiment))

pol <- polarity(airbnb_df_db$Text, airbnb_df_db$listing_id)
#plot(pol)
airbnb_df_db$Polarity <- pol$group$ave.polarity
#colnames(airbnb_df_db)


#Regression

airbnb_df_db$price <- parse_number(airbnb_df_db$price)
airbnb_df_db$date <- as.Date(paste(paste(airbnb_df_db$yr, airbnb_df_db$mon, sep="-"),"-01",sep=""))

scatter.smooth(x=airbnb_df_db$price, y=airbnb_df_db$SentimentGI, main="Price ~ Sentiment")  # scatterplot

colnames(airbnb_df_db)

linearMod <- lm(price ~ Polarity + review_scores_rating + WordCount + SentimentGI + NegativityGI + PositivityGI + SentimentHE + NegativityHE + PositivityHE + SentimentLM + NegativityLM + PositivityLM + RatioUncertaintyLM + SentimentQDAP + NegativityQDAP + PositivityQDAP, data=airbnb_df_db)  # build linear regression model on full data
print(linearMod)

modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
modelCoeffs

AIC(linearMod)  #
BIC(linearMod)  #

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(airbnb_df_db), 0.8*nrow(airbnb_df_db))  # row indices for training data
trainingData <- airbnb_df_db[trainingRowIndex, ]  # model training data
testData  <- airbnb_df_db[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(log(price) ~ review_scores_rating + WordCount + SentimentGI + NegativityGI + PositivityGI + SentimentHE + NegativityHE + PositivityHE + SentimentLM + NegativityLM + PositivityLM + RatioUncertaintyLM + SentimentQDAP + NegativityQDAP + PositivityQDAP, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

actuals_preds <- data.frame(cbind(actuals=testData$price, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 58.42%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

airbnb_df_db <- na.omit(airbnb_df_db)
cvResults <- suppressWarnings(CVlm(data = airbnb_df_db, form.lm=price ~ review_scores_rating + WordCount + SentimentGI + NegativityGI + PositivityGI + SentimentHE + NegativityHE + PositivityHE + SentimentLM + NegativityLM + PositivityLM + RatioUncertaintyLM + SentimentQDAP + NegativityQDAP + PositivityQDAP, m=3, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  # => 251.2783 mean squared error


########### Machine Learning

data = airbnb_df_db[,c("price", "review_scores_rating", "WordCount", "SentimentGI", "NegativityGI", "PositivityGI", "SentimentHE", "NegativityHE", "PositivityHE", "SentimentLM", "NegativityLM", "PositivityLM", "RatioUncertaintyLM", "SentimentQDAP", "NegativityQDAP", "PositivityQDAP", "Polarity")]
data <- rbind(data, data)
data <- rbind(data, data)
data <- rbind(data, data)
data <- rbind(data, data)

smp_size <- floor(0.75 * nrow(data))

set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

label <- "price"
task = makeRegrTask(id = "bh", data = train, target = label)
lrn = makeLearner("regr.gbm", par.vals = list(n.trees =
                                                500, interaction.depth = 3))
rdesc = makeResampleDesc("CV", iters = 5)

validation_result <- mlr::resample(
  learner = lrn,
  task = task,
  resampling = rdesc,
  keep.pred = TRUE
)

validation_result

model <- train(lrn, task)
preds <- predict(model, newdata = test)

test_predicted <- bind_cols(test, preds$data)

test_predicted[, c('price', 'response')]

var_importance <- generateFeatureImportanceData(
  task,
  method = "permutation.importance",
  lrn,
  colnames(data)[!colnames(data) %in% label],
  nmc = 50L,
  local = FALSE
)

var_importance <- data.frame(t(data.frame(var_importance$res)))
var_importance$var <- colnames(data)[!colnames(data) %in% label]

ggplot(var_importance, aes(x=reorder(var,mse), y=mse)) +
  geom_bar(stat='identity') +
  coord_flip()


##===================  9. Check with Maps       ===================##
##-----------------------------------------------------------------##

#http://www.statsoft.org/wp-content/uploads/2016/09/Lecture6_HKMapVis.html

#Map Plots
thismap = map_data("world")
ggplot(thismap, aes(long, lat, group=group, fill=region)) + 
  geom_polygon(show.legend = F) +
  ggtitle("Colorful World")

airbnb_df_db <- left_join(airbnb_df_db, listing[,c("listing_id", "longitude", "latitude")], by=c("listing_id"))

mymap = readRDS("C:/Users/Cheche/Documents/TUM/WARWICK/Courses/Text Analytics/Group_Assignment/gadm36_NLD_2_sp.rds") # source: http://gadm.org/

ggplot(mymap, aes(long, lat, group=group)) +
  geom_polygon(fill="white", colour="gray")

p <- ggplot() +
  geom_polygon(data = mymap, aes(long, lat, group=group, fill = id), show.legend = F)

p <- ggmap(get_map("Amsterdam", maptype="roadmap", zoom=14, messaging = FALSE, markers = FALSE))
colnames(airbnb_df_db)
p + geom_point(data=airbnb_df_db, aes(x=airbnb_df_db$longitude, y=airbnb_df_db$latitude, color = SentimentLM, size = 5, aplha = .5))+   scale_color_continuous(type = "viridis")


##==========================  Part C ==============================##
##-----------------------------------------------------------------##
data_for_partc_2_cities <- airbnb_df %>% 
  select(reviewer_id,comments, review_scores_rating, listing_id, price, city_typed) %>% 
  mutate(price = as.numeric(gsub("\\$","",price)))

city_index_amsterdam <- "amsterdam"
partc_amsterdam <- data_for_partc_2_cities[data_for_partc_2_cities$city_typed %in% city_index_amsterdam,]

#####clean up for part c
partc_amsterdam$review_length_chars <- nchar(partc_amsterdam$comments)

hist(partc_amsterdam$review_length_chars,breaks = 200,main = "Review Length(All)")

partc_amsterdam <- partc_amsterdam %>% 
  filter(review_length_chars>144)

hist(partc_amsterdam$review_length_chars,breaks = 200,main = "Review Length(All) -Left trim to 144")

partc_amsterdam <- partc_amsterdam %>% 
  filter(review_length_chars<1000)

hist(partc_amsterdam$review_length_chars,breaks = 200,main = "Review Length(All) -Right trim to 1000")


to_remove_listing_ids <- partc_amsterdam %>% 
  group_by(listing_id) %>% 
  summarise(total = n()) %>% 
  filter(total<=5) %>% pull(listing_id)

partc_amsterdam <- partc_amsterdam %>% 
  filter(!(listing_id %in% to_remove_listing_ids)) 

partc_amsterdam$comments <- gsub('[[:digit:]]+',' ', partc_amsterdam$comments)

partc_amsterdam$comments <- gsub('[[:punct:]]+',' ', partc_amsterdam$comments)

split_size <- 10000
tokens_list_amsterdam <- split(partc_amsterdam, 
                     rep(1:ceiling(nrow(partc_amsterdam)
                                   /split_size), 
                         each=split_size,
                         length.out=nrow(partc_amsterdam)))
tokens_all_amsterdam <- data.frame()
for(i in 1:length(tokens_list)){
  tokens_h <- tokens_list[[i]] %>% 
    unnest_tokens(word,comments) %>%
    count(word,listing_id) %>%
    anti_join(stop_words)
  print(i)
  tokens_all_amsterdam <- bind_rows(tokens_all,tokens_h)
}

tokens_all_amsterdam$token_length <- nchar(tokens_all_amsterdam$word)
tokens_all_amsterdam %>% group_by(token_length) %>% summarise(total =n())

tokens_all_amsterdam <- tokens_all_amsterdam %>% 
  filter(token_length>2)

tokens_all_amsterdam %>% group_by(token_length) %>% 
  summarise(total =n()) %>% 
  arrange(desc(token_length))

tokens_all_amsterdam <- tokens_all_amsterdam %>% 
  filter(token_length<=15)

tokens_all_tf_idf_amsterdam <- tokens_all_amsterdam %>% 
  bind_tf_idf(word,listing_id,n)

hist(tokens_all_tf_idf_amsterdam$tf_idf,breaks = 200,main="TF-IDF plot")

tokens_all_tf_idf_amsterdam <- tokens_all_tf_idf_amsterdam %>% 
  filter(tf_idf<0.2)

hist(tokens_all_tf_idf_amsterdam$tf_idf,breaks = 200,main="TF-IDF plot")

tokens_all_tf_idf_amsterdam <- tokens_all_tf_idf %>% 
  filter(tf_idf<0.05)

hist(tokens_all_tf_idf_amsterdam$tf_idf,breaks = 200,main="TF-IDF plot")

tokens_all_tf_idf_amsterdam <- tokens_all_tf_idf_amsterdam %>% 
  filter(tf_idf>0.001)

#save(tokens_all_tf_idf,file="review_tokens.rda")
#####


#load("review_tokens.rda")

# language <- udpipe_download_model(language="english",overwrite = F)
ud_model <- udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")

# udpipe::udpipe_annotate(test$comments,doc_id = test$review_id,object = ud_model) %>% as.data.frame() %>% filter(upos %in% c("NOUN","ADJ","ADV")) %>% select(doc_id,lemma) %>% group_by(doc_id) %>% summarise(annotated_comments = paste(lemma, collapse = " ")) %>% View(.)

annotated_reviews_all_amsterdam <- data.frame()
split_size <- 100

for_pos_list_amsterdam <- split(partc_amsterdam, 
                      rep(1:ceiling(nrow(partc_amsterdam)
                                    /split_size), 
                          each=split_size,
                          length.out=nrow(data_for_partc)))

# for(i in 1:length(for_pos_list)){
for(i in 1:3){
  
  udpipe_annotate(for_pos_list[[i]]$comments,
                  doc_id = for_pos_list[[i]]$reviewer_id,
                  object = ud_model) %>% 
    as.data.frame() %>% 
    filter(upos %in% c("NOUN","ADJ","ADV")) %>%
    select(doc_id,lemma) %>% 
    group_by(doc_id) %>% 
    summarise(annotated_comments = paste(lemma, collapse = " ")) %>% 
    rename(reviewer_id = doc_id) -> this_annotated_reviews
  
  print(paste(i,"from",length(for_pos_list)))
  annotated_reviews_all <- bind_rows(annotated_reviews_all_amsterdam,
                                     this_annotated_reviews)
}


########
# Prepare metadata for the STM model
annotated_reviews_all_amsterdam$reviewer_id <- as.integer(annotated_reviews_all_amsterdam$reviewer_id)

annotated_reviews_all_amsterdam %>% left_join(partc_amsterdam) -> partc_amsterdam

processed <- textProcessor(partc_amsterdam$annotated_comments,
                           metadata = partc_amsterdam,
                           customstopwords = c("airbnb",city_index),
                           stem = F)

threshold <- round(1/100 * length(processed$documents),0)

out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta,
                     lower.thresh = threshold)

#K <- c(2,3,4,5,6,7,10,15) 
#kresult <- searchK(out$documents, out$vocab, K, prevalence =~ price+review_scores_rating, data=out$meta)
#plot(kresult)


airbnbfit <- stm(documents = out$documents,
                 vocab = out$vocab,
                 K = 0,
                 prevalence =~ price+review_scores_rating,
                 max.em.its = 75, 
                 data = out$meta,
                 reportevery=3,
                 # gamma.prior = "L1",
                 sigma.prior = 0.7,
                 init.type = "Spectral")

# get the dominant word for each topic
cloud(airbnbfit,topic = 1)
cloud(airbnbfit,topic = 2)
cloud(airbnbfit,topic = 3)
cloud(airbnbfit,topic = 4)
cloud(airbnbfit,topic = 5)
cloud(airbnbfit,topic = 6)
cloud(airbnbfit,topic = 7)
cloud(airbnbfit,topic = 8)
cloud(airbnbfit,topic = 9)
cloud(airbnbfit,topic = 10)
cloud(airbnbfit,topic = 11)
cloud(airbnbfit,topic = 12)
cloud(airbnbfit,topic = 13)
cloud(airbnbfit,topic = 14)
cloud(airbnbfit,topic = 15)
cloud(airbnbfit,topic = 16)
cloud(airbnbfit,topic = 17)
cloud(airbnbfit,topic = 18)
cloud(airbnbfit,topic = 19)
cloud(airbnbfit,topic = 20)
cloud(airbnbfit,topic = 21)

topic_labels_amsterdam <- c("host", "location", "room", "amenity", "service", "travel", "bike tour", 
                  "apartment", "communication", "clean", "home-like", "alex", "bed", "even", 
                  "extra service", "transportation", "neighbourhood", "household item", 
                  "value of money", "nearby cafe", "eating")

#Prepare a table to put the topic proportions in
topic_summary <- summary(airbnbfit)
topic_proportions <- colMeans(airbnbfit$theta)

table_towrite_labels <- data.frame()
for(i in 1:length(topic_summary$topicnums)){
  
  row_here <- tibble(topicnum= topic_summary$topicnums[i],
                     topic_label = topic_labels[i],
                     proportion = 100*round(topic_proportions[i],4),
                     frex_words = paste(topic_summary$frex[i,1:7],
                                        collapse = ", "))
  table_towrite_labels <- rbind(row_here,table_towrite_labels)
}
table_towrite_labels %>% arrange(desc(proportion))



plot(airbnbfit,custom.labels = topic_labels,main = "")

#estimate
out$meta$cabin_class <- as.factor(out$meta$cabin_class)
convergence <- as.data.frame(airbnbfit$theta)
colnames(convergence) <- paste0("topic",1:20)

effects <- estimateEffect(~review_scores_rating + price,
                          stmobj = airbnbfit,
                          metadata = out$meta )

plot(effects, covariate = "review_scores_rating",
     topics = c(1:21),
     model = airbnbfit, method = "difference",
     cov.value1 = "5", cov.value2 = "1",
     xlab = "Low Rating ... High Rating",
     xlim = c(-0.04,0.04),
     main = "",
     custom.labels = topic_labels,
     labeltype = "custom")

