---
title: "download"
author: "William"
date: "2020/2/17"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

rm(list=ls())
library(rvest)
library(httr)
library(stringr)
library(XML)
library(dplyr)


```

```{r}

url <- "http://insideairbnb.com/get-the-data.html"
url_page <- read_html(url)


```

```{r}

cities <- url_page %>%
  html_node("div.contentContainer") %>%
  html_nodes("table")

```

```{r}

citynames <- cities %>%
  html_attr("class")

```

```{r}

for (i in 1:length(citynames)){
  cityname <- strsplit(citynames[i],split=' ')[[1]]
  citynames[i] <- cityname[length(cityname)]
}

```

```{r}

totaldownload <- c()

for (i in 1:length(cities)){
  download <- cities[i] %>%
  html_nodes("a") %>%
  html_attr("href")
  download <- download
  totaldownload <- c(totaldownload,download)
}



```


```{r}

dir.create("textdatademo")
#Get file path
workingfile <- getwd()
envoirment <- paste0(workingfile,"/","textdatademo","/")

```



```{r}

for (i in 1:length(totaldownload)) {
  if (any(str_detect(totaldownload[i],c("listings.csv.gz","calendar.csv.gz","reviews.csv.gz")))){
    tryCatch({
    urlsplit <- strsplit(totaldownload[i],split='/')[[1]]
    filename <- paste(urlsplit[6],urlsplit[7],urlsplit[9],sep = "_")
    finalpath <- paste0(envoirment,filename)
    download.file(totaldownload[i],finalpath)
    },error = function(e) { 
      print(e)
      }
    )
  }
}



```


















