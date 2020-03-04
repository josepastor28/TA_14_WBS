---
title: "Read"
author: "William"
date: "2020/2/22"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Sys.setlocale("LC_ALL", "English")
rm(list=ls())
library(rvest)
library(httr)
library(stringr)
library(XML)
library(dplyr)
library(RSQLite)

```

```{r}

workingfile <- getwd()
envoirment <- paste0(workingfile,"/","textdatademo","/")
filelist <- list.files(envoirment)


```



```{r}

readcalendar <- function(filename){
  data <- gzfile(paste0(envoirment,filename),"rt")
  calendar <- read.csv(data)
  
  calendar$adjusted_price <- as.numeric(calendar$adjusted_price)
  calendar$listing_id <- as.factor(calendar$listing_id)
  
  calendarnew <- calendar %>%
    group_by(listing_id) %>%
    dplyr::summarize(avg_price = mean(adjusted_price))
  return(calendarnew)
}
    
```

```{r}

readlist <- function(filename){
  datalist <- gzfile(paste0(envoirment,filename),"rt")
  onelisting <- read.csv(datalist)
  dbWriteTable(con,"listings",onelisting,append = TRUE)
  dbExecute(con,"DELETE from listings where rowid not in(SELECT min(rowid) from listings                  group by id )")
  }
    
```


```{r}

sqlite <- dbDriver("SQLite")
con <- dbConnect(sqlite,"airbnb.db")

```


```{r old}

for (i in 1:6){
  if (str_detect(filelist[i],"calendar")){
    calendarnew <- readcalendar(filelist[i])
    dbWriteTable(con,"calendars",calendarnew,append = TRUE)
  } 
  else if(str_detect(filelist[i],"listings")){
    datalist <- gzfile(paste0(envoirment,filelist[i]),"rt")
    onelisting <- read.csv(datalist)
    dbWriteTable(con,"listings",onelisting,append = TRUE)

  }
  else if(str_detect(filelist[i],"reviews")){
    datalist <- gzfile(paste0(envoirment,filelist[i]),"rt")
    onereview <- read.csv(datalist)
    dbWriteTable(con,"reviews",onereview,append = TRUE)
  }
}

```


```{r new}

for (i in length(filelist):1){
  if(str_detect(filelist[i],"listings")){
    tryCatch({
      readlist(filelist[i])
      print(paste0(filelist[i]," done!"))
    },error = function(e) { 
      print(paste0("there is something wrong with ",filelist[i]))
      print(e)
      }
      
    )
}
  else if(str_detect(filelist[i],"reviews")){
    datalist <- gzfile(paste0(envoirment,filelist[i]),"rt")
    onereview <- read.csv(datalist)
    dbWriteTable(con,"reviews",onereview,append = TRUE)
    dbExecute(con,"DELETE from reviews where rowid not in (SELECT min(ROWID) from reviews                        group by id||comments)")
    print(paste0(filelist[i]," done!"))
  }
}

```


```{r}

dbDisconnect(con)

```







