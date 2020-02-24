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


```

```{r}

workingfile <- getwd()
envoirment <- paste0(workingfile,"/","textdata","/")
filelist <- list.files(envoirment)


```

```{r}

calendarfin <- data.frame()
review <- data.frame()
listing <- data.frame()

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

for (i in 1:length(filelist)){
  if (str_detect(filelist[i],"calendar")){
    calendarnew <- readcalendar(filelist[i])
    calendarfin <- rbind(calendarfin,calendarnew)
  } 
  else if(str_detect(filelist[i],"listings")){
    datalist <- gzfile(paste0(envoirment,filelist[i]),"rt")
    onelisting <- read.csv(datalist)
    listing <- rbind(listing,onelisting)
  }
  else if(str_detect(filelist[i],"reviews")){
    datalist <- gzfile(paste0(envoirment,filelist[i]),"rt")
    onereview <- read.csv(datalist)
    review <- rbind(review,onereview)
  }
}

```

