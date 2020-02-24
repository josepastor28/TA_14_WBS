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
  download <- download[1:3]
  totaldownload <- c(totaldownload,download)
}



```


```{r}

dir.create("textdata")
#Get file path
workingfile <- getwd()
envoirment <- paste0(workingfile,"/","textdata","/")

```



```{r}

for (i in 1:length(totaldownload)) {
  urlsplit <- strsplit(totaldownload[i],,split='/')[[1]]
  filename <- paste(urlsplit[6],urlsplit[9],sep = "_")
  finalpath <- paste0(envoirment,filename)
  download.file(totaldownload[i],finalpath)
}


```
