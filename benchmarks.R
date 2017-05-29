# Form Benchmarks ---------------------------------------------------------

library(tidyverse)
library(lubridate)
library(stringr)
library(RGoogleAnalytics)

getGAData()
DOsData <- read.csv("q1DoubleOptinsBySource.csv")

# remove salesforce report identifier

DOsData <- DOsData[1:(sum(!is.na(DOsData$Subscriber))-5),]
summary(DOsData)

# remove languages

langs <- list('en-US/','en-GB/','de/','fr/','cs/','tr/','zn-CN/','es-ES/','es-MX/','pt-BR/','ru/','pl/','it/','hu/','pt-PT/','zh-TW/','sk/','id/','vi/','sl/','sv-SE/','uk/','sr/','sq/','es-AR/','nl/','ja/','ro/','fi/','ar/','th/','ko/','bg/','fa/','es-CL/','ca/')

gaCompleteData <- gaCompleteData %>%
  rowwise() %>%
  mutate(path = removeLanguage(path)) %>%
  group_by(path,metric) %>%
  summarize(value=sum(value)) %>%
  spread(metric,value) %>%
  arrange(desc(`ga:goal19Completions`))

DOsData <- DOsData %>%
  filter(as.numeric(Subscriber)==1) %>%
  mutate(path = gsub("http://www.mozilla.org","",Signup.Source.URL)) %>%
  mutate(path = gsub("https://www.mozilla.org","",path)) %>%
  rowwise() %>%
  mutate(path = removeLanguage(path),Subscriber = as.integer(Subscriber)) %>%
  select(-Signup.Source.URL) %>%
  group_by(path) %>%
  summarize(subscribers = sum(Subscriber),dos = sum(Double.Opt.In))

# combine GA and SFDC datasets

fullBenchmarkData <- left_join(gaCompleteData,DOsDataa,by="path")

fullBenchmarkData <- fullBenchmarkData %>%
  mutate(style = ifelse(str_detect(path,"newsletter"),"Main","Footer"),
         newsletter = ifelse(str_detect(path,"firefox"),"Firefox",
                             ifelse(str_detect(path,"developer"),"Developer","Mozilla")))

# get final result

fullBenchmarkData %>%
  group_by(newsletter,style) %>%
  summarize(pageviews = sum(`ga:pageviews`,na.rm=TRUE),conversions = sum(`ga:goal19Completions`,na.rm=TRUE),optins = sum(dos,na.rm=TRUE), cr = conversions/pageviews*100, or = optins/conversions*100)


# helper functions

removeLanguage <- function(string) {
  i <- 1
  trimmed_string <- string
  while (i <= length(langs)) {
    trimmed_string <- gsub(as.character(langs[i]),"",trimmed_string)
    i <- i+1
  }
  #trimmed_string <- removeQuery(trimmed_string)
  return(trimmed_string)
}

removeQuery <- function(string) {
  arr <- unlist(str_split(string,"\\?"))
  trimmed_string <- arr[[1]]
  return(trimmed_string)
}

getGAData <- function () {
  
  load("../TEI Pipeline/gToken")
  
  ValidateToken(gToken)
  
  gaCompleteData <<- data.frame()
  
  gaQueries <- list(
    list(
      id = "Mozilla Pageviews",
      gaId = "xxxxxxxxx",
      queryType = "ga:pageviews",
      dimension = "ga:pagePath"
    ),
    list(
      id="Mozilla Conversions",
      gaId = "xxxxxxxxx",
      queryType = "ga:goal19Completions",
      dimension = "ga:goalCompletionLocation"
    )
  )
  
  gatherGAData <- function (id,startDate,endDate,gaId,queryType,dimension) {
    query <- Init(
      start.date = as.character(startDate),
      end.date = as.character(endDate),
      dimensions = dimension,
      metrics = queryType,
      sort = str_c("-",queryType,sep=""),
      max.results = 20000,
      table.id = gaId )
    gaQuery <- QueryBuilder(query)
    gaData <- GetReportData(gaQuery,gToken)
    colnames(gaData)[1] <- "path"
    colnames(gaData)[2] <- "value"
    gaData <- gaData %>%
      mutate(metric = queryType)
    gaCompleteData <<- rbind(gaCompleteData,gaData)
  }
  
  lapply(gaQueries, function (i) gatherGAData(i$id,startDate,endDate,i$gaId,i$queryType,i$dimension))
}