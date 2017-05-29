# Parsing Source URLs -----------------------------------------------------

library(tidyverse)
library(lubridate)
library(stringr)
library(urltools)

# import file

file <- read_csv("sourceURL/VojtechSourceURLs220170519.csv")

# parse files

parsedURLs <- file %>%
  mutate(parsedURL = parseURL(Source_URL)) %>%
  group_by(Category,parsedURL) %>%
  summarize(count = sum(!is.na(parsedURL))) %>%
  arrange(desc(count))
 
parseURL <- function(source_url) {
    url <- source_url
    url <- ifelse(url == "Interim_BSD_Middleware_Updater","interim.middleware.updater",url)
    url <- gsub("%3A",":",url)
    url <- gsub("%2F","/",url)
    url <- removeLanguage(url)
    parsed <- url_parse(url)
    new_source <- str_c(parsed$domain,'%',parsed$path)
    return(new_source)
}


# remove language helper function

removeLanguage <- function(string) {
  langs <- list('en-US/','en-GB/','de/','fr/','cs/','tr/','zn-CN/','es-ES/','es-MX/','pt-BR/','ru/','pl/','it/','hu/','pt-PT/','zh-TW/','sk/','id/','vi/','sl/','sv-SE/','uk/','sr/','sq/','es-AR/','nl/','ja/','ro/','fi/','ar/','th/','ko/','bg/','fa/','es-CL/','ca/')
  i <- 1
  trimmed_string <- string
  while (i <= length(langs)) {
    trimmed_string <- gsub(as.character(langs[i]),"",trimmed_string)
    i <- i+1
  }
  #trimmed_string <- removeQuery(trimmed_string)
  return(trimmed_string)
}