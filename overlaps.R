# List Overlaps -------------------------------

library(tidyverse)
library(stringr)

# import salesforce report data

h1 <- read.csv('report1495043559021.csv')
h2 <- read.csv('report1495049673986.csv')

# remove salesforce identifiers at the bottom of the files (you have to change the row numbers)
h1slim <- h1[-c(5091920:5091924),]
h2slim <- h2[-c(5244157:5244161),]
full <- rbind(h1slim,h2slim)

# calculate total sums per row
for (i in 1:20) {
 full[,i] <- as.numeric(full[,i])
}
full <- full  %>%
  mutate(total = rowSums(.[1:20]))

# group subscription flags by category

full <- full %>%
  rowwise() %>%
  mutate(firefox = sum(Firefox.Accounts.Journey.Subscriber+Firefox...You.Subscriber+Test.Pilot.Subscriber+Test.Flight.Subscriber),other = sum(total - firefox - Mozilla.Foundation.Subscriber - Developer.News.Subscriber), mozilla=sum(Mozilla.Foundation.Subscriber), dev=sum(Developer.News.Subscriber)) %>%
  select(firefox,mozilla,other,dev,total)


# generate empty data frames

output <- data.frame()
overlaps <- data.frame()


# individual counts
for (i in 1:4) {
  name <- as.name(colnames(full)[i])
  condition <- paste(name, ">=", "1")
  value <- nrow(filter_(full,condition))
  venn_output <- rbind(output,c(as.character(name),as.integer(value)))
}

# overlaps
for (i in 1:4) {
  for (j in (i+1):3) {
    name1 <- as.name(colnames(full)[i])
    name2 <- as.name(colnames(full)[j])
    condition <- paste(as.name(colnames(full)[i]), ">=", "1","&",as.name(colnames(full)[j]), ">=","1")
    value <- nrow(filter_(full,condition))
    overlaps <- rbind(overlaps,c(as.character(name1),as.character(name2),as.integer(value)))  
  }
}