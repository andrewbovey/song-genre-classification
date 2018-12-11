setwd("~/Desktop/SYS6018")

library(dplyr) #data manipulation
library(stringr) # for str_split
library(tidyr) 
library(tidyverse)
library(ggplot2)

##### Data Import and Clean #####

songset <- read_csv("lyrics.csv")

head(songset)

songs <- songset[!is.na(songset$lyrics),] # Remove NA lyrics
songs <- songs[songs$genre != "Not Available",] # Remove Not Availablle
songs <- songs[songs$genre != "Other",] # Remove Other

rm(songset)

##### View the Genres ####

# View the genres in the data
ggplot(as.data.frame(table(songs$genre)), aes(x=Var1, y = Freq, fill=Var1)) # Graph Genre Prevalence
  + geom_bar(stat="identity") 
  + xlab("Genre") 
  + ylab("Songs") 
  + scale_fill_discrete(name = "Genre")

g <- c("Country", "Electronic", "Folk", "Hip-Hop", "Indie", "Jazz", "Metal", "Pop", "R&B", "Rock")
p <- c(0.068, 0.047, 0.003, 0.209, 0.031, 0.009, 0.095, 0.164, 0.095, 0.198) # Data from https://www.digitalmusicnews.com/2018/01/04/hip-hop-rock-2017-biggest-genre/

popularity <- data.frame(genre = g, listens = p)

# View the genres by actual listens
ggplot(data=popularity, aes(x=genre, y=listens, fill=genre)) + 
  geom_bar(stat="identity") + ylab("portion of actual listens")


##### Calculate the Total Words and Unique Words #####

list_words<-strsplit(as.character(songs$lyrics)," ")
unique_count<-unlist(lapply(lapply(list_words, unique),length))
word_count<-unlist(lapply(list_words,length))
word_count[1]
unique_count[1]

songs2 <- cbind(songs,word_count,unique_count) # New matrix with word count and unique word count

# View a histogram of word count
ggplot(songs2,aes(x=word_count, fill = genre)) +
  geom_histogram(data=subset(songs2,genre == 'Pop'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Rock'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Metal'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Country'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Hip-Hop'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Folk'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Indie'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'R&B'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Electronic'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Jazz'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  xlim(0, 1300) + xlab("Total Words") + ylab("Proportion of Songs")

# View a histogram of unique word count
ggplot(songs2,aes(x=unique_count, fill = genre)) + 
  geom_histogram(data=subset(songs2,genre == 'Pop'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Rock'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Metal'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Country'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Hip-Hop'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Folk'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Indie'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'R&B'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Electronic'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  geom_histogram(data=subset(songs2,genre == 'Jazz'), aes(y = stat(count / sum(count))),alpha = 0.35, bins = 100) +
  xlim(0, 800) + xlab("Unique Words") + ylab("Proportion of Songs")
