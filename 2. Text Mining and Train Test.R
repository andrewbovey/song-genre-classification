# setwd("~/Desktop/SYS6018")

library(dplyr) #data manipulation
library(tidytext) #text mining
library(tidyr) 
library(lubridate)
library(tidyverse)
library(tm)
library(quanteda)


##### Create VCorpus from Lyrics #####
song.corp.setup <- songs[,c(1,6)]
names(song.corp.setup) <- c("doc_id","text")
songs.corp <- VCorpus(DataframeSource(song.corp.setup))

songs.corp.clean <- tm_map(songs.corp, stripWhitespace)                          # remove extra whitespace
songs.corp.clean <- tm_map(songs.corp.clean, removeNumbers)                      # remove numbers
songs.corp.clean <- tm_map(songs.corp.clean, removePunctuation)                  # remove punctuation
songs.corp.clean <- tm_map(songs.corp.clean, content_transformer(tolower))       # ignore case
songs.corp.clean <- tm_map(songs.corp.clean, removeWords, stopwords("english"))  # remove stop words
songs.corp.clean <- tm_map(songs.corp.clean, stemDocument)                       # stem all words

inspect(songs.corp.clean[[10005]])
#time boy rarin go tonight gonna hell show think youv got someth prove just wait see make first 
#move boot fit occas tonight tank everyth go just right now feelin alright crew feelin alright 
#crew feelin alright crew feelin alright crew ive got blood hair want smash face what point blood 
#confus dont blink stupid battl curs aggriv metal outburst aint ever gonna settl next time im gonna 
#even score find theyll back

songs.clean.tfidf = DocumentTermMatrix(songs.corp.clean, control = list(weighting = weightTfIdf)) #TF-IDF document term matrix creation

as.matrix(songs.clean.tfidf[5000:5005,5000:5005])


##### Sparse Terms Removal Threshold #####
tfidf.98 = removeSparseTerms(songs.clean.tfidf, 0.98)  # remove terms that are absent from at least 98% of documents (keep most terms)
tfidf.95 = removeSparseTerms(songs.clean.tfidf, 0.95)  # remove terms that are absent from at least 95% of documents (keep most terms)

songs.tf <- cbind(songs,as.matrix(tfidf.98[,])) # Bind tfidf.98 to songs
songs.tf.95 <- cbind(songs,as.matrix(tfidf.95[,])) # Bind tfidf.95 to songs

`%ni%`<-Negate('%in%') # Opposite of %in% function
names(songs.tf)[names(songs.tf) %ni% names(songs.tf.95)] # Which words are not in the 95% that ARE in the 98%?

##### Create Modeling Data Frame #####
songs.tf.words <- songs.tf[,c(5,7:length(songs.tf))] # Remove irrelevant data elements

songs.tf.words$genre <- as.factor(songs.tf.words$genre) # Make the genre column a Factor

samp <- sample(x = seq(1:nrow(songs.tf.words)), size = 0.65*nrow(songs.tf.words) ) # Sample the data for 65/35 Train/Test split
train <- songs.tf.words[samp,] # Train data creation
test <- songs.tf.words[-samp,] # Test data creation
c(nrow(train), nrow(test))
# [1] 154327  83100

