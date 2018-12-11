library(randomForest)

##### Creating a Random Forest Model with Train Data #####
# 500-tree randomForest model
songs.rf <- randomForest(x = train[,c(2:length(train))], y = train[,1], keep.forest=T, ntree = 500)
str(songs.rf)

preds <- predict(songs.rf, newdata = test) # Fitting the model to the Test data

sum(train[,1] == songs.rf$predicted) / nrow(train) # 60.6% accurate for Train
sum(test[,1] == preds)/nrow(test) # 60.9% accurate for Test (better!)

table(test[,1],preds) # A confusion matrix 

##### Using a Reduced Model (Fewer Features) #####
songs.rf.las <- randomForest(x = train[,c(which(cf$Tot >= 11.947))], y = train[,1], keep.forest=T)

preds.las <- predict(songs.rf.las, newdata = test) # Fitting the model to the Test data

sum(train[,1] == songs.rf.las$predicted) / nrow(train) # 
sum(test[,1] == preds.las)/nrow(test) # 


##### Creaing Train/Test with 3 Genres ####
train.red <- train[which(train$genre == "Rock" | train$genre == "Pop" | train$genre == "Hip-Hop"),]
table(train.red$genre)
train.red$genre <- droplevels(train.red$genre, exclude = if(anyNA(levels(train.red$genre))) NULL else NA)
levels(train.red$genre)

test.red <- test[which(test$genre == "Rock" | test$genre == "Pop" | test$genre == "Hip-Hop"),]
table(test.red$genre)
test.red$genre <- droplevels(test.red$genre, exclude = if(anyNA(levels(test.red$genre))) NULL else NA)
levels(test.red$genre)


##### Using a Rduced Model (Fewer Features, Fewer Classes) #####
songs.rf.las.red <- randomForest(x = train.red[,c(2:length(train.red))], y = train.red[,1], keep.forest=T)

preds.las.red <- predict(songs.rf.las.red, newdata = test.red) # Fitting the model to the Test data

sum(train.red[,1] == songs.rf.las$predicted) / nrow(train) # 
sum(test.red[,1] == preds.las.red)/nrow(test.red) # 
