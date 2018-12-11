library(e1071)

##### Modeling with SVM #####
songs.svm <- svm(genre~., data=train, kernel="linear", cost=0.1 , scale=FALSE) # Training SVM on the Train data

response.svm <- predict(songs.svm, as.matrix(test[,2:length(test)]),type = "class") # Predicting using SVM on Test data

##### SVM Accuracy #####
sum(songs.svm$fitted == train[,1]) / nrow(train) # 51.3%
sum(response.svm == test[,1]) / nrow(test) # 50.5%


##### Modeling with SVM with Lasso Features #####
songs.svm.las <- svm(genre~., data=train[,c(which(cf$Tot >= 11.947))], kernel="linear", cost=0.1 , scale=FALSE) # Training SVM on the Train data, with Lasso Features

response.svm.las <- predict(songs.svm, as.matrix(test[,c(which(cf$Tot >= 11.947))]),type = "class") # Predicting using SVM on Test data (Lasso Features)

sum(songs.svm.las$fitted == train[,1]) / nrow(train) # 52.1%
sum(response.svm.las == test[,1]) / nrow(test) # 50.6%
