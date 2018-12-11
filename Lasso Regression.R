library(glmnet)

# Create matrices for cv.glmnet input
x<-model.matrix(genre~.,data=train)
y=train[,c("genre")]
x=x[,-1]

# Run the model with 5 folds, using multinomial family for classification
glmnet1<-cv.glmnet(x=x,y=y,type.measure='mse',nfolds=5,alpha=.5,family = 'multinomial')

# Create a dataframe of the coefficients for each genre
c<-coef(glmnet1,s='lambda.min',exact=TRUE)
cf <- data.frame('Rock' = c$Rock[,1],'Country' = c$Country[,1],'Electronic' = c$Electronic[,1], 'Folk' = c$Folk[,1],'Hip-Hop' = c$`Hip-Hop`[,1],'Indie'=c$Indie[,1],'Jazz' = c$Jazz[,1],'Metal'=c$Metal[,1],'Pop'=c$Pop[,1],'R&B'=c$`R&B`[,1])
rownames(cf) <- rownames(c$Rock)

# Calculate coefficient Standard Deviation and Total (Sum) Coefficient Magnitude
cf$Sd <- apply(cf[,c(1:10)], 1, sd)
cf$Tot <- abs(cf$Rock) + abs(cf$Country) + abs(cf$Electronic) + abs(cf$Folk) + abs(cf$Hip.Hop) + abs(cf$Indie) + abs(cf$Jazz) + abs(cf$Metal) + abs(cf$Pop) + abs(cf$R.B)

# Find the top Total Coefficient Magnitude rows and display some of them
order(cf$Tot)
as.data.frame(cf[c(325, 371, 191, 176,329, 501,28,467,97,108,24, 283,54, 549, 5,75, 240, 564, 22, 510, 332, 534, 82, 402, 278,  43, 516, 184,  40, 177, 418, 563),])

# Summary Statistics for Total Magnitude and Standard Deviation
summary(cf[,c(11:12)])

# Calculate the Fitted and the Predicted values for the Train and Test
response.glm <- predict(glmnet1, as.matrix(test[,2:length(test)]),s = "lambda.min",type = "class")
train.response <- predict(glmnet1, as.matrix(train[,2:length(train)]),s = "lambda.min",type = "class")

# Test Lasso Accuracy
sum(test[,1] == response.glm) / nrow(test) # 54.9% Accurate
sum(train[,1] == train.response) / nrow(train) # 55.3% Accurate

which(cf$Tot >= 11.947) # A vector of column indeces for a subset model based upon Total Magnitude of Lasso Coefficients

