library("e1071")

traindata <- read.csv("Train500.csv",TRUE,",")
testdata<-read.csv("Test500.csv",TRUE,",")
traindata$X <- NULL
testdata$X <- NULL
#Since all applicants are employed, The Occupation field doesn't help us in classifying
#traindata$Occupation <- NULL
#testdata$Occupation <- NULL

#Build a SVM classifier
svm_model <- svm(Creditability ~., data = traindata, type = 'C-classification')
print(summary(svm_model))
svm_predict <- predict(svm_model, testdata[,-1])
print(table(pred = svm_predict, testdata[,1]))

#Tune svm to find best cost and gamma
x <- subset(traindata, select=-Creditability)
y <- traindata$Creditability
svm_tune <- tune(svm, train.x = x, train.y = y, kernel = 'radial', ranges = list(cost=10^(-1:2), gamma = c(0.1,0.2)))
#print(warnings())
print(svm_tune)

#Build a SVM model with cost = 1 and gamma = 0.1(from summary(svm_tune))
svm_model_tuned <- svm(Creditability ~., data = traindata, type = 'C-classification', cost = 1, gamma = 0.1)
print(summary(svm_model_tuned))
svm_tune_predict <- predict(svm_model_tuned, testdata[,-1])
print(table(pred = svm_tune_predict, testdata[,1]))


#Observation: Compare the confusion matrix built using svm_model and svm_model_tuned
#pred   0   1
#   0  43  30
#   1 114 313

#pred   0   1
#   0  45  26
#   1 112 317


