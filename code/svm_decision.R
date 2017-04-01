library("e1071")
library('rpart')

traindata <- read.csv("Train500.csv",TRUE,",")
testdata<-read.csv("Test500.csv",TRUE,",")
traindata$X <- NULL
testdata$X <- NULL
traindata$Creditability <- factor(traindata$Creditability, levels = c(0,1), labels = c("Bad","Good"))
testdata$Creditability <- factor(testdata$Creditability, levels = c(0,1), labels = c("Bad","Good"))

tree_model <- rpart(Creditability~., data = traindata, method = 'class')
ptree_model <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])
pred_var <- as.data.frame(ptree_model$variable.importance)
indexes <- which(as.vector(as.matrix(pred_var)) > 4)
pred_var <- row.names(pred_var)[indexes]
print(pred_var)

f <- as.formula(paste("Creditability~", paste(pred_var, collapse = '+')))
#print(f)
#Build a SVM classifier
svm_model <- svm(f, data = traindata, type = 'C-classification')
summary(svm_model)
svm_predict <- predict(svm_model, testdata[,-1])
print(table(pred = svm_predict, testdata[,1]))

#Tune svm to find best cost and gamma
x <- subset(traindata, select = pred_var)
y <- traindata$Creditability
svm_tune <- tune(svm, train.x = x, train.y = y, kernel = 'radial', ranges = list(cost=10^(-1:2), gamma = c(0.1,0.2)))
print(svm_tune)

#Build a SVM model with cost = 1 and gamma = 0.2(from summary(svm_tune))
svm_model_tuned <- svm(Creditability ~., data = traindata, type = 'C-classification', cost = 1, gamma = 0.2)
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



#pred   Bad Good
#  Bad   51   28
#  Good 101  320


      
#pred   Bad Good
#  Bad   15    3
#  Good 137  345

