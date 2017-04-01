library('rpart')

traindata <- read.csv("Train500.csv",TRUE,",")
testdata<-read.csv("Test500.csv",TRUE,",")
traindata$X <- NULL
testdata$X <- NULL
#Since all applicants are employed, The Occupation field doesn't help us in classifying
#traindata$Occupation <- NULL
#testdata$Occupation <- NULL
traindata$Creditability <- factor(traindata$Creditability, levels = c(0,1), labels = c("Bad","Good"))
testdata$Creditability <- factor(testdata$Creditability, levels = c(0,1), labels = c("Bad","Good"))


tree_model <- rpart(Creditability~., data = traindata, method = 'class')
#print("Before Prune")
#summary(tree_model)
tree_predict <- predict(tree_model, testdata[,-1])
#print(tree_predict[,2])
#table(testdata$Creditability, tree_predict[,2]>0.5)
#table(testdata$Creditability, predict = factor(tree_predict[,2]>0.75, levels = c(FALSE,TRUE), labels = c("Bad", "Good")))
#table(testdata$Creditability, tree_predict[,2]>0.75)

ptree_model <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])
print("After Prune")
#summary(ptree_model)
#print(names(ptree_model))
pred_var <- as.data.frame(ptree_model$variable.importance)
indexes <- which(as.vector(as.matrix(pred_var)) > 4)
#print(indexes)
pred_var <- row.names(pred_var)[indexes]
print(pred_var)
ptree_predict <- predict(ptree_model, testdata[,-1])
#print(tree_predict[,2])
#table(actual = testdata$Creditability, predict = factor(ptree_predict[,2]>0.75, levels = c(FALSE,TRUE), labels = c("Bad", "Good")))

#table(testdata$Creditability, ptree_predict[,2]>0.4)
