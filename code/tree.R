library('tree')

traindata <- read.csv("Training50.csv",TRUE,",")
testdata<-read.csv("Test50.csv",TRUE,",")
traindata$X <- NULL
testdata$X <- NULL
#Since all applicants are employed, The Occupation field doesn't help us in classifying
traindata$Occupation <- NULL
testdata$Occupation <- NULL

tree_model <- tree(Creditability ~ ., data = traindata)
summary(tree_model)

model_predict <- predict(tree_model, testdata[,-1])
#summary(predict)
print(table(actual = testdata$Creditability, predict = predict > 0.5))

prune.tree(tree_model)
