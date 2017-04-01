library('class')
library('gmodels')

traindata <- read.csv("Training50.csv",TRUE,",")
testdata<-read.csv("Test50.csv",TRUE,",")
traindata$X <- NULL
testdata$X <- NULL
#Since all applicants are employed, The Occupation field doesn't help us in classifying
traindata$Occupation <- NULL
testdata$Occupation <- NULL
#Lets rename 0 as 'bad' and 1 as 'good'.
traindata$Creditability <- factor(traindata$Creditability, levels = c(0,1), labels = c("Bad","Good"))
testdata$Creditability <- factor(testdata$Creditability, levels = c(0,1), labels = c("Bad","Good"))

#function to normalize
normalize <- function(x){
		return ((x - min(x))/(max(x) - min(x)))
	}

#normalize the training and test data set.
traindata_norm <- as.data.frame(lapply(traindata[-1],normalize))
testdata_norm <- as.data.frame(lapply(testdata[-1],normalize))
#summary(traindata_norm)

#Predict using KNN
predict <- knn(train = traindata_norm, test = testdata_norm, cl = traindata[,1], k = 21)

#label testdata[,1](Creditability) as actual
actual <- testdata[,1]
#print(table(actual = testdata[,1], predict = pred))
CrossTable(actual, predict,prop.chisq = FALSE)

#How to interpret the CrossTable?
#TN -> 35 -> 7%
#TP -> 323 -> 64.6%
#FN -> 20 -> 4%
#FP -> 122 -> 24.4%
#Accuracy of model = (TN+TP)/500 = 71.6%
