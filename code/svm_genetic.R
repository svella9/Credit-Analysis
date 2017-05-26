library(caret)
library(doParallel) # parallel processing
library(dplyr) # Used by caret

#Read the proccessed train and test datasets
traindata <- read.csv("Train500.csv",TRUE,",")
testdata<-read.csv("Test500.csv",TRUE,",")

#Observation no. is not required.
traindata$X <- NULL
testdata$X <- NULL

#Lets relabel categories in Creditability column, 0 as 'bad' and 1 as 'good'.
traindata$Creditability <- factor(traindata$Creditability, levels = c(0,1), labels = c("Bad","Good"))
testdata$Creditability <- factor(testdata$Creditability, levels = c(0,1), labels = c("Bad","Good"))

# Create training feature data frame
trainX <- traindata[,-1]
# Create testing feature data frame
testX <- testdata[,-1]
#target variable
y <- testdata[,1]


                          
final <- c("Account.Balance","Purpose","Guarantors","No.of.dependents","Telephone")#rf_ga3$ga$final # Get features selected by GA
trainX2 <- trainX[,final] # training data: selected features
testX2 <- testX[,final] # test data: selected features

#print(final)
#final <- c("Account.Balance","Payment.Status.of.Previous.Credit","Length.of.current.employment","Occupation" ,"Telephone")
#trainX2 <- trainX[,final]
#testX2 <- testX[,final]

# Set up training control
ctrl <- trainControl(method="repeatedcv", # 10fold cross validation
	repeats=5, # do 5 repititions of cv
	summaryFunction=twoClassSummary, # Use AUC to pick the best model
	classProbs=TRUE)

registerDoParallel(4,cores=4)

#Train and Tune the SVM
svm.tune <- train(x=trainX2,
                  y= traindata$Creditability,
                  method = "svmRadial",
                  tuneLength = 9, # 9 values of the cost function
                  preProc = c("center","scale"),
                  metric="ROC",
                  trControl=ctrl) # same as for gbm above

#Make predictions on the test data with the SVM Model
svm.pred <- predict(svm.tune,testX2)
 
table(actual = y,predict = svm.pred)
