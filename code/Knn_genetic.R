library(caret)
library(doParallel) # parallel processing
library(dplyr) # Used by caret
library('class')
library('gmodels')

#library(pROC) # plot the ROC curve

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

registerDoParallel(4) # Registrer a parallel backend for train
getDoParWorkers() # check that there are 4 workers
 
ga_ctrl <- gafsControl(functions = rfGA, # Assess fitness with RF
                       method = "cv",    # 10 fold cross validation
                       genParallel=TRUE, # Use parallel programming
                       allowParallel = TRUE)
                     
rf_ga3 <- gafs(x = trainX, y = y,
                           iters = 100, # 100 generations of algorithm
                           popSize = 20, # population size for each generation
                           gafsControl = ga_ctrl)
                          
final <- rf_ga3$ga$final # Get features selected by GA
trainX2 <- trainX[,final] # training data: selected features
testX2 <- testX[,final] # test data: selected features

print(final)
#final <- c("Account.Balance","Payment.Status.of.Previous.Credit","Length.of.current.employment","Occupation" ,"Telephone")
#trainX2 <- trainX[,final]
#testX2 <- testX[,final]

#function to normalize
normalize <- function(x){
		return ((x - min(x))/(max(x) - min(x)))
	}

#normalize the training and test data set.
traindata_norm <- as.data.frame(lapply(trainX2,normalize))
testdata_norm <- as.data.frame(lapply(testX2,normalize))

#Predict using KNN
predict <- knn(train = traindata_norm, test = testdata_norm, cl = traindata[,1], k = 21)

#label testdata[,1](Creditability) as actual
actual <- testdata[,1]
#print(table(actual = testdata[,1], predict = pred))
CrossTable(actual, predict,prop.chisq = FALSE)

