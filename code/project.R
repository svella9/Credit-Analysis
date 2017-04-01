traindata <- read.csv("Train500.csv",TRUE,",")
testdata<-read.csv("Test500.csv",TRUE,",")
traindata$X <- NULL
testdata$X <- NULL

#Building a model considering all features/attributes
model <- glm(Creditability~.,family=binomial(link = "logit"),data=traindata)
sum <- summary(model)
model_predict <- predict(model, type = 'response')

#From the summary(model) we can infer that there are lot of insignificant attributes.
#print(sum)
#Construct a confusion matrix. (Note: This matrix will be used to compare the model after removing the insignificant attributes.)
#print(table(testdata$Creditability, model_predict > 0.5))

#Function to determine only significant attributes.
significant_pvalues <- function(pvals){
	p <- list()
	for(i in c(1:length(pvals))){
		if(pvals[i] <= 0.0507)
			p[length(p)+1] <- names(pvals[i])
	}
	return(p)
}

#list containing only significant attributes returned by the significant_pvalues()
pred <- significant_pvalues(sum$coeff[2:nrow(sum$coeff),4])
#print(pred)
#Now build a model with only significant variables.
model <- glm(Creditability ~ Account.Balance+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status, family = binomial(link = "logit"), data = traindata)
#print(summary(model))
model_predict <- predict(model, type = 'response')
#Compare the confusion matrix constructed using the new model with the one constructed previously(A very slight improvement in the model).
print(table(actual = testdata$Creditability, predict = factor(model_predict > 0.5, levels = c(FALSE, TRUE), labels = c("Bad", "Good"))))
#print(table(actual = testdata$Creditability, predict = factor(model_predict > 0.9, levels = c(FALSE, TRUE), labels = c("Bad", "Good"))))
#print(table(actual = testdata$Creditability, predict = factor(model_predict > 0.4, levels = c(FALSE,TRUE), labels = c("Bad", "Good"))))

