library('rpart')

traindata <- read.csv("Train500.csv",TRUE,",")
testdata<-read.csv("Test500.csv",TRUE,",")
traindata$X <- NULL
testdata$X <- NULL

#Building a model considering all features/attributes
model <- glm(Creditability~.,family=binomial(link = "logit"),data=traindata)
sum <- summary(model)
#model_predict <- predict(model, type = 'response')

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
print("Logistic")
print(pred)


tree_model <- rpart(Creditability~., data = traindata, method = 'class')
ptree_model <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])
print("After Prune")
pred_var <- as.data.frame(ptree_model$variable.importance)
indexes <- which(as.vector(as.matrix(pred_var)) > 4)
pred_var <- row.names(pred_var)[indexes]
print(pred_var)

print('Union')
print(union(pred_var, pred))
