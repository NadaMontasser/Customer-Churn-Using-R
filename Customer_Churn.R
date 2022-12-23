
# Reading the churn data
setwd("D:/Data Science/Assigments/02")
data <-read.csv("Churn_Dataset.csv", header=TRUE)
head(data)
str(data)
summary(data)
glimpse(data)


######################################################################
####         question 1        ####

install.packages("ggcorrplot")
library(ggcorrplot)

num_colnms <-  c("tenure","MonthlyCharges","TotalCharges")
df_nums <- df[,num_colnms]
#1-plot the scatter plot between numeriacl attributes
pairs(df_nums,col=c("red", "green"),pch=18 ,main = " scatter plot of  Numerical Variables ")

#2-plot the correlation matrix for numerical attributes
df_num <- data.frame(scale(df_nums))
corm <- cor(df_num)
ggcorrplot(corm, title = "Correlation of Numeric Variables",colors = c("#6D9EC1", "white", "#E46726"))

hist(df$Churn)

#########################################################################
####         question 2        ####
install.packages("superml", dependencies=TRUE)
library(superml)

#Data Cleaning
#addressing the missing values
missing <-data[is.na(data$TotalCharges) |data$TotalCharges == " ",]

#delete nulls from the data & missing values
df <- data[!(is.na(data$TotalCharges) |data$TotalCharges == " "), ]
summary(df)
df

#addressing the duplicate values there no duplicated data
duplicate <-  df[duplicated(df$customerID),]
#convert categorical to numerical

#1- first drop column called Customer-ID 
df <- subset(df, select = -c(1))

#2-second separate numerical values away
nums <- c("SeniorCitizen","tenure","MonthlyCharges","TotalCharges") 
not_nums <- df[ , !(names(df) %in% nums), drop = FALSE]
not_nums

#3-apply label encoding
label <- LabelEncoder$new()
column = names(not_nums)
for (k in 1:length(column)){
  not_nums[,k] <- label$fit_transform(not_nums[,k])}
not_nums

#4-retrieve the all data together
install.packages("dplyr")
library(dplyr)
not_nums$SeniorCitizen =df[ ,"SeniorCitizen"]
not_nums$tenure =df[ ,"tenure"]
not_nums$MonthlyCharges =df[ ,"MonthlyCharges"]
not_nums$TotalCharges =df[ ,"TotalCharges"]
df <- not_nums %>% relocate(Churn, .after = TotalCharges)


###################################################################
####         question 3        ####
library(rpart)
library(rpart.plot)
library(caret)
library(rpart)
library(rpart.plot)
library(caret)
library(xgboost)
library(pROC)
#1- split the data to train and test
set.seed(818) 
data_splited <- sample(0:1, size= nrow(df), prob = c(0.80,0.20), replace = TRUE)
train <- df[data_splited == 0, ]
test <- df[data_splited == 1, ]

#2-split the target from the train data 
Xtrain <- train[,-20]
Xtarget <- train[,20]

#3- slpit the the target from the test data
Xtest <- test[,-20]
test_target <- test[,20]

#4-build decision tree model base model
fit <- rpart(formula = Churn ~., data = train ,method = "class")
pred1 <- predict(object=fit ,test,type="class")
acc1 <- table(test$Churn,pred1)
confusionMatrix(acc1)
summary(fit)

#5-plot the tree
rpart.plot(fit)

#ROC
roc(test$Churn,as.numeric(pred1))

#Plot Roc Graph
plot(roc(test$Churn,as.numeric(pred1)), col = 1, lty = 2, main = "ROC")

#Precision _Recall_ Fscore
measurePrecisionRecall(test$Churn, pred1)



#6-build decision tree model with Penalty Matrix
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
fit2 <- rpart(formula = Churn ~., data = train,method = "class",parms = list(loss = penalty.matrix))
pred2 <- predict(object=fit2 ,test,type="class")
acc2 <- table(test$Churn,pred2)
confusionMatrix(acc2)
summary(fit2)
#plot the tree
rpart.plot(fit2,main = "Penalty Matrix")

#Precision _Recall_ Fscore
measurePrecisionRecall(test_data$Churn,pred2)

# ROC
roc(test$Churn,as.numeric(pred2))
plot(roc(test$Churn,as.numeric(pred2)), col = 1, lty = 2, main = "ROC")



#7-build decision tree model with different strategy (gini_index) 
# Use gini as the split criterion
fit3 <- rpart(formula = Churn ~., data = train,method = "class",parms = list(split = "gini"))
pred3 <- predict(object=fit3 ,test,type="class")
acc3 <- table(test$Churn,pred3)
#Confusion matrix
confusionMatrix(acc3)
summary(fit3)
rpart.plot(fit3, main = "Gini Index")

#ROC
roc(test$Churn,as.numeric(pred3))

#Plot Roc Graph
plot(roc(test$Churn,as.numeric(pred3)), col = 1, lty = 2, main = "ROC")

#Precision _Recall_ Fscore
measurePrecisionRecall(test$Churn, pred3)


#8-build decision tree model with different strategy (information gain)
# Use information gain as the split criterion Let’s repeat the fitting,
fit4 <- rpart(formula = Churn ~., data = train,method = "class",parms = list(split = "information"))
pred4 <- predict(object=fit4 ,test,type="class")
acc4 <- table(test$Churn,pred4)
confusionMatrix(acc4)
summary(fit2)
rpart.plot(fit4,main = "information gain")


#ROC
roc(test$Churn,as.numeric(pred4))

#Plot Roc Graph
plot(roc(test$Churn,as.numeric(pred4)), col = 1, lty = 2, main = "ROC")

#Precision _Recall_ Fscore
measurePrecisionRecall(test$Churn, pred4)


#pruning
printcp(fit)
plotcp(fit)

model_prun <- prune(fit, cp = 0.036 )
pred_pr <- predict(object=model_prun ,test,type="class")

# Compute the accuracy of the pruned tree
acc_pr <- table(test$Churn,pred_pr)
confusionMatrix(acc_pr)
summary(model_prun)
rpart.plot(model_prun, main="Purned tree")


#Precision _Recall_ Fscore
measurePrecisionRecall(test_data$Churn,prediction2)

# ROC
roc(test$Churn,as.numeric(pred_pr))
plot(roc(test$Churn,as.numeric(pred_pr)), col = 1, lty = 2, main = "ROC")

#Precision _Recall_ Fscore
measurePrecisionRecall(test$Churn, pred_pr)

#############################################################################3

library(rpart)
library(rpart.plot)
library(caret)
library(xgboost)
library(pROC)


#XGboost model 
xgb <- xgboost(data = as.matrix(Xtrain),label = Xtarget,max_depth = 3,nrounds = 70)
xgbpred <- predict(xgb,  as.matrix(Xtest))
xgbpred[xgbpred>=.5] = 1
xgbpred[xgbpred<.5] = 0

#confusion matrix and accuracy of xg model
Con_GB = table(test$Churn, xgbpred)
confusionMatrix(Con_GB)
summary(xgb)


#ROC
roc(test$Churn,as.numeric(xgbpred))

#Plot Roc Graph
plot(roc(test$Churn,(xgbpred)), col = 1, lty = 2, main = "ROC")

#Precision _Recall_ Fscore
measurePrecisionRecall(test$Churn, xgbpred)



#################################
install.packages("devtools")

devtools::install_github("rstudio/keras")
1devtools::install_github("rstudio/reticulate")
install.packages("tensorflow")
install.packages("keras")
library(tensorflow)
library(reticulate)
library(keras)
reticulate::use_python("/home/sarah/anaconda3/bin/python3")
install_tensorflow()
1install_tensorflow(gpu=TRUE)
library(rpart)
library(rpart.plot)
library(caret)
library(xgboost)
library(pROC)



train_deep = as.matrix(train)
test_deep = as.matrix(test)
dimnames(train_deep) = NULL

keras_ml <- keras_model_sequential()

keras_ml %>%
  layer_dense(units = 19, activation = 'tanh', input_shape = 19) %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 10, activation = 'tanh') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = 'sigmoid')

summary(keras_ml)

keras_ml %>% compile(
  loss      = 'mean_squared_error',
  optimizer = 'adam',
  metrics = c('accuracy')
)

fit_1 = fit(keras_ml,data.matrix(Xtrain), train$Churn, epochs = 50, batch_size = 128)
plot(fit_1)



prediction = predict(keras_ml, data.matrix(Xtest))
prediction[prediction>=.5] = 1
prediction[prediction<.5] = 0



#confusion matrix and accuracy of Nural network
Con_Dnn = table(test$Churn, prediction)
confusionMatrix(Con_Dnn)
summary(prediction)

#ROC
roc(test$Churn,as.numeric(prediction))

#Plot Roc Graph
plot(roc(test$Churn,as.numeric(prediction)), col = 1, lty = 2, main = "ROC")

#Precision _Recall_ Fscore
measurePrecisionRecall(test$Churn,prediction)



#########################################################################################
#########################################################################################
#partB
install.packages("arules")
library(arules)
#read transactions dataset
trans <- read.transactions("D:/Data Science/Assigments/02/transactions.csv",
                           format = "basket", sep = ",",rm.duplicates = TRUE)
trans
#return top 10
itemFrequencyPlot(trans, topN = 10,col="#FF8686")

#first rule
transRules1 <- apriori(trans, parameter = list(support =0.002,
                                               confidence =0.20, maxlen= 3))
transRules1 <- sort(transRules_1, by = "lift",decreasing = TRUE )
inspect(transRules_1)
transRules1 <- sort(transRules_1, by = "lift",decreasing = TRUE )[0:1]
inspect(transRules1)


# second rule
transRules2 <- apriori(trans, parameter = list(support =0.002,
                                               confidence =0.20, maxlen= 2))
transRules2 <- sort(transRules2, by = "lift",decreasing = TRUE )
inspect(transRules2)
transRules2 <- sort(transRules2, by = "lift",decreasing = TRUE )[0:1]
inspect(transRules2)

