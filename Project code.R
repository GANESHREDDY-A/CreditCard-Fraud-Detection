
## Read CSV file
cc <- read.csv("creditcard.csv")
## Examine the structure of the data set
str(cc)
##Descriptive stats
summary(cc)
## Create the data.table
credit.dt <- setDT(cc)
credit.dt
#Descriptive stats by Class
temp1 <- credit.dt[, .(min.amount=min(Amount), max.amount=max(Amount), mean.amount=mean(Amount), med.amount=median(Amount), sd.amount=sd(Amount)), by=Class]
temp1
##Check for missing values
colSums(is.na(cc))


#Remove 'Time' variable
cc.data <- credit.dt[,-1]
#Change 'Class' variable to factor
cc.data$Class <- as.factor(cc.data$Class)
levels(cc.data$Class) <- c("Not_Fraud", "Fraud")
head(cc.data)

set.seed(123) 
#Train 70% of the dataset
train.index <- sample(1:nrow(cc.data), 
                      round(dim(cc.data) [1]*0.7))  
#Collect all the columns with training row ID into training set
train.data <- cc.data[train.index, ]
#Remaining 30% of dataset for validation
test.data <- cc.data[-train.index, ]
head(train.data)
head(test.data)


tab <- table(train.data$Class)
tab

# R Downsampling
set.seed(12345)
down_train <- downSample(x = train.data[, -30],
                         y=train.data$Class)
table(down_train$Class)
set.seed(5627)
# Build down-sampled model
down_fit <- rpart(Class ~ ., data = down_train)
# AUC on down-sampled data
pred_down <- predict(down_fit, newdata = test.data)
print('Fitting model to downsampled data')
roc.curve(test.data$Class, pred_down[,2], plotit = TRUE)


set.seed(12345)
up_train <- upSample(x = train.data[, -30],
                     y = train.data$Class)
table(up_train$Class)
set.seed(5627)
# Build up-sampled model
up_fit <- rpart(Class ~ ., data = up_train)
# AUC on up-sampled data
pred_up <- predict(up_fit, newdata = test.data)
print('Fitting model to upsampled data')
roc.curve(test.data$Class, pred_up[,2], plotit = TRUE)


# Upsample Test Data
set.seed(12345)
up_test <- upSample(x = test.data[, -30], y = test.data$Class)
table(up_test$Class)
logit.reg <- glm(Class ~ ., data = up_train, family = "binomial")
options(scipen = 999) 
summary(logit.reg)
#Generate odds ratio
exp(coef(logit.reg))
#performance evaluation
logit.reg.pred <- predict(logit.reg, up_test, type = "response")
t(t(head(logit.reg.pred, 10)))
#confusion matrix
table(up_test$Class, logit.reg.pred > 0.5)
summary(logit.reg.pred)
acc2<-table(logit.reg.pred > 0.5, up_test$Class)
print("Confusion Matrix for Test Data")
acc2
#Precision score 
precision <- acc2[2,2]/(acc2[2,2]+acc2[2,1])
precision
#Recall
recall <- acc2[2,2]/(acc2[2,2]+acc2[1,2])
recall
#Specificity
specificity <- acc2[1,1]/(acc2[1,1]+acc2[2,1])
specificity
#F Score
fscore <- (precision*recall)/(precision+recall)
fscore
print('ROC')
roc.curve(up_test$Class, logit.reg.pred, plotit = TRUE)

# Decision Trees
library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . , creditcard_data, method = 'class')
predicted_val <- predict(decisionTree_model, creditcard_data, type = 'class')
probability <- predict(decisionTree_model, creditcard_data, type = 'prob')
rpart.plot(decisionTree_model)

# Random Forest
set.seed(1234)
memory.limit(size = 15000)
bag.credit <- randomForest(as.factor(Class) ~ ., data = up_train, ntree = 300, mtry = 6, importance = TRUE)
bag.credit
#variable importance
importance <- data.frame(bag.credit$importance)
#plot the variable importance 
Imp1 <- ggplot(importance, aes(x=reorder(rownames(importance),MeanDecreaseGini), y=MeanDecreaseGini)) +
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (MeanDecreaseGini)", x="Variable", y="Variable importance (MeanDecreaseGini)")
Imp2 <- ggplot(importance, aes(x=reorder(rownames(importance),MeanDecreaseAccuracy), y=MeanDecreaseAccuracy)) +
  geom_bar(stat="identity", fill="lightblue", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (MeanDecreaseAccuracy)", x="Variable", y="Variable importance (MeanDecreaseAccuracy)")
gt <- arrangeGrob(Imp1, Imp2, ncol=2)
as_ggplot(gt)
#prediction
rf.pred <- as.factor(predict(bag.credit, newdata = up_test))
#confusion matrix
conf <- confusionMatrix(rf.pred, up_test$Class, positive = "1")
conf
#area under the curve(AUC)
roc.curve(up_test$Class, rf.pred, plotit = TRUE)

