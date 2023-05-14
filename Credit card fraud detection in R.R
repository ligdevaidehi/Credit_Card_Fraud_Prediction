if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, tidyverse, ggplot2, ggcorrplot, pROC, ROSE,
               corrplot, dplyr, caret, MASS, caTools, smotefamily, DMwR, rpart, rpart.plot, randomForest, gridExtra, ggpubr, cvms, rattle)

theme_set(theme_classic())
## Read CSV file and create DataTable 

## Read CSV file
cc <- read.csv("creditcard.csv")
cc
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

##Checking Class imbalance
table(credit.dt$Class)

#Percentage of Class imbalance
100*prop.table(table(credit.dt$Class))

common_theme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(data = credit.dt, aes(x = factor(Class), y = prop.table(stat(count)), 
                             fill = factor(Class),
                             label = scales::percent(prop.table(stat(count))))) +
  scale_fill_brewer(palette = "Set2") +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count',
            position = position_dodge(.9),
            vjust = -0.5,
            size = 3) +
  scale_x_discrete(labels = c("no fraud", "fraud")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = 'Class', y = 'Percentage') + 
  ggtitle ("Distribution of Class Variable") +
  common_theme

#Correlations
correlations <- cor(credit.dt[,], method="pearson")
round(correlations, 2)
##title <- "Correlation of Fraud Dataset Variables"
corrplot(correlations, number.cex = .9, type = "upper",
         method = "color", tl.cex=0.8,tl.col = "black")

credit.dt %>%
  ggplot(aes(x = Time, fill = factor(Class))) + geom_histogram(bins = 100)+
  labs(x = 'Time in Seconds Since First Transaction', y = 'No. of Transactions') +
  ggtitle('Distribution of Time of Transaction by Class') +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(Class ~ ., scales = 'free_y') + common_theme


ggplot(credit.dt, aes(x = factor(Class), y = Amount)) + geom_boxplot() + 
  labs(x = 'Class (Non-Fraud vs Fraud)', y = 'Amount (Euros)') +
  ggtitle("Distribution of Transaction Amount by Class") + common_theme

credit.dt %>%
  ggplot(aes(x = Amount)) + geom_histogram(col="black", fill = "darkseagreen3")+
  labs(x = 'Amount < 300 Euros', y = 'Frequency') +
  ggtitle('Distribution of Amount < 300 Euros') + xlim(c(0,300))+ ylim(c(0,30000)) +
  common_theme

##caret::featurePlot(x=credit.dt[,2:29], y=credit.dt[,31])

#Remove 'Time' variable
cc.data <- credit.dt[,-1]


#Change 'Class' variable to factor
cc.data$Class <- as.factor(cc.data$Class)
levels(cc.data$Class) <- c("Not_Fraud", "Fraud")

head(cc.data)

#Set seed to make partition reproducible
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
###################################################
# Initial Class Ratio for Training Data

tab <- table(train.data$Class)
tab

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
#.....................
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

# ROSE
set.seed(12345)
rose_train <- ROSE(Class ~ ., data  = train.data)$data 

table(rose_train$Class)

# ROSE model
set.seed(5627)
rose_fit <- rpart(Class ~ ., data = rose_train)

#AUC for ROSE model
pred_rose <- predict(rose_fit, newdata = test.data)


print('Fitting model to ROSE data')
roc.curve(test.data$Class, pred_rose[,2], plotit = TRUE)
