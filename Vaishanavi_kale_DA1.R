#Installing all the packages
install.packages("psych")
install.packages("rpart")
install.packages("caret")
install.packages("rpart.plot")
install.packages("e1071") 
install.packages("DataExplorer")
install.packages("VIM")
install.packages("randomForest")
install.packages("fastAdaboost")

#Loading all libraries used
#For the data analysis and visualization
library(DataExplorer)
library(psych)
#For knn imputation
library(VIM)
#For data manipluation
library(dplyr)
#For plotting graphs
library(ggplot2)
#For decision tree
library(rpart)
#For plotting tree
library(rpart.plot)
#For reading excel file
library(readxl)
#For model validation(confusion matrix,accuracy)
library(caret) 
library(e1071)
#For boosting
library(randomForest)
library(fastAdaboost)

#Reading file from the excel into a dataframe in r
dataset1 <- read_excel(choose.files(),sheet = 2)
dataset1 <- as.data.frame(dataset1,check.names=T)
score_data <- read_excel(choose.files(),sheet = 1)
#Viewing the dataset
View(dataset1)
View(score_data)
score_data <- rename(score_data,`Residence Time (In current district)`=`Residence Time`)
#Class or Structure of all variables
str(dataset1)
str(score_data)

#################################### EDA #############################################

#Histograms of numerical variables
plot_histogram(dataset1[-1])


#Bargraph of Credit History and Credit Standing
ggplot(dataset1,aes(x=`Credit History`,fill=`Credit Standing`))+
  geom_bar(stat = "count",show.legend = T,position = "dodge")+
  ggtitle("Bargraph of Credit History and Credit Standing")

#Boxplot of Age vs Credit Standing
ggplot(dataset1,aes(y=Age,x=`Credit Standing`))+
  geom_boxplot(fill="skyblue")+
  ggtitle("Boxplot of Age vs Credit Standing")

#Boxplot of Age vs Credit Standing
ggplot(dataset1,aes(y=`Months since Checking Acct opened`,x=`Residence Time (In current district)`))+
  geom_point(aes(colour=`Credit Standing`))+
  ggtitle("Scatterplot of Months since Acct opened vs Residence Time")

#Employment vs credit history
table(Credit_Standing=dataset1$`Credit Standing`,Employment=dataset1$Employment)

#Visualizing data for finding if predictors are strongly correlated
pairs.panels(dataset1[-1])


###########################DATA CLEANING###################################

#Plot missing vaules(NA)
plot_missing(dataset1)

#Count of missing values
profile_missing(dataset1)

#Personal status column has some missing values
#Using statistical approch to deal with missing values (only 6 values missing out of 780 obversations)
#As the column=Personal status is categorical and character type we will use mode of the column  to fill missing values
#By table we can find frequent occuring the variables.
table(dataset1$`Personal Status`)

#As the Personal Status=Divorced has highest frequency we will replace all missing values in this column with it.
dataset1$`Personal Status` <- ifelse(is.na(dataset1$`Personal Status`),"Divorced",dataset1$`Personal Status`)
dataset1$`Personal Status`

#Similar method is used for handling missing values
table(dataset1$Housing)

#As the Housing=Own has highest frequency we will replace all missing values in this column with it.
dataset1$Housing <- ifelse(is.na(dataset1$Housing),"Own",dataset1$Housing)
dataset1$Housing

#For employment
employ <- table(dataset1$Employment)
employ
dataset1$Employment <- as.factor(dataset1$Employment)
#FOr missing values in employment column will use knn model to predict the missing values in Employment column.
dataset1 <- kNN(dataset1,variable = "Employment")
dataset1$Employment_imp <- NULL

#Converting all charcter type to factors using mutate_if of dpylr
dataset1 <- dataset1 %>%
  mutate_if(sapply(dataset1, is.character), as.factor)
score_data <- score_data %>%
  mutate_if(sapply(score_data, is.character), as.factor)

#checking whether class all character variables is factor
str(dataset1)
str(score_data)
#Summary of all variables
summary(dataset1)

#Min value of residence Time is -2 which is incorrect as time cannot be negative
dataset1$`Residence Time (In current district)`
#from above command : 391(row) obervation has value -2 will make it as NA 
dataset1[391,12] <- NA

#Impute the value by mean of the column and aS all values in this column are integer will make it integer
dataset1[391,12] <- as.integer(mean(dataset1$`Residence Time (In current district)`,na.rm = TRUE))
dataset1[391,12]

####################DECISION TREE#################################

#ID column is not taken as predictor hence it is removed
tree_data <- dataset1[-1]
#View(tree_data)
summary(tree_data)
#Data partition

#set.seed to last three digit of student ID 
set.seed(524)

#Partitioning dataset into training set and testing set
index <- sample(2,nrow(tree_data),replace = T,prob = c(0.8,0.2))

#Put 80% of data in training set
train <- tree_data[index==1,]


#Put remaining 20% in test set
test <- tree_data[index==2,]


#Decision tree model
d_tree <- rpart(`Credit Standing` ~.,data=train)
d_tree

#Visualising decision tree
prp(d_tree,extra = 4)
plotcp(d_tree)
printcp(d_tree)

#Predicting test data
pred <- predict(d_tree,test,type="class")
pred

pred_score <- predict(d_tree,score_data,type = "class")
pred_score
#Confusion matrix
confusionMatrix(test$`Credit Standing`,pred)

################### Random Forest #################################################
#Formatting all the variable names for generating random forest
names(train) <- make.names(names(train))
names(train)
set.seed(524)
rf_model <- randomForest(Credit.Standing ~.,data=train,ntree=200)
rf_model

#Predicting test data
names(test) <- make.names(names(test))
names(test)
rf_pred <- predict(rf_model,test,type="class")
rf_pred

#Confusion matrix
confusionMatrix(rf_pred,test$Credit.Standing)

#More about the model
plot(rf_model)
hist(treesize(rf_model),main = "Number of the nodes",col="skyblue")
#Important variables
varImpPlot(rf_model)
importance(rf_model)

################################ Adaboost ########################################
#Model 
boost_model <- adaboost(Credit.Standing ~ .,data = train,nIter = 70)
boost_model

#Predicting test data
boost_pred <- predict(boost_model,test,type="class")
boost_pred

#Confusion matrix
confusionMatrix(boost_pred$class,test$Credit.Standing)


################## ANAMOLY DECTECTION ################

# Predicting test data
pred_all <- predict(d_tree,tree_data,type="class")
#Putting predicted actual and ids in data frame
t<- data.frame(actual=tree_data$`Credit Standing`,predicted=pred_all)
t$error <- ifelse(t$actual == t$predicted,0,1)
t$id <- c(1:780)
View(t)
# Vector for storing result
t1 <- rep(0,780)
#Checking for consecutive error
for (i in 1:780){
  if(t$error[i]==1){
    t1[i] <- t$id[i]
  }
}

t1
# 622 to 638 has wrong values are feed in.

################ ROC ######################

tree_data1 <- tree_data
names(tree_data1) <- make.names(names(tree_data1))
names(tree_data1)


# Prediction probabilities of the dataset 
predAll <-predict(boost_model,tree_data1,type="p")
predAll<- as.data.frame(predAll$prob)
predAll
#Make Bad=0 and Good=1 for comparing probabilities
actual <- ifelse(tree_data$`Credit Standing`=="Bad",0,1)
#Two vwctors x and y for storing point to plot
x <- rep(0,12)
y <- rep(0,12)
#Function to calculate sensitivity which is plotted on Y-axis
fun_sensitvty <- function(conf_matrix){
  sensitvty <- conf_matrix[2,2]/(conf_matrix[2,2]+conf_matrix[1,2])
  return(sensitvty)
}
#Function to calculate specificity (Here, 1-specificity is plotted on X-axis)

fun_specficty <- function(conf_matrix){
  specficty <- conf_matrix[1,1]/(conf_matrix[1,1]+conf_matrix[2,1])
  return(1-specficty)
}

for(i in 1:9){
  #Setting threshold cut-off from 0.1 to o,9
  c <- i/10
  pred_value <- ifelse(predAll[,2] > c, 1, 0)
  #Confusion matrix
  conf_matrix<- table(pred_value,actual)
  print(conf_matrix)
  #Calculating sensitivity and specificity for each threshold
  y[i] <- fun_sensitvty(conf_matrix)
  x[i] <- fun_specficty(conf_matrix)
}

#for 10 th point cut-off is set to .45
pred_value <- ifelse(predAll[,2] > .45, 1, 0)
conf_matrix<- table(pred_value,actual)
print(conf_matrix)
y[10] <- fun_sensitvty(conf_matrix)
x[10] <- fun_specficty(conf_matrix)
#for cut-off=0
pred_value <- ifelse(predAll[,2] > 0, 1, 0)
conf_matrix<- table(pred_value,actual)
print(conf_matrix)
y[11] <- conf_matrix[1,2]/(conf_matrix[1,2]+0)
x[11] <- 1-0
#for cut-off=1
pred_value <- ifelse(predAll[,2] > 1, 1, 0)
conf_matrix<- table(pred_value,actual)
print(conf_matrix)
y[12] <- 0
x[12] <- 1-(conf_matrix[1,1]/(conf_matrix[1,1]+0))
x <- sort(x)
y <- sort(y)
x
y
plot(x,y,type="l",main="ROC Curve",xlab="1-Specificity",ylab="Sensitivity",col="Blue",lwd=5)
abline(a=0,b=1,lwd=2)

