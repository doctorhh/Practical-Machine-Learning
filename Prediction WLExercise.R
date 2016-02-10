library(ggplot2)
library(caret)
library(randomForest)
library(rpart)
library(rattle)


# After an inital file load, it has been observed that a lot of variable contained
# NA, blank space and #DIV/0! value.
# Easier and shortest solution to consider blank space, #DIV/0! and NA as NA
train_file <- read.csv(file = 'pml-training.csv',na.strings = c('NA','#DIV/0!',''))
test_file <- read.csv(file = 'pml-testing.csv',na.strings = c('NA','#DIV/0!',''))

ggplot(train_file, aes(classe, ..count..)) + geom_bar()

#Remove column containing NA value (based on the parameter from the read.csv)
train_data <- train_file[,colSums(is.na(train_file)) == 0]
test_data <- test_file[,colSums(is.na(train_file)) == 0]

#data <- data[,-seq(1:7)]
train_data<-train_data[,-(1:7)]
test_data<-test_data[,-(1:7)]

# Partition data set between training and validation 
set.seed(101) 
intrain <- createDataPartition(train_data$classe, p = 0.7, list = FALSE) 
training = train_data[intrain, ] # 70% split 
testing <- train_data[-intrain, ] # 30% split 

# Model Classification Tree
set.seed(202)
fitControl = trainControl(method = "cv", number = 5)
model_fit_Rpart <- train(classe ~.,data = training, method="rpart",trControl=fitControl)
# The application of the rpart() generated more accuracy than the train() function
# function but created more variances. Hence the selection of the train() function
# model_fit_Rpart <- rpart(classe ~ ., data=training, method="class")
model_fit_Rpart$finalModel
fancyRpartPlot(model_fit_Rpart$finalModel)

# Testing the model and predicting value from the testing set.
predict_Rpart<-predict(model_fit_Rpart,testing)
# predict_Rpart<-predict(model_fit_Rpart,testing, type='class')
conf_matrix_Rpart<-confusionMatrix(predict_Rpart,testing$classe)
conf_matrix_Rpart

# Model Random Forest
set.seed(303)
# Setting the parameter for the resampling method to "out of Box" sampling and to
# limit the number of 5 iteration instead of the default of 25 (to save CPU processing) time
trControl = trainControl(method = "oob", number=5)
model_fit_RF <- train(classe ~.,data = training, method="rf", trControl=trControl)
# Due to memory & time constraint, the original randomForest () has provided similar 
# result kappa (.9944) compared to the train() with the "rf" method.
# model_fit_RF <- randomForest(classe ~ ., training)
model_fit_RF

# Testing the model and predicting value from the testing set.
predict_RF<-predict(model_fit_RF,testing)
conf_matrix_RF<-confusionMatrix(predict_RF,testing$classe)
conf_matrix_RF

#Test file validation
predict_valid <- predict(model_fit_RF, test_data) 
predict_valid

# code as suggested by Coursera
pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
}
pml_write_files(predict_valid)
