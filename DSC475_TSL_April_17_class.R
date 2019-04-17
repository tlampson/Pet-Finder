#April 17 class

#pull in data and clean it up


#Load all datasets
breed_guide1 <- read.csv("C:/Users/tlampson/Desktop/Archive/DSC475/breed_labels_1.csv")
color <- read.csv("C:/Users/tlampson/Desktop/Archive/DSC475/color_labels.csv")
state <- read.csv("C:/Users/tlampson/Desktop/Archive/DSC475/state_labels.csv")
train <- read.csv("C:/Users/tlampson/Desktop/Archive/DSC475/train/train.csv")

pet <- train
str(pet)

#reclassify variables

pet$Type <- factor(pet$Type, levels=c("0", "1" , "2"), labels = c("Not Specified", "Dog", "Cat" ))
pet$Breed1 <-factor(pet$Breed1, levels=breed_guide1$Breed1, labels = breed_guide1$BreedName1)
pet$Breed2 <-factor(pet$Breed2, levels=breed_guide1$Breed1, labels = breed_guide1$BreedName1)
pet$Gender <- factor(pet$Gender, levels=c("0", "1" , "2", "3"), labels = c("Not Specified", "Male", "Female" , "Mixed"))
pet$Color1 <- factor(pet$Color1, levels=color$ColorID, labels = color$ColorName)
pet$Color2 <- factor(pet$Color2, levels=color$ColorID, labels = color$ColorName)
pet$Color3 <- factor(pet$Color3, levels=color$ColorID, labels = color$ColorName)
pet$MaturitySize <- factor(pet$MaturitySize, levels=c("0", "1" , "2", "3", "4"), labels = c("Not Specified", "Small" , "Medium", "Large", "Extra Large"))
pet$FurLength <- factor(pet$FurLength, levels=c("0", "1" , "2", "3"), labels = c("Not Specified", "Short" , "Medium", "Long"))
pet$Vaccinated <- factor(pet$Vaccinated, levels=c("1" , "2", "3"), labels = c("Yes", "No" , "Not Specified"))
pet$Dewormed <- factor(pet$Dewormed, levels=c("1" , "2", "3"), labels = c("Yes", "No" , "Not Specified"))
pet$Sterilized <- factor(pet$Sterilized, levels=c("1" , "2", "3"), labels = c("Yes", "No" , "Not Specified"))
pet$Health <- factor(pet$Health, levels=c("0", "1" , "2", "3"), labels = c("Not Specified", "Healthy" , "Minor Injury", "Serious Injury"))
pet$State <- factor(pet$State, levels=state$StateID, labels = state$StateName)
pet$PhotoAmt <- as.integer(pet$PhotoAmt)
pet$AdoptionSpeed <- factor(pet$AdoptionSpeed, levels=c("0", "1" , "2", "3", "4"), labels = c("0", "1" , "2", "3", "4"))

str(pet)

#load libraries 

library(C50)
library(gmodels) 
library(class)
library(neuralnet)
library(e1071)
library(randomForest)
library(party)
#remove extraneous columns
# unique columns Name(2), RescuerID(19), Description(21), PetID(22)

pet_tree <- pet
random <- sample(14993)
pet_tree <- pet_tree[random, ]
pet_tree_A <- pet_tree[, c(-2,-19,-21,-22)]
str(pet_tree_A)


#c.50 models

set.seed(1812)
train_sample <- sample(14993, 12750)
head(train_sample)
pet_train <- pet_tree_A[train_sample, ] 
pet_test  <- pet_tree_A[-train_sample, ]
pet_tree_train <- pet_tree[train_sample, ] 
pet_tree_test  <- pet_tree[-train_sample, ]
prop.table(table(pet_train$AdoptionSpeed))
prop.table(table(pet_test$AdoptionSpeed))

pet_model <- C5.0(pet_train[-20], pet_train$AdoptionSpeed) 
summary(pet_model)
pet_pred <- predict(pet_model, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

matrix_dimensions <- list(c("0", "1", "2", "3", "4"), c("0", "1", "2", "3", "4"))
names(matrix_dimensions) <- c("predicted", "actual") 

error_cost_1 <- matrix(c(0, 1, 2, 3, 4, 1, 0, 1, 2, 3, 2, 1, 0, 1, 2, 3, 2, 1, 0, 1, 4, 3, 2, 1, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_1 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_1) 
summary(pet_model_c_1)
pet_pred_c_1 <- predict(pet_model_c_1, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_1, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_2 <- matrix(c(0, 1, 1, 1, 7, 1, 0, 1, 1, 6, 1, 1, 0, 1, 5, 1, 1, 1, 0, 4, 7, 6, 5, 4, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_2 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_2) 
summary(pet_model_c_2)
pet_pred_c_2 <- predict(pet_model_c_2, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_2, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_3 <- matrix(c(0, 2, 2, 2, 1, 2, 0, 2, 2, 1, 2, 2, 0, 2, 1, 2, 2, 2, 0, 1, 1, 1, 1, 1, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_3 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_3) 
summary(pet_model_c_3)
pet_pred_c_3 <- predict(pet_model_c_3, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_3, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_4 <- matrix(c(0, 3, 4, 2, 1, 3, 0, 3, 2, 1, 4, 3, 0, 2, 1, 2, 2, 2, 0, 1, 1, 1, 1, 1, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_4 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_4) 
summary(pet_model_c_4)
pet_pred_c_4 <- predict(pet_model_c_4, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_4, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_5 <- matrix(c(0, 30, 40, 20, 1, 30, 0, 30, 20, 1, 40, 30, 0, 20, 1, 20, 20, 20, 0, 1, 1, 1, 1, 1, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_5 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_5) 
summary(pet_model_c_5)
pet_pred_c_5 <- predict(pet_model_c_5, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_5, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_6 <- matrix(c(0, 30, 40, 20, 10, 30, 0, 30, 20, 10, 40, 30, 0, 20, 10, 20, 20, 20, 0, 10, 10, 10, 10, 10, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_6 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_6) 
summary(pet_model_c_6)
pet_pred_c_6 <- predict(pet_model_c_6, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_6, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_7 <- matrix(c(0, 1, 20, 30, 40, 1, 0, 1, 20, 30, 20, 1, 0, 10, 20, 30, 20, 10, 0, 1, 40, 30, 20, 1, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_7 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_7) 
summary(pet_model_c_7)
pet_pred_c_7 <- predict(pet_model_c_7, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_7, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))


error_cost_8 <- matrix(c(0, 6, 7, 8, 4, 6, 0, 6, 7, 3, 7, 6, 0, 6, 2, 8, 7, 6, 0, 1, 4, 3, 2, 1, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_8 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_8) 
summary(pet_model_c_8)
pet_pred_c_8 <- predict(pet_model_c_8, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_8, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_9 <- matrix(c(0, 10, 10, 10, 8, 10, 0, 10, 10, 6, 10, 10, 0, 10, 4, 10, 10, 10, 0, 2, 18, 16, 14, 12, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_9 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_9) 
summary(pet_model_c_9)
pet_pred_c_9 <- predict(pet_model_c_9, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_9, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_11 <- matrix(c(0, 10, 10, 6, 10, 10, 0, 10, 4, 10, 10, 10, 0, 2, 10, 16, 14, 12, 0, 12, 10, 10, 10, 2, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_11 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_11) 
summary(pet_model_c_11)
pet_pred_c_11 <- predict(pet_model_c_11, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_11, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_12 <- matrix(c(0, 10, 4, 10, 10, 10, 0, 2, 10, 10, 14, 12, 0, 12, 14, 10, 10, 2, 0, 10, 10, 10, 4, 10, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_12 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_12) 
summary(pet_model_c_12)
pet_pred_c_12 <- predict(pet_model_c_12, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_12, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_13 <- matrix(c(0, 12, 14, 16, 18, 2, 0, 10, 10, 10, 4, 10, 0, 10, 10, 6, 10, 10, 0, 10, 8, 10, 10, 10, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_13 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_13) 
summary(pet_model_c_13)
pet_pred_c_13 <- predict(pet_model_c_13, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_13, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))

error_cost_14 <- matrix(c(0, 2, 10, 10, 10, 12, 0, 12, 14, 16, 10, 2, 0, 10, 10, 10, 4, 10, 0, 10, 10, 6, 10, 10, 0), nrow = 5,    dimnames = matrix_dimensions) 
pet_model_c_14 <- C5.0(pet_train[-20], pet_train$AdoptionSpeed, costs = error_cost_14) 
summary(pet_model_c_14)
pet_pred_c_14 <- predict(pet_model_c_14, pet_test) 
CrossTable(pet_test$AdoptionSpeed, pet_pred_c_14, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual Speed', 'predicted speed'))


##########################
#KNN
KNN_pet<-train
KNN_pet <- KNN_pet[ ,c(-2,-19,-21,-22)]
str(KNN_pet)
#changing PhotoAmt to int to match the rest of the variables and classifying AdoptionSpeed as a factor
KNN_pet$PhotoAmt <- as.integer(KNN_pet$PhotoAmt)
KNN_pet$AdoptionSpeed <- factor(KNN_pet$AdoptionSpeed, levels=c("0", "1" , "2", "3", "4"), labels = c("0", "1" , "2", "3", "4"))
#dewormed, sterilized, and vaccinated are all 1-3 representing "Yes", "No", and "Unsure". This should be changed so that "Unsure" lies midway between 1 and 3.
KNN_pet$Dewormed <- factor(KNN_pet$Dewormed, levels=c("1" , "2", "3"), labels = c("1", "3" , "2"))
KNN_pet$Dewormed <- as.integer(KNN_pet$Dewormed)
KNN_pet$Sterilized <- factor(KNN_pet$Sterilized, levels=c("1" , "2", "3"), labels = c("1", "3" , "2"))
KNN_pet$Sterilized <- as.integer(KNN_pet$Sterilized)

KNN_pet$Vaccinated <- factor(KNN_pet$Vaccinated, levels=c("1" , "2", "3"), labels = c("1", "3" , "2"))
KNN_pet$Vaccinated <- as.integer(KNN_pet$Vaccinated)
KNNpet_z <-(as.data.frame(scale(KNN_pet[-20])))
KNNpet_train_z <-KNNpet_z[1:12750, ]
KNNpet_test_z <-KNNpet_z[12751:14993, ]
KNNpet_train_labels_z <-KNN_pet[1:12750, 20]
KNNpet_test_labels_z <-KNN_pet[12751:14993, 20]
KNNpet_test_pred_150 <-knn(train = KNNpet_train_z, test = KNNpet_test_z, cl = KNNpet_train_labels_z, k = 150) 
CrossTable(x = KNNpet_test_labels_z, y = KNNpet_test_pred_150, prop.chisq=FALSE) 



##########################
# CI
pet_ci_tree_trial <- ctree(AdoptionSpeed~., data=pet_train)
plot(pet_ci_tree_trial, main="Conditional Inference Tree")
pet_ci_tree_trial_pred <- predict(pet_ci_tree_trial, pet_test, type="response")
pet_ci_tree_trial_perf <- table(pet_test$AdoptionSpeed, pet_ci_tree_trial_pred, dnn=c("Actual", "Predicted"))
pet_ci_tree_trial_perf


#######
#Neural Nets



######
#Model Prediction dataframe for number output neural net

Predictions4 <- data.frame("Pet ID" = pet_tree_test[22], "c5_Prediction_0" = pet_pred,"c5_Prediction_1" = pet_pred_c_1, "c5_Prediction_2" = pet_pred_c_2,"c5_Prediction_3" = pet_pred_c_3,"c5_Prediction_4" = pet_pred_c_4,"c5_Prediction_5" = pet_pred_c_5,"c5_Prediction_6" = pet_pred_c_6,"c5_Prediction_7" = pet_pred_c_7,"c5_Prediction_8" = pet_pred_c_8,"c5_Prediction_9" = pet_pred_c_9,"c5_Prediction_11" = pet_pred_c_11,"c5_Prediction_12" = pet_pred_c_12,"c5_Prediction_13" = pet_pred_c_13,"c5_Prediction_14" = pet_pred_c_14,"KNN_Prediction_1" = KNNpet_test_pred_150, "CI_Prediction_1" = pet_ci_tree_trial_pred,"AdoptionSpeed"= pet_tree_test[24]) 
str(Predictions4)
PredN4<-Predictions4[,-1]
PredN4$c5_Prediction_0 <- as.integer(PredN4$c5_Prediction_0)
PredN4$c5_Prediction_1 <- as.integer(PredN4$c5_Prediction_1)
PredN4$c5_Prediction_2 <- as.integer(PredN4$c5_Prediction_2)
PredN4$c5_Prediction_3 <- as.integer(PredN4$c5_Prediction_3)
PredN4$c5_Prediction_4 <- as.integer(PredN4$c5_Prediction_4)
PredN4$c5_Prediction_5 <- as.integer(PredN4$c5_Prediction_5)
PredN4$c5_Prediction_6 <- as.integer(PredN4$c5_Prediction_6)
PredN4$c5_Prediction_7 <- as.integer(PredN4$c5_Prediction_7)
PredN4$c5_Prediction_8 <- as.integer(PredN4$c5_Prediction_8)
PredN4$c5_Prediction_9 <- as.integer(PredN4$c5_Prediction_9)
PredN4$c5_Prediction_11 <- as.integer(PredN4$c5_Prediction_11)
PredN4$c5_Prediction_12 <- as.integer(PredN4$c5_Prediction_12)
PredN4$c5_Prediction_13 <- as.integer(PredN4$c5_Prediction_13)
PredN4$c5_Prediction_14 <- as.integer(PredN4$c5_Prediction_14)
PredN4$KNN_Prediction_1 <- as.integer(PredN4$KNN_Prediction_1)
PredN4$CI_Prediction_1 <- as.integer(PredN4$CI_Prediction_1)
PredN4$AdoptionSpeed <- as.integer(PredN4$AdoptionSpeed)

#######
# Number output Neural Net

PredN4_model_3 <- neuralnet(AdoptionSpeed ~ c5_Prediction_0 + c5_Prediction_3 + c5_Prediction_4 + c5_Prediction_9 + c5_Prediction_11 +  c5_Prediction_12 +c5_Prediction_13 +c5_Prediction_14 +KNN_Prediction_1  +  CI_Prediction_1, data = PredN4, hidden=c(5,3), stepmax= 1e+08)
plot(PredN4_model_3)
model_results_PredN4_3 <- compute(PredN4_model_3, PredN4)
predicted_speed_PredN4_3 <- model_results_PredN4_3$net.result
cor(predicted_speed_PredN4_3, PredN4$AdoptionSpeed)
#.4390398

##########
#Prediction Dataframe for factor output Neural Net
PredN5 <-PredN4
PredN5$AdoptionSpeed <- as.factor(PredN5$AdoptionSpeed)

#Neural Net for factor output

PredN5_model_1 <- neuralnet(AdoptionSpeed ~ c5_Prediction_0 + c5_Prediction_3 + c5_Prediction_4 + c5_Prediction_9 + c5_Prediction_11 +  c5_Prediction_12 +c5_Prediction_13 +c5_Prediction_14 +KNN_Prediction_1  +  CI_Prediction_1, data = PredN5, hidden=c(5,3), stepmax= 1e+08)
plot(PredN5_model_1)
model_results_PredN5_1 <- compute(PredN5_model_1, PredN5)
predicted_speed_PredN5_1 <- model_results_PredN5_1$net.result
cor(predicted_speed_PredN5_1, PredN5$AdoptionSpeed)


########
#Dataframe for reducing factor predictions to a single 0-4 factor variable
factor_results<- as.data.frame(predicted_speed_PredN5_1)

factor_results$test <-NA
factor_results$test <- ifelse(factor_results$V1>factor_results$V2,factor_results$V1,factor_results$V2)
factor_results$test2 <-NA
factor_results$test2 <- ifelse(factor_results$V4>factor_results$V3,factor_results$V4,factor_results$V3)
factor_results$test3 <-NA
factor_results$test3 <- ifelse(factor_results$test>factor_results$test2,factor_results$test,factor_results$test2)
factor_results$test4 <-NA
factor_results$test4 <- ifelse(factor_results$test3>factor_results$V5,factor_results$test3,factor_results$V5)
factor_results$Speed <-NA
factor_results$Speed[which(factor_results$test4<=factor_results$V1)]<-0
factor_results$Speed[which(factor_results$test4<=factor_results$V2)]<-1
factor_results$Speed[which(factor_results$test4<=factor_results$V3)]<-2
factor_results$Speed[which(factor_results$test4<=factor_results$V4)]<-3
factor_results$Speed[which(factor_results$test4<=factor_results$V5)]<-4


#numeric predictions as integers
numnetpred<-predicted_speed_PredN4_3

numnetpred<-as.integer(numnetpred)


#Dataframe for comparing neural net predictions and adoptionspeed

net_results<- data.frame("num" = predicted_speed_PredN4_3, "num2" = numnetpred, "fact"= factor_results$Speed, "speed"= pet_tree_test[24] )
net_results$round<-NA
net_results$round<-round(net_results$num, digits=0)
net_results$int <-NA
net_results$int[which(net_results$num2 == 1)]<- 0
net_results$int[which(net_results$num2 == 2)]<- 1
net_results$int[which(net_results$num2 == 3)]<- 2
net_results$int[which(net_results$num2 == 4)]<- 3
net_results$int[which(net_results$num2 == 5)]<- 4
table(net_results$round)
table(net_results$int)

str(net_results)
net_results$round <- factor(net_results$round, levels=c("0", "1" , "2", "3", "4"), labels = c("0", "1" , "2", "3", "4"))
net_results$int <- factor(net_results$int, levels=c("0", "1" , "2", "3", "4"), labels = c("0", "1" , "2", "3", "4"))

net_results$roundyes <- NA
net_results$roundyes <- 0
net_results$roundyes[which(net_results$round == net_results$AdoptionSpeed)] <- 1
net_results$intyes <- NA
net_results$intyes <- 0
net_results$intyes[which(net_results$int == net_results$AdoptionSpeed)] <- 1
net_results$factyes <-0
net_results$factyes[which(net_results$fact == net_results$AdoptionSpeed)] <- 1

sum(net_results$roundyes)
sum(net_results$intyes)
sum(net_results$factyes)
668/2243
#numeric output has correlation of .4390398

#round has an accuracy of 30.8%
#int has an accuracy of 29.78%
#factor output has an accuracy of 41.72%

#################

