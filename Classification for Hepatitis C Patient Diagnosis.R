##########PREPROCESSING HCV######################
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(neuralnet)
library(caret)
library(kernlab)
library(rpart)

# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

### HCV DATA SET
getwd()

## import data
hcv <- read.csv("C:/Users/Rania Yunaningrum/OneDrive/Dokumen/SMT 7/SML/hcvdat0.csv")
head(hcv)
dim(hcv)
str(hcv)
#mengubah category mencadi level
hcv$Category <- as.factor(hcv$Category)
hcv$Sex <- as.factor(hcv$Sex)

#drop Variabel X
hcv <- hcv[,-1]
head(hcv)
dim(hcv)

#indentifying missing value
colSums(is.na(hcv)) 
#tidak ada na di respon, sehingga semua data misval diganti ke median setiap kategori

#penggantian missing value dengan median
#ALB
hcv$ALB[which(is.na(hcv$ALB))] <- median(na.omit(hcv$ALB[which(hcv$Category == hcv$Category[which(is.na(hcv$ALB))])]))
#ALT
hcv$ALT[which(is.na(hcv$ALT))] <- median(na.omit(hcv$ALT[which(hcv$Category == hcv$Category[which(is.na(hcv$ALT))])]))
#PROT
hcv$PROT[which(is.na(hcv$PROT))] <- median(na.omit(hcv$PROT[which(hcv$Category == hcv$Category[which(is.na(hcv$PROT))])]))
#ALP
hcv$ALP[which(is.na(hcv$ALP) & hcv$Category == "1=Hepatitis")] <- median(na.omit(hcv$ALP[which(hcv$Category == "1=Hepatitis")]))
hcv$ALP[which(is.na(hcv$ALP) & hcv$Category == "2=Fibrosis")] <- median(na.omit(hcv$ALP[which(hcv$Category == "2=Fibrosis")]))
hcv$ALP[which(is.na(hcv$ALP) & hcv$Category == "3=Cirrhosis")] <- median(na.omit(hcv$ALP[which(hcv$Category == "3=Cirrhosis")]))
#CHOL
hcv$CHOL[which(is.na(hcv$CHOL) & hcv$Category == "0=Blood Donor")] <- median(na.omit(hcv$CHOL[which(hcv$Category == "0=Blood Donor")]))
hcv$CHOL[which(is.na(hcv$CHOL) & hcv$Category == "2=Fibrosis")] <- median(na.omit(hcv$CHOL[which(hcv$Category == "2=Fibrosis")]))
hcv$CHOL[which(is.na(hcv$CHOL) & hcv$Category == "3=Cirrhosis")] <- median(na.omit(hcv$CHOL[which(hcv$Category == "3=Cirrhosis")]))

colSums(is.na(hcv)) #periksa missing value yang belum terhandle

#Standarisasi data
library(dplyr)
std <- hcv %>% mutate_each_(funs(scale(.) %>% as.vector), 
                            vars=c("Age","ALB","ALP","ALT","AST","BIL",
                                   "CHE","CHOL","CREA","GGT","PROT"))
head(std)
summary(std)
dim(std)

#Feature sellection data
cor_matrix <- cor(std %>% select_if(is.numeric))

# Buat heatmap dari matriks korelasi
library(ggplot2)
library(tidyr)
library(reshape2)
ggplot(data = melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +
  scale_fill_gradient(low = "pink", high = "purple") +
  theme_minimal() +
  labs(title = "Heatmap Matriks Korelasi Variabel Numerik")

selected_features <- findCorrelation(cor_matrix, cutoff = 0.7)
selected_data <- std[, selected_features]
print(selected_data)
#tidak ada variabel yang disingkirkan karena nilai korelasi setiap antar variabel <0.7

#pengecekan outlier
data_numerik <- std %>% select_if(is.numeric)
boxplot(data_numerik)

#Pembagian data training(0.7) dan testing(0.3)
table(std$Category)
set.seed(1)
split_index <- createDataPartition(std$Category, p = 0.7, list = FALSE, times = 1)
training_data <- std[split_index, ]
testing_data <- std[-split_index, ]
table(training_data$Category)
table(testing_data$Category)
dim(training_data)
dim(testing_data)

#data yang digunakan adalah 
#training : training_data
#testing : testing_data
#dengan 5 kategori

## data hasil pre processing
install.packages("writexl")
library(writexl)

write_xlsx(training_data, path = "C:/Users/Rania Yunaningrum/OneDrive/Dokumen/SMT 7/SML/train_hcv.xlsx")

write_xlsx(testing_data, path = "C:/Users/Rania Yunaningrum/OneDrive/Dokumen/SMT 7/SML/test_hcv.xlsx")

#data yang digunakan adalah 
#training : training_data
#testing : testing_data
#dengan 8 kategori

##########SVC###########################################
library(e1071)
library(caret)

kernel_types <- c("rbfdot", "polydot", "vanilladot","tanhdot", "laplacedot", "besseldot")

# Create an empty list to store models
models <- list()

# Loop through the kernel types and train SVC models
for (kernel_type in kernel_types) {
  model <- ksvm(Category ~ ., data = training_data, kernel = kernel_type, type = "C-svc", kpar = "automatic",C=5, epsilon = 0.1)
  models[[kernel_type]] <- model
  cat("SVC Model with Kernel Type:", kernel_type, "\n")
  print(model)
  print("====================")
}

for (kernel_type in kernel_types) {
  predictions <- predict(models[[kernel_type]], newdata = testing_data)
  cat("Accuracy for with Kernel Type:", kernel_type, "\n")
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = testing_data$Category))
  accuracy <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy:", accuracy, "\n")
}

##kernel terbaik Laplace, dicari C terbaik
for (i in 1:100) {
  # Create an SVC model
  model <- ksvm(Category ~ ., data = training_data, kernel = "laplacedot", type = "C-svc", kpar = "automatic",C=i, epsilon = 0.1)
  models[[i]] <- model
}

for (i in 1:100) {
  predictions <- predict(models[[i]], newdata = testing_data)
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = testing_data$Category))
  accuracy[i] <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy saat C=",i," : ", accuracy[i], "\n")
}
cat("akurasi tertinggi sebesar",max(accuracy), "pada C =", which.max(accuracy))

#C terbaik adalah C = 3, dicari epsilon terbaik
for (i in 1:100) {
  # Create an SVC model
  model <- ksvm(Category ~ ., data = training_data, kernel = "laplacedot", type = "C-svc", kpar = "automatic",C=3, epsilon = i*0.1)
  models[[i]] <- model
}

for (i in 1:100) {
  predictions <- predict(models[[i]], newdata = testing_data)
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = testing_data$Category))
  accuracy[i] <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy saat epsilon=",i*0.01," : ", accuracy[i], "\n")
}
cat("akurasi tertinggi sebesar",max(accuracy), "pada epsilon =", which.max(accuracy)*0.01)

#model optimal
model <- ksvm(Category ~ ., data = training_data, kernel = "laplacedot", type = "C-svc", kpar = "automatic",C=3, epsilon = 0.01)

#evaluasi model pada data training
predictions <- predict(model, newdata = training_data)
confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = training_data$Category))
print(table(Predicted = predictions, Actual = training_data$Category))
sensitivity <- mean(confusion_matrix$byClass[,"Sensitivity"])
specificity  <- mean(confusion_matrix$byClass[,"Specificity"])
precision <- mean(confusion_matrix$byClass[,"Precision"])
recall <- mean(confusion_matrix$byClass[,"Recall"])
F1  <- mean(confusion_matrix$byClass[,"F1"])
accuracy <- confusion_matrix$overall["Accuracy"]
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", F1, "\n")
cat("Accuracy:", accuracy, "\n")

#evaluasi model pada data testing
predictions <- predict(model, newdata = testing_data)
confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = testing_data$Category))
print(table(Predicted = predictions, Actual = testing_data$Category))
sensitivity <- mean(confusion_matrix$byClass[,"Sensitivity"])
specificity  <- mean(confusion_matrix$byClass[,"Specificity"])
precision <- mean(confusion_matrix$byClass[,"Precision"])
recall <- mean(confusion_matrix$byClass[,"Recall"])
F1  <- mean(confusion_matrix$byClass[,"F1"])
accuracy <- confusion_matrix$overall["Accuracy"]
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", F1, "\n")
cat("Accuracy:", accuracy, "\n")

##########Regresi Logistik########################
library(pscl)
library(nnet)
library(caret)

# MODEL 1
logit1data <- multinom(Category~., data = training_data)
summary(logit1data)

# Uji Serentak 
pR2(logit1data)
g2 = 470.4847706
x = qchisq(0.95, 12)

cat('Uji Serentak', '\nH0: tidak ada pengaruh variabel bebas secara simultan terhadap variabel tak bebas' ,
    '\nH1: ada pengaruh paling sedikit satu variabel bebas terhadap variabel tak bebas\n',
    'alpha = 0.05', 'G2 = ', g2, 'chisq = ', x,
    if(g2 > x){
      paste0("\nKeputusan: Tolak H0")
    } else {
      paste0("\nKeputusan: Gagal Tolak H0")
    })

#model 2
logit2data <- multinom(Category~Age+ALB+ALP+ALT+AST+BIL+CHE+
                         CHOL+CREA+GGT+PROT, data = training_data)
summary(logit2data)
#model 3
logit3data <- multinom(Category~Age+ALB+ALP+AST+BIL+CHE+
                         CHOL+CREA+GGT+PROT, data = training_data)
summary(logit3data)
#model 4
logit4data <- multinom(Category~Age+ALP+AST+BIL+CHE+
                         CHOL+CREA+GGT+PROT, data = training_data)
summary(logit4data)
#model 5
logit5data <- multinom(Category~Age+ALP+AST+CHE+
                         CHOL+CREA+GGT+PROT, data = training_data)
summary(logit5data)
#model 6
logit6data <- multinom(Category~Age+ALP+AST+CHE+
                         CHOL+GGT+PROT, data = training_data)
summary(logit6data)
#model 7
logit7data <- multinom(Category~Age+ALP+AST+CHE+CHOL+GGT, data = training_data)
summary(logit7data)
#model 8
logit8data <- multinom(Category~ALP+AST+CHE+CHOL+GGT, data = training_data)
summary(logit8data)
#model 9
logit9data <- multinom(Category~ALP+CHE+CHOL+GGT, data = training_data)
summary(logit9data)
#model 10
logit10data <- multinom(Category~ALP+CHE+CHOL, data = training_data)
summary(logit10data)
#model 11
logit11data <- multinom(Category~CHE+CHOL, data = training_data)
summary(logit11data)
#model 12
logit12data <- multinom(Category~CHOL, data = training_data)
summary(logit12data)

# Uji Parsial
#H0: variabel independen ke j tidak mempunyai pengaruh secara signifikan terhadap variabel dependen
#H1: variabel independen ke j mempunyai pengaruh secara signifikan terhadap variabel dependen
data.frame(Model = c("Model 1","Model 2","Model 3","Model 4","Model 5",
                     "Model 6","Model 7","Model 8","Model 9","Model 10",
                     "Model 11","Model 12"), AIC = c(logit1data$AIC,logit2data$AIC,
                                                     logit3data$AIC,logit4data$AIC,
                                                     logit5data$AIC,logit6data$AIC,
                                                     logit7data$AIC,logit8data$AIC,
                                                     logit9data$AIC,logit10data$AIC,
                                                     logit11data$AIC,logit12data$AIC))
#model 2 terpilih

## Evaluasi Model

#training
training_data$pred <- predict(logit2data, training_data, type="class")
training_data$pred <- as.factor(training_data$pred)
has1tr <- confusionMatrix(training_data$pred, training_data$Category)
has1tr

#testing
testing_data$pred <- predict(logit2data, testing_data, type="class")
testing_data$pred <- as.factor(testing_data$pred)
has1ts <- confusionMatrix(testing_data$pred, testing_data$Category)
has1ts

data.frame("Training" = c("Precision", "Recall", "F1-score", "Accuracy"), 
           "RegLog" = c(has1tr$byClass[3], has1tr$byClass[1], 
                        (2*(has1tr$byClass[3]*has1tr$byClass[1])/(has1tr$byClass[3]+has1tr$byClass[1])),
                        has1tr$overall[1]*100))

data.frame("Testing" = c("Precision", "Recall", "F1-score", "Accuracy"), 
           "RegLog" = c(has1ts$byClass[3], has1ts$byClass[1], 
                        (2*(has1ts$byClass[3]*has1ts$byClass[1])/(has1ts$byClass[3]+has1ts$byClass[1])),
                        has1ts$overall[1]*100))
##########DECISION TREE###########################
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Pisahkan atribut (fitur) dan label (Category)
head(training_data)
dim(training_data)
x_train <- training_data[,-1]  # Atribut
y_train <- training_data[,1]  # Label
head(y_train)
head(x_train)
# Buat objek pohon keputusan
tree_model <- rpart(Category ~ ., data = training_data)
tree_model

# Visualisasikan pohon keputusan dengan ukuran teks yang lebih besar
prp(tree_model, type = 2, extra = 101, fallen.leaves = TRUE, varlen = 0, faclen = 0, tweak = 1)


# library untuk confusion matrix
install.packages("caret")
library(caret)
library(tidyverse)

## Prediksi klasifikasi pada data training
#confusion matrix
predictions_train <- predict(tree_model, newdata = training_data, type = "class")
predictions_train <- as.factor(predictions_train)
y_train <- as.factor(y_train)
head(predictions_train)
table(predictions_train)
head(y_train)
table(y_train)

# Create a confusion matrix
confusion_matrix_train <- confusionMatrix(predictions_train, y_train)

# Print the confusion matrix data training
print(confusion_matrix_train)

#Evaluasi pada data training
recall_tr = confusion_matrix_train$byClass[1]
accuracy_tr = confusion_matrix_train$overall[1]
precision_tr = confusion_matrix_train$byClass[3]
eval_train <- tibble(recall_tr,
                     accuracy_tr,
                     precision_tr)
eval_train

F1_score = 2 * (precision_tr * recall_tr) / (precision_tr + recall_tr)
F1_score


## data testing
head(testing_data)
x_test = testing_data[,-1]
y_test = testing_data[,1]
head(x_test)
head(y_test)

## Prediksi pada data testing
predictions_test <- predict(tree_model, data.frame(testing_data), type = "class")
head(predictions_test)
length(predictions_test)
table(predictions_test)

length(y_test)
table(y_test)
y_test <- as.factor(y_test)
predictions_test <- as.factor(predictions_test)

confusion_matrix <- confusionMatrix(predictions_test, y_test)
confusion_matrix


# evaluasi model pada data testing
recall = confusion_matrix$byClass[1]
accuracy = confusion_matrix$overall[1]
precision = confusion_matrix$byClass[3]
eval_test <- tibble(recall,
                    accuracy,
                    precision)
eval_test

F1_score = 2 * (precision * recall) / (precision + recall)
F1_score

##########RANDOM FOREST#############################
library(randomForest)
library(caret)

#ctrl21 <- trainControl(method="repeatedcv", number=2, repeats=1)
#hcvcat <- train(Category~., data=training_data, method="rf", trControl = ctrl21)

#saveRDS(hcvcat, "hcvcat_2N1R.RDS")

s21 <- readRDS("hcvcat_2N1R.RDS")
s21$finalModel

#plot
plot_rf1 <- plot(s21$finalModel)
legend("topright", colnames(s21$finalModel$err.rate),col=1:6,cex=0.8,fill=1:6)

## Evaluasi Model

#training
has2tr <- confusionMatrix(predict(s21, training_data), training_data$Category)
has2tr

#testing
has2ts <- confusionMatrix(predict(s21, testing_data), testing_data$Category)
has2ts

data.frame("Training" = c("Precision", "Recall", "F1-score", "Accuracy"), 
           "RandFor" = c(has2tr$byClass[3], has2tr$byClass[1], 
                         (2*(has2tr$byClass[3]*has2tr$byClass[1])/(has2tr$byClass[3]+has2tr$byClass[1])),
                         has2tr$overall[1]*100))

data.frame("Testing" = c("Precision", "Recall", "F1-score", "Accuracy"), 
           "RandFor" = c(has2ts$byClass[3], has2ts$byClass[1], 
                         (2*(has2ts$byClass[3]*has2ts$byClass[1])/(has2ts$byClass[3]+has2ts$byClass[1])),
                         has2ts$overall[1]*100))

##########GRADIENT BOOST MACHINE######################
library (caret)
library(gbm)

fitControl <- trainControl (method = "cv", number = 10)
tune_Grid <- expand.grid(interaction.depth = 2, n.trees = 500,
                         shrinkage = 0.1, n.minobsinnode = 10)
set.seed (1)
modfit.gbm <- train(Category~., method="gbm", data=training_data,
                    trControl = fitControl, verbose = FALSE, tuneGrid=tune_Grid)

#hasil klasifikasi berdasarkan training data
train.gbm <- predict(modfit.gbm, training_data)

#hasil prediksi model yang dilakukan pada data testing
pred.gbm <- predict(modfit.gbm, newdata=testing_data)

#confusion matrix for train data
confusion_matrix_train <- confusionMatrix(data = train.gbm,
                                          reference = training_data$Category)
#confusion matrix for test data
confusion_matrix_test <- confusionMatrix(data = pred.gbm,
                                         reference = testing_data$Category)

table (train.gbm, training_data$Category)
modfit.gbm$resample

# Calculate accuracy, precision, recall, and F1 score for training data
accuracy_train <- mean(train.gbm == training_data$Category)
precision_train <- confusion_matrix_train$byClass[1] 
recall_train <- confusion_matrix_train$byClass[2]   
f1_score_train <- 2 * (precision_train * recall_train) / (precision_train + recall_train)

# Calculate accuracy, precision, recall, and F1 score for testing data
accuracy_test <- mean(pred.gbm == testing_data$Category)
precision_test <- confusion_matrix_test$byClass[1] 
recall_test <- confusion_matrix_test$byClass[2]   
f1_score_test <- 2 * (precision_test * recall_test) / (precision_test + recall_test)

# Print
cat("Accuracy train:", accuracy_train, "\n")
cat("Precision train:", precision_train, "\n")
cat("Recall train:", recall_train, "\n")
cat("f1 score train:", f1_score_train, "\n")

# Print
cat("Accuracy test:", accuracy_test, "\n")
cat("Precision test:", precision_test, "\n")
cat("Recall test:", recall_test, "\n")
cat("f1 score test:", f1_score_test, "\n")


asli = testing_data$Category

rmse_multinomial <- function(pred.gbm, asli) {
  n_samples <- nrow(pred.gbm)
  n_classes <- ncol(pred.gbm)
  
  # Initialize the sum of squared errors
  sum_squared_errors <- 0
  
  for (i in 1:n_samples) {
    squared_errors <- (pred.gbm[i,] - asli[i])^2
    sum_squared_errors <- sum_squared_errors + sum(squared_errors)
  }
  
  # Calculate the mean squared error
  mean_squared_error <- sum_squared_errors / (n_samples * n_classes)
  
  # Calculate the RMSE-like metric
  rmse <- sqrt(mean_squared_error)
  
  return(rmse)
}

rmse_multinomial(pred.gbm, asli)
