#############PREPROCESSING######################
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

### ANURAN DATA SET

## import data
anuran <- read.csv("C:/Users/Rania Yunaningrum/OneDrive/Dokumen/SMT 7/SML/Frogs_MFCCs.csv")
head(anuran)
str(anuran)
dim(anuran)
anuran$Genus <- as.factor(anuran$Genus)

#drop Family, Species and RecordID
anuran <- anuran[,-c(23,25,26)]
head(anuran)

#identifying missing value
colSums(is.na(anuran)) #tidak ada missing value

#Standarisasi data
library(dplyr)
std <- anuran %>% mutate_each_(funs(scale(.) %>% as.vector), 
                               vars=c("MFCCs_.1","MFCCs_.2","MFCCs_.3","MFCCs_.4","MFCCs_.5","MFCCs_.6","MFCCs_.7",
                                      "MFCCs_.8","MFCCs_.9","MFCCs_10","MFCCs_11","MFCCs_12","MFCCs_13","MFCCs_14","MFCCs_15",
                                      "MFCCs_16","MFCCs_17","MFCCs_18","MFCCs_19","MFCCs_20","MFCCs_21","MFCCs_22"))
head(std)
summary(std)
dim(std)
str(std)

#Feature sellection data
cor_matrix <- cor(std %>% select_if(is.numeric))

# Buat heatmap dari matriks korelasi
library(ggplot2)
library(tidyr)
library(reshape2)
ggplot(data = melt(cor_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 1)), vjust = 0.5) +
  scale_fill_gradient(low = "blue", high = "purple") +
  theme(axis.text.x = element_text(size = 7)) +
  labs(title = "Heatmap Matriks Korelasi Variabel Prediktor")

selected_features <- findCorrelation(cor_matrix, cutoff = 0.7)
selected_data <- std[, selected_features]
head(selected_data)
#variabel yang digunakan adala (MFCCs_22, MFCCs_17, MFCCs_11, MFCCs_15, MFCCs_14, MFCCs_12, MFCCs_.5)
std <- data.frame(std$Genus,selected_data)
str(std)
head(std)

#Pembagian data training(0.7) dan testing(0.3)
table(std$std.Genus)
set.seed(1)
split_index <- createDataPartition(std$std.Genus, p = 0.7, list = FALSE, times = 1)
training_data <- std[split_index, ]
testing_data <- std[-split_index, ]
dim(training_data)
dim(testing_data)
table(training_data$std.Genus)
table(testing_data$std.Genus)

#pengecekan outlier
data_numerik <- std %>% select_if(is.numeric)
boxplot(data_numerik)

## data hasil pre processing
install.packages("writexl")
library(writexl)

write_xlsx(training_data, path = "C:/Users/Rania Yunaningrum/OneDrive/Dokumen/SMT 7/SML/Frogs_MFCCs.csvtrain_anuran.xlsx")

write_xlsx(testing_data, path = "C:/Users/Rania Yunaningrum/OneDrive/Dokumen/SMT 7/SML/Frogs_MFCCs.csvtest_anuran.xlsx")

#data yang digunakan adalah 
#training : training_data
#testing : testing_data
#dengan 8 kategori

#############SVC################################
library(e1071)
library(caret)

kernel_types <- c("rbfdot", "polydot", "vanilladot","tanhdot", "laplacedot", "besseldot")
models <- list()

# Loop through the kernel types and train SVC models
for (kernel_type in kernel_types) {
  model <- ksvm(std.Genus ~ ., data = training_data, kernel = kernel_type, type = "C-svc", kpar = "automatic",C=5, epsilon = 0.1)
  models[[kernel_type]] <- model
  cat("SVC Model with Kernel Type:", kernel_type, "\n")
  print(model)
  print("====================")
}

for (kernel_type in kernel_types) {
  predictions <- predict(models[[kernel_type]], newdata = testing_data)
  cat("Accuracy for with Kernel Type:", kernel_type, "\n")
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = testing_data$std.Genus))
  accuracy <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy:", accuracy, "\n")
}

##kernel terbaik lapacian, dicari C terbaik
for (i in 1:100) {
  # Create an SVC model
  model <- ksvm(std.Genus ~ ., data = training_data, kernel = "laplacedot", type = "C-svc", kpar = "automatic",C=i, epsilon = 0.1)
  models[[i]] <- model
}

for (i in 1:100) {
  predictions <- predict(models[[i]], newdata = testing_data)
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = testing_data$std.Genus))
  accuracy[i] <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy saat C=",i," : ", accuracy[i], "\n")
}
cat("akurasi tertinggi sebesar",max(accuracy), "pada C =", which.max(accuracy))

#C terbaik adalah C = 4, dicari epsilon terbaik
for (i in 1:100) {
  # Create an SVC model
  model <- ksvm(std.Genus ~ ., data = training_data, kernel = "laplacedot", type = "C-svc", kpar = "automatic",C=4, epsilon = i*0.1)
  models[[i]] <- model
}

for (i in 1:100) {
  predictions <- predict(models[[i]], newdata = testing_data)
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = testing_data$std.Genus))
  accuracy[i] <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy saat epsilon=",i*0.01," : ", accuracy[i], "\n")
}
cat("akurasi tertinggi sebesar",max(accuracy), "pada epsilon =", which.max(accuracy)*0.01)

#model optimal 
model <- ksvm(std.Genus ~ ., data = training_data, kernel = "laplacedot", type = "C-svc", kpar = "automatic",C=4, epsilon = 0.02)

#evaluasi model pada data training
predictions <- predict(model, newdata = training_data)
confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = training_data$std.Genus))
print(table(Predicted = predictions, Actual = training_data$std.Genus))
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
confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = testing_data$std.Genus))
print(table(Predicted = predictions, Actual = testing_data$std.Genus))
sensitivity <- mean(confusion_matrix$byClass[,"Sensitivity"])
specificity  <- mean(confusion_matrix$byClass[,"Specificity"])
precision <- mean(confusion_matrix$byClass[,"Precision"])
recall  <- mean(confusion_matrix$byClass[,"Recall"])
F1  <- mean(confusion_matrix$byClass[,"F1"])
accuracy <- confusion_matrix$overall["Accuracy"]
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", F1, "\n")
cat("Accuracy:", accuracy, "\n")

#############Regresi Logistik################
library(pscl)
library(nnet)
library(caret)

# MODEL 1
logit1data1 <- multinom(std.Genus~., data = training_data)
summary(logit1data1)

# Uji Serentak 
pR2(logit1data1)
g22 = 7421.8061131
x2 = qchisq(0.95, 8)

cat('Uji Serentak', '\nH0: tidak ada pengaruh variabel bebas secara simultan terhadap variabel tak bebas' ,
    '\nH1: ada pengaruh paling sedikit satu variabel bebas terhadap variabel tak bebas\n',
    'alpha = 0.05', 'G2 = ', g22, 'chisq = ', x2,
    if(g22 > x2){
      paste0("\nKeputusan: Tolak H0")
    } else {
      paste0("\nKeputusan: Gagal Tolak H0")
    })

#model 2
logit2data1 <- multinom(std.Genus~MFCCs_22+MFCCs_17+MFCCs_11+MFCCs_.9+MFCCs_15+
                          MFCCs_14+MFCCs_12, data = training_data)
summary(logit2data1)
#model 3
logit3data1 <- multinom(std.Genus~MFCCs_22+MFCCs_17+MFCCs_11+MFCCs_.9+MFCCs_15+
                          MFCCs_12, data = training_data)
summary(logit3data1)
#model 4
logit4data1 <- multinom(std.Genus~MFCCs_22+MFCCs_17+MFCCs_11+MFCCs_.9+MFCCs_15
                        , data = training_data)
summary(logit4data1)
#model 5
logit5data1 <- multinom(std.Genus~MFCCs_22+MFCCs_17+MFCCs_.9+MFCCs_15, data = training_data)
summary(logit5data1)
#model 6
logit6data1 <- multinom(std.Genus~MFCCs_22+MFCCs_17+MFCCs_.9, data = training_data)
summary(logit6data1)
#model 7
logit7data1 <- multinom(std.Genus~MFCCs_22+MFCCs_17, data = training_data)
summary(logit7data1)
#model 8
logit8data1 <- multinom(std.Genus~MFCCs_17, data = training_data)
summary(logit8data1)

# Uji Parsial
#H0: variabel independen ke j tidak mempunyai pengaruh secara signifikan terhadap variabel dependen
#H1: variabel independen ke j mempunyai pengaruh secara signifikan terhadap variabel dependen
data.frame(Model = c("Model 1","Model 2","Model 3","Model 4","Model 5",
                     "Model 6","Model 7","Model 8"), AIC = c(logit1data1$AIC,logit2data1$AIC,
                                                             logit3data1$AIC,logit4data1$AIC,
                                                             logit5data1$AIC,logit6data1$AIC,
                                                             logit7data1$AIC,logit8data1$AIC))
#model 1 terpilih

## Evaluasi Model

#training
training_data$pred <- predict(logit1data1, training_data, type="class")
training_data$pred <- as.factor(training_data$pred)
has3tr <- confusionMatrix(training_data$pred, training_data$std.Genus)
has3tr

#testing
testing_data$pred <- predict(logit1data1, testing_data, type="class")
testing_data$pred <- as.factor(testing_data$pred)
has3ts <- confusionMatrix(testing_data$pred, testing_data$std.Genus)
has3ts

data.frame("Training" = c("Precision", "Recall", "F1-score", "Accuracy"), 
           "RegLog" = c(has3tr$byClass[3], has3tr$byClass[1], 
                        (2*(has3tr$byClass[3]*has3tr$byClass[1])/(has3tr$byClass[3]+has3tr$byClass[1])),
                        has3tr$overall[1]*100))

data.frame("Testing" = c("Precision", "Recall", "F1-score", "Accuracy"), 
           "RegLog" = c(has3ts$byClass[3], has3ts$byClass[1], 
                        (2*(has3ts$byClass[3]*has3ts$byClass[1])/(has3ts$byClass[3]+has3ts$byClass[1])),
                        has3ts$overall[1]*100))
#############Decision tree############
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Pisahkan atribut (fitur) dan label (kategori genus)
head(training_data)
x_train <- training_data[,-1]  # Atribut
y_train <- training_data[,1]  # Label
head(y_train)
head(x_train)
# Buat objek pohon keputusan
tree_model <- rpart(std.Genus ~ ., data =training_data)

# Visualisasi Decision Tree
options(repr.plot.width = 10, repr.plot.height = 8, repr.plot.res = 1500)
prp(tree_model, type = 2, extra = 101, fallen.leaves = TRUE, varlen = 0,
    faclen = 0, tweak = 1.5)

# library untuk confusion matrix
install.packages("caret")
library(caret)

#confusion matrix
predictions_train <- predict(tree_model, newdata = training_data, type = "class")
predictions_train <- as.factor(predictions_train)
table(predictions_train)
table(y_train)
y_train <- as.factor(y_train)

# Create a confusion matrix
confusion_matrix_train <- confusionMatrix(predictions_train, y_train)

# Print the confusion matrix data training
print(confusion_matrix_train)

#menghitung evaluasi pada data training
confusion_matrix_train$overall[1]*100
recall_tr = confusion_matrix_train$byClass[1]
accuracy_tr = confusion_matrix_train$overall[1]
precision_tr = confusion_matrix_train$byClass[3]
eval_train <- tibble(recall_tr,
                     accuracy_tr,
                     precision_tr)
eval_train

F1_score = 2 * (precision_tr * recall_tr) / (precision_tr + recall_tr)
F1_score

# confusion matrix kategori adenomera
head(predict(tree_model, trainyx))

pred <- data.frame(predict(tree_model, trainyx))
head(pred)

for (i in 1:nrow(pred)) {
  pred$pre[i] <- ifelse(pred$X0[i]>=.5,0,1)
}
head(pred)
hasil_dectree <- confusionMatrix(table(predict(tree_model, trainyx)[,2]>=.5,
                                       trainyx$y == "Adenomera"))
hasil_dectree


hasil_dectree$overall[1]*100

F1_score <- hasil_dectree$byClass["F1"]
recall <- hasil_dectree$byClass[6]
accuracy <- hasil_dectree$overall[1]
precision <- hasil_dectree$byClass[5]

print(F1_score)
print(recall)
print(accuracy)
print(precision)

# Prediksi kategori genus pada data testing
head(testing_data)
x_test = testing_data[,-1]
y_test = testing_data[,1]
head(x_test)
head(y_test)


predictions_test <- predict(tree_model, data.frame(x_test), type = "class")
head(predictions_test)
length(predictions_test)
length(y_test)
y_test <- as.factor(y_test)
predictions_test <- as.factor(predictions_test)

confusion_matrix <- confusionMatrix(predictions_test, y_test)
confusion_matrix


# evaluasi model dengan data testing
recall = confusion_matrix$byClass[1]
accuracy = confusion_matrix$overall[1]
precision = confusion_matrix$byClass[3]
eval_test <- tibble(recall,
                    accuracy,
                    precision)
eval_test

F1_score = 2 * (precision * recall) / (precision + recall)
F1_score

## menghitung True Positive
# Kategori kelas yang ada dalam data
kategori_kelas <- unique(c(predictions_test, y_test))
kategori_kelas

# Inisialisasi vektor untuk menyimpan TP untuk setiap kategori
TP_per_kategori <- numeric(length(kategori_kelas))

# Hitung TP untuk setiap kategori
for (i in 1:length(kategori_kelas)) {
  kategori <- kategori_kelas[i]
  TP_per_kategori[i] <- sum(predictions_test == kategori & y_test == kategori)
}

# Hitung jumlah TP secara keseluruhan
TP_keseluruhan <- sum(TP_per_kategori)

# Tampilkan TP untuk setiap kategori dan jumlah TP keseluruhan
for (i in 1:length(kategori_kelas)) {
  cat("Kategori", kategori_kelas[i], ": TP =", TP_per_kategori[i], "\n")
}

cat("Total TP Keseluruhan:", TP_keseluruhan, "\n")

############Random Forest###################
library(randomForest)
library(caret)

#ctrl21a <- trainControl(method="repeatedcv", number=2, repeats=1)
#anurancat <- train(std.Genus~., data=training_data, method="rf", trControl = ctrl21a)

#saveRDS(anurancat, "anurancat_2N1R.RDS")

s21a <- readRDS("anurancat_2N1R.RDS")
s21a$finalModel

#plot
plot_rf1a <- plot(s21a$finalModel)
legend("topright", colnames(s21a$finalModel$err.rate),col=1:6,cex=0.8,fill=1:6)

## Evaluasi Model

#training
has4tr <- confusionMatrix(predict(s21a, training_data), training_data$std.Genus)
has4tr

#testing
has4ts <- confusionMatrix(predict(s21a, testing_data), testing_data$std.Genus)
has4ts

data.frame("Training" = c("Precision", "Recall", "F1-score", "Accuracy"), 
           "RandFor" = c(has4tr$byClass[3], has4tr$byClass[1], 
                         (2*(has4tr$byClass[3]*has4tr$byClass[1])/(has4tr$byClass[3]+has4tr$byClass[1])),
                         has4tr$overall[1]*100))

data.frame("Testing" = c("Precision", "Recall", "F1-score", "Accuracy"), 
           "RandFor" = c(has4ts$byClass[3], has4ts$byClass[1], 
                         (2*(has4ts$byClass[3]*has4ts$byClass[1])/(has4ts$byClass[3]+has4ts$byClass[1])),
                         has4ts$overall[1]*100))

############GRADIENT BOOSTING MACHINE# ####
library (caret)
library(gbm)

fitContr <- trainControl (method = "cv", number = 10)
tuneGrid <- expand.grid(interaction.depth = 2, n.trees = 500,
                        shrinkage = 0.1, n.minobsinnode = 10)
set.seed (1)
modfit.gbm1 <- train(std.Genus~., method="gbm", data=train_data,
                     trControl = fitContr, verbose = FALSE, tuneGrid=tuneGrid)
train.gbm <- predict(modfit.gbm1, train_data)
pred.gbm <- predict(modfit.gbm1, newdata=test_data)
confusion_matrix_train <- confusionMatrix(data = train.gbm,
                                          reference = train_data$std.Genus)
confusion_matrix_test <- confusionMatrix(data = pred.gbm,
                                         reference = test_data$std.Genus)

# Calculate accuracy, precision, recall, and F1 score for training data
accuracy_train <- mean(train.gbm == train_data$std.Genus)
precision_train <- confusion_matrix_train$byClass[1] 
recall_train <- confusion_matrix_train$byClass[2]   
f1_score_train <- 2 * (precision_train * recall_train) / (precision_train + recall_train)

# Calculate accuracy, precision, recall, and F1 score for testing data
accuracy_test <- mean(pred.gbm == test_data$std.Genus)
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



table (train.gbm, train_data$std.Genus)
modfit.gbm$resample


pred.gbm <- predict(modfit.gbm1, newdata=test_data)


# Load the required libraries
library(caret)
install.packages('gbm')
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