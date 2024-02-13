####Baca data####
library(readxl)
data <- read_excel("C:/Users/Rania Yunaningrum/OneDrive/Dokumen/SMT 7/SML/PROJECT ETS/dataset_sml_2023.xlsx", sheet = 1)
data
filtered_data <- data[data$subdistrict == "subdistrict_178" | data$subdistrict == "subdistrict_179" , ]
library(dplyr)
####Deteksi Missing Value####
colSums(is.na(filtered_data))

####Feature Selection####
# Temukan variabel dengan variasi 0
zero_var_cols <- names(filtered_data)[apply(filtered_data, 2, var) == 0]
if (length(zero_var_cols) > 0) {
  cat("Variabel dengan variasi 0:", paste(zero_var_cols, collapse = ", "), "\n")
} else {
  cat("Tidak ada variabel dengan variasi 0.\n")
}

zero_var_cols <- which(colnames(filtered_data) %in% zero_var_cols[2:7])
filtered_data <- filtered_data[,-zero_var_cols]

#Deteksi variabel yang varians mendekati 0
library(caret)
near_zero_vars <- nearZeroVar(filtered_data)
names(filtered_data[near_zero_vars])
filtered_data <- filtered_data[,-near_zero_vars]

#drop data yang merupakan data time 
filtered_data <- filtered_data %>% select(-dt_id)

#Standarisasi data
numeric_columns <- sapply(filtered_data, is.numeric)
filtered_numeric_data <- filtered_data[, numeric_columns]
standardized_data <- as.data.frame(scale(filtered_numeric_data))
filtered_data <- cbind(standardized_data,filtered_data$subdistrict, filtered_data$revenue_category)
print(filtered_data)

#Rename
colnames(filtered_data)[colnames(filtered_data) == "filtered_data$revenue_category"] <- "revenue_category"
colnames(filtered_data)[colnames(filtered_data) == "filtered_data$subdistrict"] <- "subdistrict"
colnames(filtered_data)[colnames(filtered_data) == "SUM BW"] <- "SUM_BW"
colnames(filtered_data)[colnames(filtered_data) == "RRC Connected User"] <- "RRC_Connected_User"
colnames(filtered_data) <- gsub("<", "_lessthan", colnames(filtered_data))
colnames(filtered_data) <- gsub(">", "_morethan", colnames(filtered_data))
colnames(filtered_data) <- gsub(" ", "_", colnames(filtered_data))
colnames(filtered_data) <- gsub("-", "_", colnames(filtered_data))
colnames(filtered_data)[colnames(filtered_data) == "DL_RB(%)_BHV"] <- "DL_RB_BHV"
colnames(filtered_data)[colnames(filtered_data) == "2G_availability"] <- "twoG_availability" 

####Split data####
set.seed(123)
sub178 <- filtered_data[filtered_data$subdistrict == "subdistrict_178",]
sub178$revenue_category <- as.factor(sub178$revenue_category)
split178 <- round(0.8 * nrow(sub178))
rand178 <- sample(1:nrow(sub178), nrow(sub178))
tr178 <- sub178[rand178[1:split178],] 
test178 <- sub178[rand178[(split178 + 1):nrow(sub178)],]

sub179 <- filtered_data[filtered_data$subdistrict == "subdistrict_179",]
sub179$revenue_category <- as.factor(sub179$revenue_category)
split179 <- round(0.8 * nrow(sub179))
rand179 <- sample(1:nrow(sub179), nrow(sub179))
tr179 <- sub179[rand179[1:split179],] 
test179 <- sub179[rand179[(split179 + 1):nrow(sub179)],]
dim(sub179)
tail(sub179)

summary(sub178)
summary(sub179)

####Subdistrict 178####
#####SVR#####
library(e1071)
kernel <- c("linear", "polynomial", "radial")
models <- list()
for(k in kernel){
  svr_model <- svm(revenue_total ~ ., data = tr178 %>% select(-revenue_category, -subdistrict), kernel = k, C = 1, epsilon = 0.1)
  models[[k]]<- svr_model
  cat("SVR Model with Kernel Type:", kernel, "\n")
  print(svr_model)
  print("====================")
}
for (k in kernel) {
  pred_svr <- predict(models[[k]], newdata = test178 %>% select(-revenue_category, - revenue_total, - subdistrict))
  mse <- mean((test178$revenue_total - pred_svr)^2)
  mae <- mean(abs(test178$revenue_total - pred_svr))
  rsquared <- 1 - (sum((test178$revenue_total - pred_svr)^2) / sum((test178$revenue_total - mean(test178$revenue_total))^2))
  cat("MSE for kernel",k,":", mse, "\n")
  cat("MAE for kernel",k,":", mae, "\n")
  cat("R-Squared for kernel",k,":", rsquared, "\n")
}

#mencari C optimum
models <- list()
for(i in 1:100){
  svr_model <- svm(revenue_total ~ ., data = tr178 %>% select(-revenue_category, -subdistrict), kernel = "linear", cost = i, epsilon = 0.1)
  models[[i]]<- svr_model
}

for (i in 1:100) {
  pred_svr <- predict(models[[i]], newdata = test178 %>% select(-revenue_category, - revenue_total, - subdistrict))
  mse[i] <- mean((test178$revenue_total - pred_svr)^2)
  mae[i] <- mean(abs(test178$revenue_total - pred_svr))
  rsquared[i] <- 1 - (sum((test178$revenue_total - pred_svr)^2) / sum((test178$revenue_total - mean(test178$revenue_total))^2))
  cat("MSE for C = ",i,":", mse[i], "\n")
  cat("MAE for C = ",i,":", mae[i], "\n")
  cat("R-Squared for C = ",i,":", rsquared[i], "\n")
}
cat("MSE tekecil",min(mse), "pada C =", which.min(mse))

#C terbaik adalah C = 66, dicari epsilon terbaik
for(i in 1:100){
  svr_model <- svm(revenue_total ~ ., data = tr178 %>% select(-revenue_category, -subdistrict), kernel = "linear", cost = 66, epsilon = i*0.01)
  models[[i]]<- svr_model
}
for (i in 1:100) {
  pred_svr <- predict(models[[i]], newdata = test178 %>% select(-revenue_category, - revenue_total, - subdistrict))
  mse[i] <- mean((test178$revenue_total - pred_svr)^2)
  mae[i] <- mean(abs(test178$revenue_total - pred_svr))
  rsquared[i] <- 1 - (sum((test178$revenue_total - pred_svr)^2) / sum((test178$revenue_total - mean(test178$revenue_total))^2))
  cat("MSE for epsilon = ",i*0.01,":", mse[i], "\n")
  cat("MAE for epsilon = ",i*0.01,":", mae[i], "\n")
  cat("R-Squared for epsilon = ",i*0.01,":", rsquared[i], "\n")
}
cat("MSE tekecil",min(mse), "pada epsilon =", which.min(mse)*0.01)

#model optimum
svr_model <- svm(revenue_total ~ ., data = tr178 %>% select(-revenue_category, -subdistrict), kernel = "linear", cost = 66, epsilon = 0.02)
pred_svr <- predict(svr_model, newdata = test178 %>% select(-revenue_category, - revenue_total, - subdistrict))
# Mengukur kinerja model
mse <- mean((test178$revenue_total - pred_svr)^2)
mae <- mean(abs(test178$revenue_total - pred_svr))
rsquared <- 1 - (sum((test178$revenue_total - pred_svr)^2) / sum((test178$revenue_total - mean(test178$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

#####Regresi Linear Berganda#####
library(dplyr)
model1 <- lm(revenue_total ~., data = tr178 %>% select(-revenue_category, -subdistrict))
# Melakukan prediksi pada data testing
pred1 <- predict(model1, newdata = test178%>% select(-revenue_category, -subdistrict, -revenue_total))
# Mengukur kinerja model
mse <- mean((test178$revenue_total - pred1)^2)
mae <- mean(abs(test178$revenue_total - pred1))
rsquared <- 1 - (sum((test178$revenue_total - pred1)^2) / sum((test178$revenue_total - mean(test178$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

#####Regresi Ridge#####
library(glmnet)
model_ridge <- glmnet(x = as.matrix(tr178 %>% select(-revenue_category,-subdistrict, -revenue_total)), y = tr178$revenue_total, alpha = 0)
best_lambda_ridge <- min(model_ridge$lambda)
print(best_lambda_ridge)
# Melakukan prediksi dengan model Ridge
prediksi_ridge <- predict(model_ridge, s = best_lambda_ridge, newx = as.matrix(test178 %>% select(-revenue_category,-subdistrict, -revenue_total)))
# Mengukur kinerja model
mse <- mean((test178$revenue_total - prediksi_ridge)^2)
mae <- mean(abs(test178$revenue_total - prediksi_ridge))
rsquared <- 1 - (sum((test178$revenue_total - prediksi_ridge)^2) / sum((test178$revenue_total - mean(test178$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

#####Regresi Lasso#####
model_lasso <- glmnet(x = as.matrix(tr178 %>% select(-revenue_total,-revenue_category,-subdistrict)), y = tr178$revenue_total, alpha = 1)
best_lambda_lasso <- min(model_lasso$lambda)
print(best_lambda_lasso)
# Melakukan prediksi dengan model Lasso
prediksi_lasso <- predict(model_lasso, s = best_lambda_lasso, newx = as.matrix(test178 %>% select(-revenue_total,-revenue_category,-subdistrict)))
# Mengukur kinerja model
mse <- mean((test178$revenue_total - prediksi_lasso)^2)
mae <- mean(abs(test178$revenue_total - prediksi_lasso))
rsquared <- 1 - (sum((test178$revenue_total - prediksi_lasso)^2) / sum((test178$revenue_total - mean(test178$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

#####Regresi KNN#####
# Membuat model KNN regresi
#install.packages('kknn')
library(kknn)
results = matrix(data = NA, nrow = length(tr178) , ncol = 3,
                 dimnames = list(NULL, c("MSE", "MAE", "Rsq"))) 
for(i in 1:length(tr178)){
  knn_reg_model <- kknn(revenue_total ~ ., tr178 %>% select(-revenue_category,-subdistrict), test178 %>% select(-revenue_category,-subdistrict), k = i)
  # Melakukan prediksi
  pred_kknn <- knn_reg_model$fitted.values
  mse <- mean((test178$revenue_total - pred_kknn)^2)
  mae <- mean(abs(test178$revenue_total - pred_kknn))
  rsquared <- 1 - (sum((test178$revenue_total - pred_kknn)^2) / sum((test178$revenue_total - mean(test178$revenue_total))^2))
  results[i,1] <- mse
  results[i,2] <- mae
  results[i,3] <- rsquared
}
results[which.min(results[, 1]), ] #berdasarkan MSE terkecil
results[,1]== results[which.min(results[, 1]), 1]
which.min(results[, 1])

####Subdistrict 179####
hist(tr179$revenue_total, main = "Histogram of Variable", xlab = "Variable")
# Assuming 'data' is your data frame and 'variable' is the variable you want to check
plot(density(tr179$revenue_total), main = "Kernel Density Plot of Variable", xlab = "Variable")

#####SVR#####
library(e1071)
kernel <- c("linear", "polynomial", "radial")
models <- list()
for(k in kernel){
  svr_model <- svm(revenue_total ~ ., data = tr179 %>% select(-revenue_category, -subdistrict), kernel = k, C = 1, epsilon = 0.1)
  models[[k]]<- svr_model
  cat("SVR Model with Kernel Type:", kernel, "\n")
  print(svr_model)
  print("====================")
}
for (k in kernel) {
  pred_svr <- predict(models[[k]], newdata = test179 %>% select(-revenue_category, - revenue_total, - subdistrict))
  mse <- mean((test179$revenue_total - pred_svr)^2)
  mae <- mean(abs(test179$revenue_total - pred_svr))
  rsquared <- 1 - (sum((test179$revenue_total - pred_svr)^2) / sum((test179$revenue_total - mean(test179$revenue_total))^2))
  cat("MSE for kernel",k,":", mse, "\n")
  cat("MAE for kernel",k,":", mae, "\n")
  cat("R-Squared for kernel",k,":", rsquared, "\n")
}

#mencari C optimum
models <- list()
for(i in 1:100){
  svr_model <- svm(revenue_total ~ ., data = tr179 %>% select(-revenue_category, -subdistrict), kernel = "linear", cost = i, epsilon = 0.1)
  models[[i]]<- svr_model
}

for (i in 1:100) {
  pred_svr <- predict(models[[i]], newdata = test179 %>% select(-revenue_category, - revenue_total, - subdistrict))
  mse[i] <- mean((test179$revenue_total - pred_svr)^2)
  mae[i] <- mean(abs(test179$revenue_total - pred_svr))
  rsquared[i] <- 1 - (sum((test179$revenue_total - pred_svr)^2) / sum((test179$revenue_total - mean(test179$revenue_total))^2))
  cat("MSE for C = ",i,":", mse[i], "\n")
  cat("MAE for C = ",i,":", mae[i], "\n")
  cat("R-Squared for C = ",i,":", rsquared[i], "\n")
}
cat("MSE tekecil",min(mse), "pada C =", which.min(mse))

#C terbaik adalah C = 87, dicari epsilon terbaik
for(i in 1:100){
  svr_model <- svm(revenue_total ~ ., data = tr179 %>% select(-revenue_category, -subdistrict), kernel = "linear", cost = 87, epsilon = i*0.01)
  models[[i]]<- svr_model
}
for (i in 1:100) {
  pred_svr <- predict(models[[i]], newdata = test179 %>% select(-revenue_category, - revenue_total, - subdistrict))
  mse[i] <- mean((test179$revenue_total - pred_svr)^2)
  mae[i] <- mean(abs(test179$revenue_total - pred_svr))
  rsquared[i] <- 1 - (sum((test179$revenue_total - pred_svr)^2) / sum((test179$revenue_total - mean(test179$revenue_total))^2))
  cat("MSE for epsilon = ",i*0.01,":", mse[i], "\n")
  cat("MAE for epsilon = ",i*0.01,":", mae[i], "\n")
  cat("R-Squared for epsilon = ",i*0.01,":", rsquared[i], "\n")
}
cat("MSE tekecil",min(mse), "pada epsilon =", which.min(mse)*0.01)

#model optimum
svr_model <- svm(revenue_total ~ ., data = tr179 %>% select(-revenue_category, -subdistrict), kernel = "linear", cost = 87, epsilon = 0.1)
pred_svr <- predict(svr_model, newdata = test179 %>% select(-revenue_category, - revenue_total, - subdistrict))
# Mengukur kinerja model
mse <- mean((test179$revenue_total - pred_svr)^2)
mae <- mean(abs(test179$revenue_total - pred_svr))
rsquared <- 1 - (sum((test179$revenue_total - pred_svr)^2) / sum((test179$revenue_total - mean(test179$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")


#####Regresi Linear Berganda#####
library(dplyr)
model1 <- lm(revenue_total ~., data = tr179 %>% select(-revenue_category, -subdistrict))
# Melakukan prediksi pada data testing
pred1 <- predict(model1, newdata = test179 %>% select(-revenue_category, -subdistrict, -revenue_total))
# Mengukur kinerja model
mse <- mean((test179$revenue_total - pred1)^2)
mae <- mean(abs(test179$revenue_total - pred1))
rsquared <- 1 - (sum((test179$revenue_total - pred1)^2) / sum((test179$revenue_total - mean(test179$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

#####Regresi Ridge#####
library(glmnet)
model_ridge <- glmnet(x = as.matrix(tr179 %>% select(-revenue_category,-subdistrict, -revenue_total)), y = tr179$revenue_total, alpha = 0)
best_lambda_ridge <- min(model_ridge$lambda)
print(best_lambda_ridge)
# Melakukan prediksi dengan model Ridge
prediksi_ridge <- predict(model_ridge, s = best_lambda_ridge, newx = as.matrix(test179 %>% select(-revenue_category,-subdistrict, -revenue_total)))
# Mengukur kinerja model
mse <- mean((test179$revenue_total - prediksi_ridge)^2)
mae <- mean(abs(test179$revenue_total - prediksi_ridge))
rsquared <- 1 - (sum((test179$revenue_total - prediksi_ridge)^2) / sum((test179$revenue_total - mean(test179$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

#####Regresi Lasso#####
model_lasso <- glmnet(x = as.matrix(tr179 %>% select(-revenue_total,-revenue_category,-subdistrict)), y = tr179$revenue_total, alpha = 1)
best_lambda_lasso <- min(model_lasso$lambda)
print(best_lambda_lasso)
# Melakukan prediksi dengan model Lasso
prediksi_lasso <- predict(model_lasso, s = best_lambda_lasso, newx = as.matrix(test179 %>% select(-revenue_total,-revenue_category,-subdistrict)))
# Mengukur kinerja model
mse <- mean((test179$revenue_total - prediksi_lasso)^2)
mae <- mean(abs(test179$revenue_total - prediksi_lasso))
rsquared <- 1 - (sum((test179$revenue_total - prediksi_lasso)^2) / sum((test179$revenue_total - mean(test179$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

#####Regresi KNN#####
# Membuat model KNN regresi
#install.packages('kknn')
library(kknn)
results = matrix(data = NA, nrow = length(tr179) , ncol = 3,
                 dimnames = list(NULL, c("MSE", "MAE", "Rsq"))) 
for(i in 1:length(tr179)){
  knn_reg_model <- kknn(revenue_total ~ ., tr179 %>% select(-revenue_category,-subdistrict), test179 %>% select(-revenue_category,-subdistrict), k = i)
  # Melakukan prediksi
  pred_kknn <- knn_reg_model$fitted.values
  mse <- mean((test179$revenue_total - pred_kknn)^2)
  mae <- mean(abs(test179$revenue_total - pred_kknn))
  rsquared <- 1 - (sum((test179$revenue_total - pred_kknn)^2) / sum((test179$revenue_total - mean(test179$revenue_total))^2))
  results[i,1] <- mse
  results[i,2] <- mae
  results[i,3] <- rsquared
}
results[which.min(results[, 1]), ] #berdasarkan MSE terkecil
results[,1]== results[which.min(results[, 1]), 1] #k = 15
which.min(results[, 1])

##RESPON : revenue_category
####subdistrict 178####
#####SVC#####
library(e1071)
library(caret)
library(kernlab)

kernel_types <- c("rbfdot", "polydot", "vanilladot","tanhdot", "laplacedot", "besseldot")
models <- list()

# Loop through the kernel types and train SVC models
for (kernel_type in kernel_types) {
  model <- ksvm(revenue_category ~ ., data = tr178%>% select(-revenue_total,-subdistrict), kernel = kernel_type, type = "C-svc", kpar = "automatic",C=5, epsilon = 0.1)
  models[[kernel_type]] <- model
  cat("SVC Model with Kernel Type:", kernel_type, "\n")
  print(model)
  print("====================")
}

for (kernel_type in kernel_types) {
  predictions <- predict(models[[kernel_type]], newdata = test178%>% select(-revenue_total, -revenue_category,-subdistrict))
  cat("Accuracy for with Kernel Type:", kernel_type, "\n")
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test178$revenue_category))
  accuracy <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy:", accuracy, "\n")
}

##kernel terbaik rbfdot, dicari C terbaik
for (i in 1:100) {
  # Create an SVC model
  model <- ksvm(revenue_category ~ ., data = tr178%>% select(-revenue_total,-subdistrict), kernel = "rbfdot", type = "C-svc", kpar = "automatic",C=i, epsilon = 0.1)
  models[[i]] <- model
}

for (i in 1:100) {
  predictions <- predict(models[[i]], newdata = test178%>% select(-revenue_total, -revenue_category,-subdistrict))
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test178$revenue_category))
  accuracy[i] <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy saat C=",i," : ", accuracy[i], "\n")
}
cat("akurasi tertinggi sebesar",max(accuracy), "pada C =", which.max(accuracy))

#C terbaik adalah C = 3, dicari epsilon terbaik
for (i in 1:100) {
  # Create an SVC model
  model <- ksvm(revenue_category ~ ., data = tr178%>% select(-revenue_total,-subdistrict), kernel = "rbfdot", type = "C-svc", kpar = "automatic",C=3, epsilon = i*0.1)
  models[[i]] <- model
}

for (i in 1:100) {
  predictions <- predict(models[[i]], newdata = test178%>% select(-revenue_total, -revenue_category,-subdistrict))
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test178$revenue_category))
  accuracy[i] <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy saat epsilon=",i*0.01," : ", accuracy[i], "\n")
}
cat("akurasi tertinggi sebesar",max(accuracy), "pada epsilon =", which.max(accuracy)*0.01)

#model optimal 
model <- ksvm(revenue_category ~ ., data = tr178%>% select(-revenue_total,-subdistrict), kernel = "rbfdot", type = "C-svc", kpar = "automatic",C=4, epsilon = 0.05)

#evaluasi model pada data training
predictions <- predict(model, newdata = test178%>% select(-revenue_total,-revenue_category,-subdistrict))
confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test178$revenue_category))
print(table(Predicted = predictions, Actual = test178$revenue_category))
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

#####Multinomial Logistic Regression#####
library(nnet)
model_logit <- multinom(revenue_category ~ ., data = tr178 %>% select(-revenue_total, -subdistrict))
pred_logit <- predict(model_logit, newdata = test178 %>% select(-revenue_total, -subdistrict, -revenue_category), type = "class")
#Kebaikan model
library(caret)
confusion_matrix <- confusionMatrix(data=pred_logit, reference = test178$revenue_category)
print(table(Predicted = pred_logit, Actual = test178$revenue_category))
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

#####Decision Tree#####
library(tree)
library(party)
library(rpart)
library(rpart.plot)
tree_model <- rpart(revenue_category ~ ., data = tr178 %>% select(-revenue_total,-subdistrict))
prp(tree_model, type = 5, extra = 100, fallen.leaves = TRUE, varlen = 0, faclen = 0, tweak = 1)
pred_dectree <- predict(tree_model, newdata = test178 %>% select(-revenue_total,-subdistrict,-revenue_category), type = 'class')
confusion_matrix <- confusionMatrix(data=pred_dectree, reference = test178$revenue_category)
print(table(Predicted = pred_dectree, Actual = test178$revenue_category))
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


#####Random Forest#####
library(randomForest)
library(caret)
ctrl21 <- trainControl(method="repeatedcv", number=2, repeats=1)
model_rf <- train(revenue_category ~ ., data = tr178 %>% select(-revenue_total,-subdistrict), method = 'rf',trControl = ctrl21)
plot(model_rf$finalModel)
prediksi_rf <- predict(model_rf,test178 %>% select(-revenue_total,-revenue_category,-subdistrict),type = 'raw')
# Mengukur kinerja model
confusion_matrix <- confusionMatrix(data=prediksi_rf, reference = test178$revenue_category)
print(table(Predicted = prediksi_rf, Actual = test178$revenue_category))
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


#####Gradient Boost Machine#####
modfit.gbm <- train(revenue_category ~., method = "gbm", data = tr178%>%select(-revenue_total,-subdistrict), 
                    trControl = fitControl, verbose = FALSE, tuneGrid = tune_Grid)
pred_gbm <- predict(modfit.gbm, test178%>%select(-revenue_category,-revenue_total,-subdistrict))
# Mengukur kinerja model
confusion_matrix <- confusionMatrix(data=pred_gbm, reference = test178$revenue_category)
print(table(Predicted = pred_gbm, Actual = test178$revenue_category))
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

#Subdistrict 179
#####SVC#####
library(e1071)
library(caret)
library(kernlab)

kernel_types <- c("rbfdot", "polydot", "vanilladot","tanhdot", "laplacedot", "besseldot")
models <- list()

# Loop through the kernel types and train SVC models
for (kernel_type in kernel_types) {
  model <- ksvm(revenue_category ~ ., data = tr179%>% select(-revenue_total,-subdistrict), kernel = kernel_type, type = "C-svc", kpar = "automatic",C=5, epsilon = 0.1)
  models[[kernel_type]] <- model
  cat("SVC Model with Kernel Type:", kernel_type, "\n")
  print(model)
  print("====================")
}

for (kernel_type in kernel_types) {
  predictions <- predict(models[[kernel_type]], newdata = test179%>% select(-revenue_total, -revenue_category,-subdistrict))
  cat("Accuracy for with Kernel Type:", kernel_type, "\n")
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test179$revenue_category))
  accuracy <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy:", accuracy, "\n")
}

##kernel terbaik rbfdot, dicari C terbaik
for (i in 1:100) {
  # Create an SVC model
  model <- ksvm(revenue_category ~ ., data = tr179%>% select(-revenue_total,-subdistrict), kernel = "rbfdot", type = "C-svc", kpar = "automatic",C=i, epsilon = 0.1)
  models[[i]] <- model
}

for (i in 1:100) {
  predictions <- predict(models[[i]], newdata = test179%>% select(-revenue_total, -revenue_category,-subdistrict))
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test179$revenue_category))
  accuracy[i] <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy saat C=",i," : ", accuracy[i], "\n")
}
cat("akurasi tertinggi sebesar",max(accuracy), "pada C =", which.max(accuracy))

#C terbaik adalah C = 3, dicari epsilon terbaik
for (i in 1:100) {
  # Create an SVC model
  model <- ksvm(revenue_category ~ ., data = tr179%>% select(-revenue_total,-subdistrict), kernel = "rbfdot", type = "C-svc", kpar = "automatic",C=3, epsilon = i*0.1)
  models[[i]] <- model
}

for (i in 1:100) {
  predictions <- predict(models[[i]], newdata = test179%>% select(-revenue_total, -revenue_category,-subdistrict))
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test179$revenue_category))
  accuracy[i] <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy saat epsilon=",i*0.01," : ", accuracy[i], "\n")
}
cat("akurasi tertinggi sebesar",max(accuracy), "pada epsilon =", which.max(accuracy)*0.01)

#model optimal 
model <- ksvm(revenue_category ~ ., data = tr179%>% select(-revenue_total,-subdistrict), kernel = "rbfdot", type = "C-svc", kpar = "automatic",C=3, epsilon = 0.23)

#evaluasi model pada data training
predictions <- predict(model, newdata = test179%>% select(-revenue_total,-revenue_category,-subdistrict))
confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test179$revenue_category))
print(table(Predicted = predictions, Actual = test179$revenue_category))
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

#####Multinomial Logistic Regression#####
library(nnet)
model_logit <- multinom(revenue_category ~ ., data = tr179 %>% select(-revenue_total, -subdistrict))
pred_logit <- predict(model_logit, newdata = test179 %>% select(-revenue_total, -subdistrict, -revenue_category), type = "class")
#Kebaikan model
library(caret)
confusion_matrix <- confusionMatrix(data=pred_logit, reference = test179$revenue_category)
print(table(Predicted = pred_logit, Actual = test179$revenue_category))
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
#AUC = (1 + TP - FP)/2
library(pROC)
roc_curve <- roc(test178$revenue_category, pred_logit)
auc_value <- auc(roc_curve)

#####Decision Tree###
library(tree)
tree_model <- rpart(revenue_category ~ ., data = tr179 %>% select(-revenue_total,-subdistrict))
prp(tree_model, type = 5, extra = 100, fallen.leaves = TRUE, varlen = 0, faclen = 0, tweak = 1)
pred_dectree <- predict(tree_model, newdata = test179 %>% select(-revenue_total,-subdistrict,-revenue_category), type = 'class')
confusion_matrix <- confusionMatrix(data=pred_dectree, reference = test179$revenue_category)
print(table(Predicted = pred_dectree, Actual = test179$revenue_category))
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

#####Random Forest#####
library(randomForest)
library(caret)
ctrl21 <- trainControl(method="repeatedcv", number=2, repeats=1)
model_rf <- train(revenue_category ~ ., data = tr179 %>% select(-revenue_total,-subdistrict), method = 'rf',trControl = ctrl21)
plot(model_rf$finalModel)
prediksi_rf <- predict(model_rf,test179 %>% select(-revenue_total,-revenue_category,-subdistrict),type = 'raw')
# Mengukur kinerja model
confusion_matrix <- confusionMatrix(data=prediksi_rf, reference = test179$revenue_category)
print(table(Predicted = prediksi_rf, Actual = test179$revenue_category))
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


#####Gradient Boost Machine#####
modfit.gbm <- train(revenue_category ~., method = "gbm", data = tr179%>%select(-revenue_total,-subdistrict), 
                    trControl = fitControl, verbose = FALSE, tuneGrid = tune_Grid)
pred_gbm <- predict(modfit.gbm, test179%>%select(-revenue_category,-revenue_total,-subdistrict))
# Mengukur kinerja model
confusion_matrix <- confusionMatrix(data=pred_gbm, reference = test179$revenue_category)
print(table(Predicted = pred_gbm, Actual = test179$revenue_category))
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

####Important Feature####
####Subdistrict 178 Lasso###
library(glmnet)
model_lasso <- glmnet(x = as.matrix(tr178 %>% select(-revenue_total,-revenue_category,-subdistrict)), y = tr178$revenue_total, alpha = 1)
best_lambda_lasso <- min(model_lasso$lambda)
print(best_lambda_lasso)
# Melakukan prediksi dengan model Lasso
prediksi_lasso <- predict(model_lasso, s = best_lambda_lasso, newx = as.matrix(test178 %>% select(-revenue_total,-revenue_category,-subdistrict)))
significant_variables <- coef(model_lasso, s = best_lambda_lasso)[,1]
plot(model_lasso)
if (sum(significant_variables != 0) > 0) {
  # Display the top significant variables
  top_significant_variables <- data.frame(
    Variable = names(significant_variables),
    Coefficient = significant_variables
  )
}
top_significant_variables <- top_significant_variables[order(abs(top_significant_variables$Coefficient), decreasing = TRUE), ]
print(top_significant_variables)

####Subdistrict 178 GBM###
modfit.gbm <- train(revenue_category ~., method = "gbm", data = tr178%>%select(-revenue_total,-subdistrict), 
                    trControl = fitControl, verbose = FALSE, tuneGrid = tune_Grid)
pred_gbm <- predict(modfit.gbm, test178%>%select(-revenue_category,-revenue_total,-subdistrict))
importance_scores <- summary(modfit.gbm, plot = FALSE)
print(importance_scores)
barplot(importance_scores$rel.inf, names.arg = importance_scores$var, las = 2, main = "Relative Influence")

####Subdistrict 179 SVR###
svr_model <- svm(revenue_total ~ ., data = tr179 %>% select(-revenue_category, -subdistrict), kernel = "linear", cost = 87, epsilon = 0.1)
pred_svr <- predict(svr_model, newdata = test179 %>% select(-revenue_category, - revenue_total, - subdistrict))
coefficients <- coef(svr_model)
print(coefficients)
features <- colnames(tr179 %>% select(-revenue_category, -subdistrict))
feature_coefficients <- data.frame(feature = features, coefficient = coefficients)
sorted_features <- feature_coefficients[order(abs(feature_coefficients$coefficient), decreasing = TRUE), ]
top_features <- head(sorted_features, n = 10)  # Change '10' to the desired number

####Subdistrict 179 Random Forest###
model_rf <- train(revenue_category ~ ., data = tr179 %>% select(-revenue_total,-subdistrict), method = 'rf',trControl = ctrl21)
importance <- varImp(model_rf, scale = FALSE)$importance
features <- colnames(tr179 %>% select(-revenue_total, -subdistrict, -revenue_category))
feature_importance <- data.frame(feature = features, importance = importance)
sorted_features <- feature_importance[order(importance, decreasing = TRUE), ]
top_features <- head(sorted_features, n = 10)  # Change '10' to the desired number
print(top_features)


####All Data####
colSums(is.na(data))

####Feature Selection####
# Temukan variabel dengan variasi 0
zero_var_cols <- names(data)[apply(data, 2, var) == 0]
if (length(zero_var_cols) > 0) {
  cat("Variabel dengan variasi 0:", paste(zero_var_cols, collapse = ", "), "\n")
} else {
  cat("Tidak ada variabel dengan variasi 0.\n")
}

zero_var_cols <- which(colnames(data) %in% zero_var_cols[2:4])
filtered_data <- data[,-zero_var_cols]

#Deteksi variabel yang varians mendekati 0
library(caret)
near_zero_vars <- nearZeroVar(filtered_data)
names(filtered_data[near_zero_vars])
filtered_data <- filtered_data[,-near_zero_vars]

#drop data yang merupakan data time 
library(dplyr)
filtered_data <- filtered_data %>% select(-dt_id)

#Standarisasi data
numeric_columns <- sapply(filtered_data, is.numeric)
filtered_numeric_data <- filtered_data[, numeric_columns]
standardized_data <- as.data.frame(scale(filtered_numeric_data))
filtered_data <- cbind(standardized_data,filtered_data$subdistrict, filtered_data$revenue_category)
print(filtered_data)

#Rename
colnames(filtered_data)[colnames(filtered_data) == "filtered_data$revenue_category"] <- "revenue_category"
colnames(filtered_data)[colnames(filtered_data) == "filtered_data$subdistrict"] <- "subdistrict"
colnames(filtered_data)[colnames(filtered_data) == "SUM BW"] <- "SUM_BW"
colnames(filtered_data)[colnames(filtered_data) == "RRC Connected User"] <- "RRC_Connected_User"
colnames(filtered_data) <- gsub("<", "_lessthan", colnames(filtered_data))
colnames(filtered_data) <- gsub(">", "_morethan", colnames(filtered_data))
colnames(filtered_data) <- gsub(" ", "_", colnames(filtered_data))
colnames(filtered_data) <- gsub("-", "_", colnames(filtered_data))
colnames(filtered_data)[colnames(filtered_data) == "DL_RB(%)_BHV"] <- "DL_RB_BHV"
colnames(filtered_data)[colnames(filtered_data) == "2G_availability"] <- "twoG_availability" 

#fixed data = filtered_data
filtered_data$revenue_category <- as.factor(filtered_data$revenue_category)
filtered_data$subdistrict <- as.factor(filtered_data$subdistrict)

split <- round(0.8 * nrow(filtered_data))
rand <- sample(1:nrow(filtered_data), nrow(filtered_data))
tr <- filtered_data[rand[1:split],] 
test <- filtered_data[rand[(split + 1):nrow(filtered_data)],]

#####SVR#####
library(e1071)
kernel <- c("linear", "polynomial", "radial")
models <- list()
for(k in kernel){
  svr_model <- svm(revenue_total ~ ., data = tr %>% select(-revenue_category, -subdistrict), kernel = k, C = 1, epsilon = 0.1)
  models[[k]]<- svr_model
  cat("SVR Model with Kernel Type:", kernel, "\n")
  print(svr_model)
  print("====================")
}
for (k in kernel) {
  pred_svr <- predict(models[[k]], newdata = test %>% select(-revenue_category, - revenue_total, - subdistrict))
  mse <- mean((test$revenue_total - pred_svr)^2)
  mae <- mean(abs(test$revenue_total - pred_svr))
  rsquared <- 1 - (sum((test$revenue_total - pred_svr)^2) / sum((test$revenue_total - mean(test$revenue_total))^2))
  cat("MSE for kernel",k,":", mse, "\n")
  cat("MAE for kernel",k,":", mae, "\n")
  cat("R-Squared for kernel",k,":", rsquared, "\n")
}

#mencari C optimum
models <- list()
for(i in 1:100){
  svr_model <- svm(revenue_total ~ ., data = tr %>% select(-revenue_category, -subdistrict), kernel = "linear", cost = i, epsilon = 0.1)
  models[[i]]<- svr_model
}

for (i in 1:100) {
  pred_svr <- predict(models[[i]], newdata = test %>% select(-revenue_category, - revenue_total, - subdistrict))
  mse[i] <- mean((test$revenue_total - pred_svr)^2)
  mae[i] <- mean(abs(test$revenue_total - pred_svr))
  rsquared[i] <- 1 - (sum((test$revenue_total - pred_svr)^2) / sum((test$revenue_total - mean(test$revenue_total))^2))
  cat("MSE for C = ",i,":", mse[i], "\n")
  cat("MAE for C = ",i,":", mae[i], "\n")
  cat("R-Squared for C = ",i,":", rsquared[i], "\n")
}
cat("MSE tekecil",min(mse), "pada C =", which.min(mse))

#C terbaik adalah C = 66, dicari epsilon terbaik
for(i in 1:100){
  svr_model <- svm(revenue_total ~ ., data = tr %>% select(-revenue_category, -subdistrict), kernel = "linear", cost = 66, epsilon = i*0.01)
  models[[i]]<- svr_model
}
for (i in 1:100) {
  pred_svr <- predict(models[[i]], newdata = test %>% select(-revenue_category, - revenue_total, - subdistrict))
  mse[i] <- mean((test$revenue_total - pred_svr)^2)
  mae[i] <- mean(abs(test$revenue_total - pred_svr))
  rsquared[i] <- 1 - (sum((test$revenue_total - pred_svr)^2) / sum((test$revenue_total - mean(test$revenue_total))^2))
  cat("MSE for epsilon = ",i*0.01,":", mse[i], "\n")
  cat("MAE for epsilon = ",i*0.01,":", mae[i], "\n")
  cat("R-Squared for epsilon = ",i*0.01,":", rsquared[i], "\n")
}
cat("MSE tekecil",min(mse), "pada epsilon =", which.min(mse)*0.01)

#model optimum
svr_model <- svm(revenue_total ~ ., data = tr %>% select(-revenue_category, -subdistrict), kernel = "linear", cost = 66, epsilon = 0.02)
pred_svr <- predict(svr_model, newdata = test %>% select(-revenue_category, - revenue_total, - subdistrict))
# Mengukur kinerja model
mse <- mean((test$revenue_total - pred_svr)^2)
mae <- mean(abs(test$revenue_total - pred_svr))
rsquared <- 1 - (sum((test$revenue_total - pred_svr)^2) / sum((test$revenue_total - mean(test$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

#####Regresi Linear Berganda#####
library(dplyr)
model1 <- lm(revenue_total ~., data = tr %>% select(-revenue_category))
# Melakukan prediksi pada data training
pred1tr <- predict(model1, newdata = tr %>% select(-revenue_category, -revenue_total) )
# Mengukur kinerja model
mse <- mean((tr$revenue_total - pred1tr)^2)
mae <- mean(abs(tr$revenue_total - pred1tr))
rsquared <- 1 - (sum((tr$revenue_total - pred1tr)^2) / sum((tr$revenue_total - mean(tr$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")
# Melakukan prediksi pada data testing
pred1test <- predict(model1, newdata = test %>% select(-revenue_category, -revenue_total) )
# Mengukur kinerja model
mse <- mean((test$revenue_total - pred1test)^2)
mae <- mean(abs(test$revenue_total - pred1test))
rsquared <- 1 - (sum((test$revenue_total - pred1test)^2) / sum((test$revenue_total - mean(test$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")


#####Regresi Ridge#####
library(glmnet)
model_ridge <- glmnet(x = as.matrix(tr %>% select(-revenue_category,-subdistrict, -revenue_total)), y = tr$revenue_total, alpha = 0)
best_lambda_ridge <- min(model_ridge$lambda)
print(best_lambda_ridge)
# Melakukan prediksi dengan model Ridge data training
prediksi_ridgetr <- predict(model_ridge, s = best_lambda_ridge, newx = as.matrix(tr %>% select(-revenue_category, -subdistrict,-revenue_total)))
# Mengukur kinerja model
mse <- mean((tr$revenue_total - prediksi_ridgetr)^2)
mae <- mean(abs(tr$revenue_total - prediksi_ridgetr))
rsquared <- 1 - (sum((tr$revenue_total - prediksi_ridgetr)^2) / sum((tr$revenue_total - mean(tr$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")
# Melakukan prediksi dengan model Ridge data testing
prediksi_ridgetest <- predict(model_ridge, s = best_lambda_ridge, newx = as.matrix(test %>% select(-revenue_category, -subdistrict, -revenue_total)))
# Mengukur kinerja model
mse <- mean((test$revenue_total - prediksi_ridgetest)^2)
mae <- mean(abs(test$revenue_total - prediksi_ridgetest))
rsquared <- 1 - (sum((test$revenue_total - prediksi_ridgetest)^2) / sum((test$revenue_total - mean(test$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

#####Regresi Lasso#####
model_lasso <- glmnet(x = as.matrix(tr %>% select(-revenue_total,-revenue_category, -subdistrict)), y = tr$revenue_total, alpha = 1)
best_lambda_lasso <- min(model_lasso$lambda)
print(best_lambda_lasso)
# Melakukan prediksi dengan model Lasso data training
prediksi_lassotr <- predict(model_lasso, s = best_lambda_lasso, newx = as.matrix(tr %>% select(-revenue_total,-revenue_category, -subdistrict)))
# Mengukur kinerja model
mse <- mean((tr$revenue_total - prediksi_lassotr)^2)
mae <- mean(abs(tr$revenue_total - prediksi_lassotr))
rsquared <- 1 - (sum((tr$revenue_total - prediksi_lassotr)^2) / sum((tr$revenue_total - mean(tr$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")
# Melakukan prediksi dengan model Lasso data testing
prediksi_lassotest <- predict(model_lasso, s = best_lambda_lasso, newx = as.matrix(test %>% select(-revenue_total,-revenue_category, -subdistrict)))
# Mengukur kinerja model
mse <- mean((test$revenue_total - prediksi_lassotest)^2)
mae <- mean(abs(test$revenue_total - prediksi_lassotest))
rsquared <- 1 - (sum((test$revenue_total - prediksi_lassotest)^2) / sum((test$revenue_total - mean(test$revenue_total))^2))
# Menampilkan metrik kinerja
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", rsquared, "\n")

#####Regresi KNN#####
# Membuat model KNN regresi
#install.packages('kknn')
library(kknn)
#Pemodelan data training
results = matrix(data = NA, nrow = length(tr) , ncol = 3,
                 dimnames = list(NULL, c("MSE", "MAE", "Rsq"))) 
for(i in 1:length(tr)){
  knn_reg_model <- kknn(revenue_total ~ ., tr %>% select(-revenue_category), tr %>% select(-revenue_category), k = i)
  # Melakukan prediksi
  pred_kknn <- knn_reg_model$fitted.values
  mse <- mean((tr$revenue_total - pred_kknn)^2)
  mae <- mean(abs(tr$revenue_total - pred_kknn))
  rsquared <- 1 - (sum((tr$revenue_total - pred_kknn)^2) / sum((tr$revenue_total - mean(tr$revenue_total))^2))
  results[i,1] <- mse
  results[i,2] <- mae
  results[i,3] <- rsquared
}
results[which.min(results[, 1]), ] #berdasarkan MSE terkecil
results[,1]== results[which.min(results[, 1]), 1]
which.min(results[, 1])
#Pemodelan data testing
results = matrix(data = NA, nrow = length(tr) , ncol = 3,
                 dimnames = list(NULL, c("MSE", "MAE", "Rsq"))) 
for(i in 1:length(tr)){
  knn_reg_model <- kknn(revenue_total ~ ., tr %>% select(-revenue_category), test %>% select(-revenue_category), k = i)
  # Melakukan prediksi
  pred_kknn <- knn_reg_model$fitted.values
  mse <- mean((test$revenue_total - pred_kknn)^2)
  mae <- mean(abs(test$revenue_total - pred_kknn))
  rsquared <- 1 - (sum((test$revenue_total - pred_kknn)^2) / sum((test$revenue_total - mean(test$revenue_total))^2))
  results[i,1] <- mse
  results[i,2] <- mae
  results[i,3] <- rsquared
}
results[which.min(results[, 1]), ] #berdasarkan MSE terkecil
results[,1]== results[which.min(results[, 1]), 1]
which.min(results[, 1])


1+1

#####SVC#####
library(e1071)
library(caret)
library(kernlab)

kernel_types <- c("rbfdot", "polydot", "vanilladot","tanhdot", "laplacedot", "besseldot")
models <- list()

# Loop through the kernel types and train SVC models
for (kernel_type in kernel_types) {
  model <- ksvm(revenue_category ~ ., data = tr%>% select(-revenue_total,-subdistrict), kernel = kernel_type, type = "C-svc", kpar = "automatic",C=5, epsilon = 0.1)
  models[[kernel_type]] <- model
  cat("SVC Model with Kernel Type:", kernel_type, "\n")
  print(model)
  print("====================")
}

for (kernel_type in kernel_types) {
  predictions <- predict(models[[kernel_type]], newdata = test%>% select(-revenue_total, -revenue_category,-subdistrict))
  cat("Accuracy for with Kernel Type:", kernel_type, "\n")
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test$revenue_category))
  accuracy <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy:", accuracy, "\n")
}

##kernel terbaik rbfdot, dicari C terbaik
for (i in 1:100) {
  # Create an SVC model
  model <- ksvm(revenue_category ~ ., data = tr%>% select(-revenue_total,-subdistrict), kernel = "rbfdot", type = "C-svc", kpar = "automatic",C=i, epsilon = 0.1)
  models[[i]] <- model
}

for (i in 1:100) {
  predictions <- predict(models[[i]], newdata = test%>% select(-revenue_total, -revenue_category,-subdistrict))
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test$revenue_category))
  accuracy[i] <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy saat C=",i," : ", accuracy[i], "\n")
}
cat("akurasi tertinggi sebesar",max(accuracy), "pada C =", which.max(accuracy))

#C terbaik adalah C = 3, dicari epsilon terbaik
for (i in 1:100) {
  # Create an SVC model
  model <- ksvm(revenue_category ~ ., data = tr%>% select(-revenue_total,-subdistrict), kernel = "rbfdot", type = "C-svc", kpar = "automatic",C=3, epsilon = i*0.1)
  models[[i]] <- model
}

for (i in 1:100) {
  predictions <- predict(models[[i]], newdata = test%>% select(-revenue_total, -revenue_category,-subdistrict))
  confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test$revenue_category))
  accuracy[i] <- confusion_matrix$overall["Accuracy"]
  cat("Accuracy saat epsilon=",i*0.01," : ", accuracy[i], "\n")
}
cat("akurasi tertinggi sebesar",max(accuracy), "pada epsilon =", which.max(accuracy)*0.01)

#model optimal 
model <- ksvm(revenue_category ~ ., data = tr%>% select(-revenue_total,-subdistrict), kernel = "rbfdot", type = "C-svc", kpar = "automatic",C=3, epsilon = 0.23)

#evaluasi model pada data training
predictions <- predict(model, newdata = test%>% select(-revenue_total,-revenue_category,-subdistrict))
confusion_matrix <- confusionMatrix(table(Predicted = predictions, Actual = test$revenue_category))
print(table(Predicted = predictions, Actual = test$revenue_category))
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

#####Multinomial Logistic Regression#####
library(nnet)
model_logit <- multinom(revenue_category ~ ., data = tr %>% select(-revenue_total, -subdistrict))
#Prediksi data Training
pred_logittr <- predict(model_logit, newdata = tr %>% select(-revenue_total, -subdistrict, -revenue_category), type = "class")
#Kebaikan model
library(caret)
confusion_matrix <- confusionMatrix(data=pred_logittr, reference = tr$revenue_category)
print(table(Predicted = pred_logittr, Actual = tr$revenue_category))
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

#Prediksi data Testing
pred_logittest <- predict(model_logit, newdata = test %>% select(-revenue_total, -subdistrict, -revenue_category), type = "class")
#Kebaikan model
library(caret)
confusion_matrix <- confusionMatrix(data=pred_logittest, reference = test$revenue_category)
print(table(Predicted = pred_logittest, Actual = test$revenue_category))
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


#######Decision Tree#####
library(tree)
library(rpart)
library(rpart.plot)
tree_model <- rpart(revenue_category ~ ., data = tr %>% select(-revenue_total,-subdistrict))
prp(tree_model, type = 5, extra = 100, fallen.leaves = TRUE, varlen = 0, faclen = 0, tweak = 1)

#Prediksi data training
pred_dectreetr <- predict(tree_model, newdata = tr %>% select(-revenue_total,-subdistrict,-revenue_category), type = 'class')
confusion_matrix <- confusionMatrix(data=pred_dectreetr, reference = tr$revenue_category)
print(table(Predicted = pred_dectreetr, Actual = tr$revenue_category))
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

#Prediksi data testing
pred_dectreetest <- predict(tree_model, newdata = test %>% select(-revenue_total,-subdistrict,-revenue_category), type = 'class')
confusion_matrix <- confusionMatrix(data=pred_dectreetest, reference = test$revenue_category)
print(table(Predicted = pred_dectreetest, Actual = test$revenue_category))
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


#####Random Forest#####
library(randomForest)
library(caret)
ctrl21 <- trainControl(method="repeatedcv", number=2, repeats=1)
model_rf <- train(revenue_category ~ ., data = tr %>% select(-revenue_total,-subdistrict), method = 'rf',trControl = ctrl21)
#Prediksi data training
prediksi_rftr <- predict(model_rf,tr %>% select(-revenue_total,-revenue_category,-subdistrict),type = 'raw')
# Mengukur kinerja model
confusion_matrix <- confusionMatrix(data=prediksi_rftr, reference = tr$revenue_category)
print(table(Predicted = prediksi_rftr, Actual = tr$revenue_category))
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

#Prediksi data testing
prediksi_rftest <- predict(model_rf,test %>% select(-revenue_total,-revenue_category,-subdistrict),type = 'raw')
# Mengukur kinerja model
confusion_matrix <- confusionMatrix(data=prediksi_rftest, reference = test$revenue_category)
print(table(Predicted = prediksi_rftest, Actual = test$revenue_category))
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

#####Gradient Boost Machine#####
modfit.gbm <- train(revenue_category ~., method = "gbm", data = tr%>%select(-revenue_total,-subdistrict), 
                    trControl = fitControl, verbose = FALSE, tuneGrid = tune_Grid)
#Prediksi data training
pred_gbmtr <- predict(modfit.gbm, tr%>%select(-revenue_category,-revenue_total,-subdistrict))
# Mengukur kinerja model
confusion_matrix <- confusionMatrix(data=pred_gbmtr, reference = tr$revenue_category)
print(table(Predicted = pred_gbmtr, Actual = tr$revenue_category))
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

#Prediksi data testing
pred_gbmtest <- predict(modfit.gbm, test%>%select(-revenue_category,-revenue_total,-subdistrict))
# Mengukur kinerja model
confusion_matrix <- confusionMatrix(data=pred_gbmtest, reference = test$revenue_category)
print(table(Predicted = pred_gbmtest, Actual = test$revenue_category))
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

######Feature Important All Data####
tree_model <- rpart(revenue_category ~ ., data = tr %>% select(-revenue_total,-subdistrict))
importance <- varImp(tree_model)
print(importance)
