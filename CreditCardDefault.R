# Load libraries
library(xlsx)
library(tidyverse)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)
library(glmnet)
library(gbm)
library(ROCR)

# Load in data
df_org <- read.xlsx("default of credit card clients.xls", startRow = 2, sheetIndex = 1)
df_org$default.payment.next.month<- as.factor(df_org$default.payment.next.month)
names(df_org)[ncol(df_org)] <- "DEFAULT"

# Set plotting parameters
par(mfrow = c(2, 3), mar = c(4, 3, 1, 1), cex.lab = 0.8, cex.axis = 0.65, mgp = c(1.1, 0.1, 0),
    tcl = -0.2)

# Plot default rates versus sex
tab1 <- prop.table(table(df_org$DEFAULT, df_org$SEX), margin = 2)
barplot(tab1, names.arg = c("Male", "Female"), ylim = c(0, 1), axis.lty = 1,
        ylab = "Proportion of Observations", col = c("blue", "red"))
text(paste0("n = ", table(df_org$SEX)), x = c(0.7, 1.9), y = rep(0.05, 2), col = "white",
     adj = 0.5, cex = 0.7)
box(which = "plot")

# Plot default rates versus education
tab2 <- prop.table(table(df_org$DEFAULT[df_org$EDUCATION %in% 1:3],
                         df_org$EDUCATION[df_org$EDUCATION %in% 1:3]), margin = 2)
barplot(tab2, names.arg = c("Graduate", "University", "HS"), ylim = c(0, 1), axis.lty = 1,
        ylab = "Proportion of Observations", col = c("blue", "red"))
text(paste0("n = ", table(df_org$EDUCATION)[2:4]), x = c(0.7, 1.9, 3.1), y = rep(0.05, 3),
     col = "white", adj = 0.5, cex = 0.7)
box(which = "plot")

# Plot default rates versus marital status
tab3 <- prop.table(table(df_org$DEFAULT[df_org$MARRIAGE %in% 1:3],
                         df_org$MARRIAGE[df_org$MARRIAGE %in% 1:3]), margin = 2)
barplot(tab3, names.arg = c("Married", "Single", "Other"), ylim = c(0, 1), axis.lty = 1,
        ylab = "Proportion of Observations", col = c("blue", "red"))
text(paste0("n = ", table(df_org$MARRIAGE)[2:4]), x = c(0.7, 1.9, 3.1), y = rep(0.05, 3),
     col = "white", adj = 0.5, cex = 0.7)
box(which = "plot")

# Plot default rates versus age
plot(density(df_org$AGE[df_org$DEFAULT == 0], from = 0, to = 80), lwd = 2,
     col = "blue", xlab = "Age (Years)", ylab = "Probability Density", main = "")
lines(density(df_org$AGE[df_org$DEFAULT == 1], from = 0, to = 80), lwd = 2,
      col = "red")

# Plot default rates versus credit limit
plot(density(df_org$LIMIT_BAL[df_org$DEFAULT == 0]/1000, from = 0, to = 1000), lwd = 2,
     col = "blue", ylim = c(0, 0.008), xlab = "Credit Limit (TWD x 1000)",
     ylab = "Probability Density", main = "")
lines(density(df_org$LIMIT_BAL[df_org$DEFAULT == 1]/1000, from = 0, to = 1000),
      lwd = 2, col = "red")

# Plot overall default frequencies
barplot(table(df_org$DEFAULT), names.arg = c("No Default", "Default"), axis.lty = 1,
        ylim = c(0, 25000), ylab = "Total Observations", legend.text = c("No Default", "Default"),
        col = c("blue", "red"), args.legend = list(x = 2.4, y = 24000, bty = "n", cex = 0.8))
box(which = "plot")

# Clear variables to free up memory
remove(tab1, tab2, tab3)

# Set plotting parameters
par(mfrow = c(1, 3), mar = c(4, 3, 1, 1), cex.lab = 0.8, cex.axis = 0.65, mgp = c(1.1, 0.1, 0),
    tcl = -0.2)

# Plot default rates versus most recent statement amount
plot(density(df_org$PAY_AMT1[df_org$DEFAULT == 0]/1000, from = 0, to = 100), lwd = 2,
     col = "blue", xlim = c(0, 60), ylim = c(0, 0.4), xlab = " Current Statement (TWD x 1000)",
     ylab = "Probability Density", main = "")
lines(density(df_org$PAY_AMT1[df_org$DEFAULT == 1]/1000, from = 0, to = 100),
      lwd = 2, col = "red")

# Plot default rates versus most recent payment amount
plot(density(df_org$BILL_AMT1[df_org$DEFAULT == 0]/1000, from = 0, to = 200), lwd = 2,
     col = "blue", xlim = c(0, 200), ylim = c(0, 0.025), xlab = "Previous Payment (TWD x 1000)",
     ylab = "Probability Density", main = "")
lines(density(df_org$BILL_AMT1[df_org$DEFAULT == 1]/1000, from = 0, to = 200),
      lwd = 2, col = "red")

# Plot default rates versus delinquency status
hist(df_org$PAY_0[df_org$DEFAULT == 0], freq = FALSE, breaks = -2:8, right = FALSE, col = "blue",
     density = 30, angle = 45, xlim = c(-2, 8), ylim = c(0, 0.6), xlab = "Delinquency Status",
     ylab = "Proportional Frequency", main = "")
hist(df_org$PAY_0[df_org$DEFAULT == 1], freq = FALSE, breaks = -2:8, right = FALSE,
     col = "red", density = 30, angle = 315, xlim = c(-2, 8), main = "", add = TRUE)
legend(x = 3.9, y = 0.58, c("No Default", "Default"), bty = "n", cex = 0.8, fill = c("blue", "red"))
box(which = "plot")

# Correlations for statement, payment, and delinquency (within group)
cor(df_org[, 7:12])
cor(df_org[, 13:18])
cor(df_org[, 19:24])

# Correlations for statement, payment, and delinquency (across group)
cor(df_org[, 7:24])

# Kolmogorov-Smirnov test for relevant curves in figures below
ks.test(df_org$AGE[df_org$DEFAULT == 0],
        df_org$AGE[df_org$DEFAULT == 1])
ks.test(df_org$LIMIT_BAL[df_org$DEFAULT == 0]/1000,
        df_org$LIMIT_BAL[df_org$DEFAULT == 1]/1000)
ks.test(df_org$PAY_AMT1[df_org$DEFAULT == 0]/1000,
        df_org$PAY_AMT1[df_org$DEFAULT == 1]/1000)
ks.test(df_org$BILL_AMT1[df_org$DEFAULT == 0]/1000,
        df_org$BILL_AMT1[df_org$DEFAULT == 1]/1000)

# Perform PCA on non-categorical data
pc.out <- prcomp(df_org[, c(2, 6:24)], scale = TRUE)
pc.pve <- 100*pc.out$sdev^2/sum(pc.out$sdev^2)

# Generate Scree plot
# First 10 PCs make up 90% PVE
par(mfrow = c(1, 2))
plot(pc.pve, type = "o", ylab = "PVE", xlab = "Principal Component")
plot(cumsum(pc.pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component")

# Create new dataframe with categoricals and first 10 PCs
df_new <- data.frame(cbind(df_org[, c(1, 3:5)], pc.out$x[, 1:10], df_org$DEFAULT))
names(df_new)[ncol(df_new)] <- "DEFAULT"

# Convert categorical variables to factors
df_new$SEX <- as.factor(df_new$SEX)
df_new$MARRIAGE <- as.factor(df_new$MARRIAGE)
df_new$EDUCATION <- as.factor(df_new$EDUCATION)

# Set seed for RNG
set.seed(347890245)

# Split data into training and test set
df_new %>%
  select(ID, everything()) %>% 
  group_by(DEFAULT) %>% 
  sample_frac(0.75) -> df_train
df_test <- suppressMessages(anti_join(df_new, df_train))

# Clear variables to free up memory
remove(pc.out, pc.pve)

# Set seed for RNG
set.seed(219333500)

# Fit full model (model 1)
model1 <- glm(DEFAULT ~ . - ID, family = "binomial", data = df_train)
glmProbs1 <- predict(model1, df_train, type = "response")
glmPred1 <- rep(0, nrow(df_train))
glmPred1[glmProbs1 > 0.5] <- 1
glmTab1 <- table(glmPred1, df_train$DEFAULT)
sum(diag(glmTab1))/sum(glmTab1)

# Reduce model using backward selection to minimise AIC (model 2)
model2 <- step(model1)
glmProbs2 <- predict(model2, df_train, type = "response")
glmPred2 <- rep(0, nrow(df_train))
glmPred2[glmProbs2 > 0.5] <- 1
glmTab2 <- table(glmPred2, df_train$DEFAULT)
sum(diag(glmTab2))/sum(glmTab2)

# Reduce model using ridge shrinkage (model 3)
mMat <- model.matrix(model1$formula, data = df_train)
model3 <- cv.glmnet(mMat, df_train$DEFAULT, alpha = 0, family = "binomial", type.measure = "class")
glmProbs3 <- predict(model3, model3$lambda.min, newx = mMat, type = "response")
glmPred3 <- rep(0, nrow(df_train))
glmPred3[glmProbs3 > 0.5] <- 1
glmTab3 <- table(glmPred3, df_train$DEFAULT)
sum(diag(glmTab3))/sum(glmTab3)

# Reduce model using LASSO shrinkage (model 4)
model4 <- cv.glmnet(mMat, df_train$DEFAULT, alpha = 1, family = "binomial", type.measure = "class")
glmProbs4 <- predict(model4, model4$lambda.min, newx = mMat, type = "response")
glmPred4 <- rep(0, nrow(df_train))
glmPred4[glmProbs4 > 0.5] <- 1
glmTab4 <- table(glmPred4, df_train$DEFAULT)
sum(diag(glmTab4))/sum(glmTab4)

# Evaluate best model (model 1) on test data
glmProbsT <- predict(model1, df_test, type = "response")
glmPredT <- rep(0, nrow(df_test))
glmPredT[glmProbsT > 0.5] <- 1
glmTabT <- table(glmPredT, df_test$DEFAULT)

# Get training and test scores for ROC curves
glmScoreTrain <- glmProbs1
glmScoreTest <- glmProbsT

# Get training/test metrics to place in table
glmMetTrain <- paste0(sprintf(c(sum(diag(glmTab1))/sum(glmTab1)*100,
                                glmTab1[1, 1]/sum(glmTab1[, 1])*100,
                                glmTab1[2, 2]/sum(glmTab1[, 2])*100),
                              fmt = "%#.2f"), "%")
glmMetTest <- paste0(sprintf(c(sum(diag(glmTabT))/sum(glmTabT)*100,
                               glmTabT[1, 1]/sum(glmTabT[, 1])*100,
                               glmTabT[2, 2]/sum(glmTabT[, 2])*100),
                             fmt = "%#.2f"), "%")

# Plot model residuals
index <- sample(1:length(glmProbs1), size = 2000)
plot(glmProbs1, rstandard(model1))
abline(h = 0, lty = 2, col = "red")

# Clear variables to free up memory
remove(mMat, model1, model2, model3, model4, glmPred1, glmPred2, glmPred3, glmPred4, glmPredT,
       glmProbs1, glmProbs2, glmProbs3, glmProbs4, glmProbsT, glmTab1, glmTab2, glmTab3, glmTab4,
       glmTabT, index)

# Set seed for RNG
set.seed(183750348)

# Fit bagged tree with 500 trees (model 1)
model1 <- randomForest(DEFAULT ~ . - ID, data = df_train, ntree = 500,
                       mtry = 13, importance = TRUE)
dtmPred1 <- predict(model1, df_train)
dtmTab1 <- table(dtmPred1, df_train$DEFAULT)
sum(diag(dtmTab1))/sum(dtmTab1)

# Fit bagged tree with 1000 trees (model 2)
model2 <- randomForest(DEFAULT ~ . - ID, data = df_train, ntree = 1000,
                       mtry = 13, importance = TRUE)
dtmPred2 <- predict(model2, df_train)
dtmTab2 <- table(dtmPred2, df_train$DEFAULT)
sum(diag(dtmTab2))/sum(dtmTab2)

# Fit random forest with 500 trees (model 3)
model3 <- randomForest(DEFAULT ~ . - ID, data = df_train, ntree = 500,
                       mtry = 4, importance = TRUE)
dtmPred3 <- predict(model3, df_train)
dtmTab3 <- table(dtmPred3, df_train$DEFAULT)
sum(diag(dtmTab3))/sum(dtmTab3)

# Fit random forest with 1000 trees (model 3)
model4 <- randomForest(DEFAULT ~ . - ID, data = df_train, ntree = 1000,
                       mtry = 4, importance = TRUE)
dtmPred4 <- predict(model4, df_train)
dtmTab4 <- table(dtmPred4, df_train$DEFAULT)
sum(diag(dtmTab4))/sum(dtmTab4)

# Evaluate best model (model 1) on test data
dtmPredT <- predict(model1, df_test) 
dtmTabT <- table(dtmPredT, df_test$DEFAULT)

# Get training and test scores for ROC curves
dtmScoreTrain <- predict(model1, df_train, type = "prob")[, 1]
dtmScoreTest <- predict(model1, df_test, type = "prob")[, 1]

# Get training/test metrics to place in table
dtmMetTrain <- paste0(sprintf(c(sum(diag(dtmTab1))/sum(dtmTab1)*100,
                                dtmTab1[1, 1]/sum(dtmTab1[, 1])*100,
                                dtmTab1[2, 2]/sum(dtmTab1[, 2])*100),
                              fmt = "%#.2f"), "%")
dtmMetTest <- paste0(sprintf(c(sum(diag(dtmTabT))/sum(dtmTabT)*100,
                               dtmTabT[1, 1]/sum(dtmTabT[, 1])*100,
                               dtmTabT[2, 2]/sum(dtmTabT[, 2])*100),
                             fmt = "%#.2f"), "%")

# Clear variables to free up memory
remove(model1, model2, model3, model4, dtmPred1, dtmPred2, dtmPred3, dtmPred4, dtmPredT,
       dtmTab1, dtmTab2, dtmTab3, dtmTab4, dtmTabT)

# Set seed for RNG
set.seed(500073918)

# Convert response to character, otherwise gbm won't work
df_train$DEFAULT <- as.character(df_train$DEFAULT)
df_test$DEFAULT <- as.character(df_test$DEFAULT)

# Fit boosted tree models with varying shrinkage parameter (0.1 - 1.0)
for(i in 1:10){
  model <- gbm(DEFAULT ~ . - ID, data = df_train, distribution = "bernoulli",
               n.trees = 1000, interaction.depth = 3, shrinkage = i/10)
  assign(paste0("model", i), model)
  gbmProbs = predict(model, newdata = df_train, n.trees = 1000, type = "response")
  gbmPred <- rep(0, nrow(df_train))
  gbmPred[gbmProbs > 0.5] <- 1
  gbmTab <- table(gbmPred, df_train$DEFAULT)
  print(sum(diag(gbmTab))/sum(gbmTab))}

# Get training and test metrics for best model (model 7)
gbmProbs7 <- predict(model7, newdata = df_train, n.trees = 1000, type = "response")
gbmPred7 <- rep(0, nrow(df_train))
gbmPred7[gbmProbs7 > 0.5] <- 1
gbmTab7 <- table(gbmPred7, df_train$DEFAULT)
gbmProbsT <- predict(model6, newdata = df_test, n.trees = 1000, type = "response")
gbmPredT <- rep(0, nrow(df_test))
gbmPredT[gbmProbsT > 0.5] <- 1
gbmTabT <- table(gbmPredT, df_test$DEFAULT)

# Get training and test scores for ROC curves
gbmScoreTrain <- gbmProbs7 
gbmScoreTest <- gbmProbsT

# Get training/test metrics to place in table
gbmMetTrain <- paste0(sprintf(c(sum(diag(gbmTab7))/sum(gbmTab7)*100,
                                gbmTab7[1, 1]/sum(gbmTab7[, 1])*100,
                                gbmTab7[2, 2]/sum(gbmTab7[, 2])*100),
                              fmt = "%#.2f"), "%")
gbmMetTest <- paste0(sprintf(c(sum(diag(gbmTabT))/sum(gbmTabT)*100,
                               gbmTabT[1, 1]/sum(gbmTabT[, 1])*100,
                               gbmTabT[2, 2]/sum(gbmTabT[, 2])*100),
                             fmt = "%#.2f"), "%")

# Convert response back to factor
df_train$DEFAULT <- as.factor(df_train$DEFAULT)
df_test$DEFAULT <- as.factor(df_test$DEFAULT)

# Clear variables to free up memory
remove(model, model1, model2, model3, model4, model5, model6, model7, model8, model9, model10,
       gbmPred, gbmPred7, gbmPredT, gbmProbs, gbmProbs7, gbmProbsT, gbmTab, gbmTab7, gbmTabT, 
       i)

# Set seed for RNG
set.seed(384507774)

# Fit SVM with linear kernel (model 1)
model1 <- tune(svm, DEFAULT ~ . - ID, data = df_train, kernel = "linear",
               decision.values = TRUE)
svmPred1 <- predict(model1$best.model, df_train, type = "response") 
svmTab1 <- table(svmPred1, df_train$DEFAULT)
sum(diag(svmTab1))/sum(svmTab1)

# Fit SVM with cubic kernel (model 2)
model2 <- tune(svm, DEFAULT ~ . - ID, data = df_train, kernel = "polynomial",
               degree = 3, decision.values = TRUE)
svmPred2 <- predict(model2$best.model, df_train, type = "response") 
svmTab2 <- table(svmPred2, df_train$DEFAULT)
sum(diag(svmTab2))/sum(svmTab2)

# Fit SVM with radial kernel (model 3)
model3 <- tune(svm, DEFAULT ~ . - ID, data = df_train, kernel = "radial",
               decision.values = TRUE)
svmPred3 <- predict(model3$best.model, df_train, type = "response") 
svmTab3 <- table(svmPred3, df_train$DEFAULT)
sum(diag(svmTab3))/sum(svmTab3)

# Evaluate best model (model 3) on test data
svmPredT <- predict(model3$best.model, df_test, type = "response")  
svmTabT <- table(svmPredT, df_test$DEFAULT)

# Get training and test scores for ROC curves
svmScoreTrain <- attributes(predict(model3$best.model, df_train, decision.values = TRUE))$decision.values
svmScoreTest <- attributes(predict(model3$best.model, df_test, decision.values = TRUE))$decision.values

# Get training/test metrics to place in table
svmMetTrain <- paste0(sprintf(c(sum(diag(svmTab3))/sum(svmTab3)*100,
                                svmTab3[1, 1]/sum(svmTab3[, 1])*100,
                                svmTab3[2, 2]/sum(svmTab3[, 2])*100),
                              fmt = "%#.2f"), "%")
svmMetTest <- paste0(sprintf(c(sum(diag(svmTabT))/sum(svmTabT)*100,
                               svmTabT[1, 1]/sum(svmTabT[, 1])*100,
                               svmTabT[2, 2]/sum(svmTabT[, 2])*100),
                             fmt = "%#.2f"), "%")

# Clear variables to free up memory
remove(model1, model2, model3, svmPred1, svmPred2, svmPred3, svmPredT, svmTab1, svmTab2,
       svmTab3, svmTabT)

# Plot ROC curve; code adapted from ISLR
# Need to flip curves for some models since labels get flipped
rocplot <- function(pred, truth, mType, ...){
  if(mType %in% c("glm", "gbm")){
    predob <- prediction(pred, truth)}
  if(mType %in% c("dtm", "svm")){
    predob <- prediction(pred, truth, c(1, 0))}
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, xlab = "1 - Specificity", ylab = "Sensitivity", ...)
  abline(0, 1, lty = 2)}

# Set plotting parameters
par(mfrow = c(2, 4), mar = c(4, 3, 1, 1), cex.lab = 0.8, cex.axis = 0.65, mgp = c(1.1, 0.1, 0),
    tcl = -0.2)

# Plot ROC curves for training data
rocplot(glmScoreTrain, df_train$DEFAULT, "glm")
text(x = 1, y = 0.05, "LR Train", adj = 1)
rocplot(dtmScoreTrain, df_train$DEFAULT, "dtm")
text(x = 1, y = 0.05, "BG Train", adj = 1)
rocplot(gbmScoreTrain, df_train$DEFAULT, "gbm")
text(x = 1, y = 0.05, "BT Train", adj = 1)
rocplot(svmScoreTrain, df_train$DEFAULT, "svm")
text(x = 1, y = 0.05, "SV Train", adj = 1)

# Plot ROC curves for test data
rocplot(glmScoreTest, df_test$DEFAULT, "glm")
text(x = 1, y = 0.05, "LR Test", adj = 1)
rocplot(dtmScoreTest, df_test$DEFAULT, "dtm")
text(x = 1, y = 0.05, "BG Test", adj = 1)
rocplot(gbmScoreTest, df_test$DEFAULT, "gbm")
text(x = 1, y = 0.05, "BT Test", adj = 1)
rocplot(svmScoreTest, df_test$DEFAULT, "svm")
text(x = 1, y = 0.05, "SV Test", adj = 1)

# Create table for training metrics
df1 <- data.frame(glmMetTrain, dtmMetTrain, gbmMetTrain, svmMetTrain)
colnames(df1) <- c("LR", "BG", "BT", "SV")
rownames(df1) <- c("Accuracy", "Sensitivity", "Specificity")

# Create table for test metrics
df2 <- data.frame(glmMetTest, dtmMetTest, gbmMetTest, svmMetTest)
colnames(df2) <- c("LR", "BG", "BT", "SV")
rownames(df2) <- c("Accuracy", "Sensitivity", "Specificity")
