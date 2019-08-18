###############################
# Henry Qiu  ,  Goktug Cengiz #
#    Mehmet Fatih Cagil       #
###############################
#Practice of Decision Trees

#libraries
library(xlsx)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(mlr)
library(ROCR)
library(mice)
library(magrittr)
library(dplyr)
library(missForest)
library(randomForest)
require(gplots)
library(caret)
library(tidyverse)

# 1) Read the Audit.xlsx file and convert it to the csv extension.
xlsxdata = read.xlsx("C:/Users/Henry/Desktop/MASTER/2n Semester/MVA/Homework/H5/audit.xlsx",sheetName = "audit")
write.csv(xlsxdata, "C:/Users/Henry/Desktop/MASTER/2n Semester/MVA/Homework/H5/audit.csv", row.names = FALSE)
#ad = read.csv("C:/Users/Henry/Desktop/MASTER/2n Semester/MVA/Homework/H5/audit.csv",
             # header = TRUE, sep = ",", quote = "\"", dec = ".", row.names = 1, fill = TRUE)
ad = read.csv("/Users/goktugcengiz/Desktop/Datasets/audit.csv",
              header = TRUE, sep = ",", quote = "\"", dec = ".", row.names = 1, fill = TRUE)

# 2) Decide which predictors you would use and eventually preprocess these variables.
str(ad)
summary(ad)

# 6 active categorical variables: "Employment", "Education", "Marital", "Occupation", "Gender", "Accounts"
# 4 active continuous variables:  "Age", "Income", "Deductions", "Hours"
# 1 supplementary continuous variable: "Adjustment"
# 1 supplementary categorical variable: "Adjusted", which is our target/dependent variable

apply(ad, 2, function(x) sum(is.na(x))) #Missing Value Detection
# However, we do not resort to imputation, because decision trees methods are known to be quite robust from the 
# point of view of missings and outliers. 

# Transform the continuous variables into categorical
# Luckyly none of the continuous variables have missing values, we can apply the following method.
ad=transform(ad, Age = ifelse(Age < 28,"17-27",
                              ifelse(Age < 38,"28-37",
                                     ifelse(Age <48,"38-47","48-91"))))

ad=transform(ad, Income = ifelse(Income < 34434,"Low",
                                 ifelse(Income < 59769,"Medium",
                                        ifelse(Income <113843,"High","Very High"))))

ad=transform(ad, Hours = ifelse(Hours < 38,"Part-time",
                                ifelse(Hours <46,"Reduced-time","Full-time")))

# The haven't done the transformation of the variable deduction because the value that it takes vary
# too much between individuals.

# 3) Select the 1/3 of the last observations as test data.
twoThird = floor(nrow(ad)*2/3)
trainData = ad[1:twoThird,]
testData = ad[(twoThird+1):nrow(ad),]


# 4) Obtain the decision tree to predict whether the variable Adjusted on the training data.
# Decide the cutoff value for taking the decision.
tree.audit <- rpart(Adjusted ~ ., 
                    data=trainData[,-11],  # take out also illustrative var. "Adjustment"
                    control=rpart.control(cp=0.001, xval=10),
                    method = "class")
# cp = complexity parameter = min value of alpha, when growing the initial tree
# xval = numbr of CV runs performed on training data
printcp(tree.audit)

# We obtain the optimal tree by pruning the maximal one up to the minimal crossvalidation error.
# Depending on the context, we could take the minimum xerror.
tree.audit$cptable = as.data.frame(tree.audit$cptable)

# Decide the cutoff value
ind = which.min(tree.audit$cptable$xerror) # Index of the minimum xerror
xerr <- tree.audit$cptable$xerror[ind] # Minimum xerror value
xstd <- tree.audit$cptable$xstd[ind] # Xstd value of the element with smaller xerror 
i = 1
while (tree.audit$cptable$xerror[i] > xerr+xstd) i = i+1
alfa = tree.audit$cptable$CP[i]
# Cut the tree acording to the alpha value
tree.optimal <-prune(tree.audit,cp=alfa)
tree.optimal
printcp(tree.optimal)

# 5) Plot the importance of variables in the prediction.
fancyRpartPlot(tree.optimal)
impVar = tree.optimal$variable.importance

varImpPlot <- barplot(impVar,
                      main="Importance of variables",
                      xaxt="n",
                      xlab="Variables",
                      ylab="Importance",
                      las=1)
text(cex=1, x=varImpPlot-.25, y=-10, labels(impVar), xpd=TRUE, srt=45, col = "blue")


# 6) Compute the accuracy, precision, recall and AUC on the test individuals.
# We first do the prediction with the optimal tree model obtained from the training dataset.
prediction = predict(tree.optimal,newdata = testData[,-c(11,12)], type = "class")

# We show the confusion matrix
cm <- confusionMatrix(prediction,
                      factor(testData$Adjusted),
                      positive="1",
                      dnn = c("Prediction", "Reference")
)
cm$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = c("1", "0"))) %>%
  group_by(Reference) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Reference, Freq / total, 0),
    frac = Freq / total * frac_fill
  ) %>%
  mutate(frac_directed = if_else(Prediction == "0", frac_fill * - 1, frac_fill)) %>%
  ggplot(aes(Prediction, Reference, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#badb33") +
  scale_x_discrete(position = "top")

accuracy = cm$overall[1]
precision = floor(cm$table[1,1]) / floor(cm$table[1,1] + cm$table[2,1])
recall = floor(cm$table[1,1]) / floor(cm$table[1,1] + cm$table[1,2])

# Now to get the AUC we need to obtain the probabilities of the predictions of each class
tree.prob = predict(tree.optimal, testData[,-c(11,12)])
positives = tree.prob[,2]
pred = prediction(positives, testData$Adjusted)

# Plot the ROC curve
roc = performance(pred, measure="tpr", x.measure="fpr") #tpr: true positive rate, fpr: false positive rate
plot(roc, col=2, lwd=3, main="Decision Tree ROC curve")
legend(.5,.4, "AUC = 0.8107618")
abline(0,1)

auc = performance(pred, "auc")
auc = as.numeric(auc@y.values)
auc
dd1 = data.frame(FP = roc@x.values[[1]], TP = roc@y.values[[1]])
# 7) Perform Random Forest on the same data. 
# We first need to do imputation of the missing values before using Random Forest to predict.
# We will use miss forest to do the imputation
# We first need to transform all the character variables into factors
dataFac=ad %>% mutate_if(is.character, as.factor)
mfImp = missForest(dataFac);
dataImp <- mfImp$ximp
summary(dataImp)

trainRF = dataImp[1:twoThird,]
testRF = dataImp[(twoThird+1):nrow(dataImp),]
trainRF$Adjusted <- as.factor(trainRF$Adjusted)
testRF$Adjusted <- as.factor(testRF$Adjusted)

#Fit model with best number of predictors
auditRF = randomForest(formula = Adjusted~.,
                       data = trainRF)

Forest.prob = predict(auditRF,
        newdata = testRF,
        type="prob")

auditRF
#Plot variable importance
varImpPlot(auditRF)
# Now to get the AUC we need to obtain the probabilities of the predictions of each class
Forest.prob2 = data.frame(Forest.prob[,2])
Forest.pred = prediction(Forest.prob2, testRF$Adjusted)
Forest.perf = performance(Forest.pred, measure="tpr", x.measure="fpr")
#Plot the Roc Curve
plot(Forest.perf, col=2, lwd=3, main="Random Forest ROC curve")
legend(.5,.4, "AUC = 0.9561669")
abline(0,1)

aucRF = performance(Forest.pred, "auc")@y.values
aucRF
dd2 = data.frame(FP = Forest.perf@x.values[[1]], TP = Forest.perf@y.values[[1]])
#Comparing DT and RF
predictionRF = predict(auditRF, newdata = testRF, type = "class")

# We show the confusion matrix
cf <- confusionMatrix(predictionRF,
                      factor(testRF$Adjusted),
                      positive="1",
                      dnn = c("Prediction", "Reference")
)
cf$table %>%
  data.frame() %>% 
  mutate(Prediction = factor(Prediction, levels = c("1", "0"))) %>%
  group_by(Reference) %>% 
  mutate(
    total = sum(Freq),
    frac_fill = if_else(Prediction == Reference, Freq / total, 0),
    frac = Freq / total * frac_fill
  ) %>%
  mutate(frac_directed = if_else(Prediction == "0", frac_fill * - 1, frac_fill)) %>%
  ggplot(aes(Prediction, Reference, fill = frac_directed)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), size = 8) +
  scale_fill_gradient2(low = "red", mid = "white", high = "#badb33") +
  scale_x_discrete(position = "top")
g = ggplot() + 
  geom_line(data = dd1, aes(x = FP, y = TP, color = 'Decision Tree')) + 
  geom_line(data = dd2, aes(x = FP, y = TP, color = 'Random Forest')) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
  ggtitle('ROC Curve') + 
  labs(x = 'False Positive Rate', y = 'True Positive Rate') 
g + scale_colour_manual(name = 'Classifier', 
                        values = c('Decision Tree'='#E69F00','Random Forest'='#56B4E9'))
