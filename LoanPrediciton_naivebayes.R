read.csv('C:\\Users\\SV077490\\OneDrive - Cerner Corporation\\Desktop\\D-O\\Projects-R\\Loan Prediction\\cutomer_loan.csv')-> loan

#missing values
sum(is.na(loan))

loan$dti <- (loan$debts / loan$income)
loan$loan_decision_status <- ifelse(loan$loan_decision_type == "Denied",0,1)

loan$loan_decision_status <- as.factor(loan$loan_decision_status)

customer_loan_refined <- loan[,c(3,4,6,7,8,11,13,14)]

#encode variables

customer_loan_refined$gender <- as.numeric(factor(customer_loan_refined$gender,
                                                  levels = c('Male','Female'),
                                                  labels = c(1,2)))
customer_loan_refined$marital_status <- as.numeric(factor(customer_loan_refined$marital_status,
                                                          levels = c('Divorced','Married','Single'),
                                                          labels = c(1,2,3)))

customer_loan_refined$occupation <- as.numeric(factor(customer_loan_refined$occupation,
                                                      levels = c('Accout','Business','IT','Manager','NYPD'),
                                                      labels = c(1,2,3,4,5)))

customer_loan_refined$loan_type <- as.numeric(factor(customer_loan_refined$loan_type,
                                                     levels = c('Auto','Credit','Home','Personal'),
                                                     labels = c(1,2,3,4)))

#Model Building:
library(caTools)
split = sample.split(customer_loan_refined$loan_decision_status, SplitRatio = 0.70)
training = subset(customer_loan_refined, split == TRUE)
test = subset(customer_loan_refined, split == FALSE)

#Feature scaling
training[,1:7] = scale(training[,1:7])
test[,1:7] = scale(test[,1:7])
View(training)
head(training)


#PCA

library(caret)
pca = preProcess(x = training[-8], method = 'pca', pcaComp = 2)
training_pca = predict(pca, training)
training_set_pca = training_pca[c(2, 3, 1)]
test_pca = predict(pca, test)
test_set_pca = test_pca[c(2, 3, 1)]
head(test_set_pca)

#naive Bayes
install.packages("e1071")
library(e1071)
classifier = naiveBayes(x = training_set_pca[-3], y = training_set_pca$loan_decision_status)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set_pca[-3])

# confusionMatrix to calculate accuracy
confusionMatrix(table(test_set_pca[, 3], y_pred))
