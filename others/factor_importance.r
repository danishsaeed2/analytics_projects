library(randomForest)
library(readxl)
library(e1071)
library(caret)

data <- read_excel("Dataset.xlsx")

dim(data)
head(data)

data = subset(data, select = -c(Index,compensation))

data <- transform(
	data,
	Gender = as.factor(Gender),
	ssc_board = as.factor(ssc_board),
	hsc_board = as.factor(hsc_board),
	hsc_stream = as.factor(hsc_stream),
	degree_domain = as.factor(degree_domain),
	work_experience = as.factor(work_experience),
	specialization = as.factor(specialization),
	status = as.factor(status)
)

data_nomi = subset(data, select = -c(ssc_.,hsc_.,degree_.,entrance_score,mba_.))
sapply(data_nomi, class)

data_cont = subset(data, select = c(ssc_.,hsc_.,degree_.,entrance_score,mba_.,status))
sapply(data_cont, class)

summary(data_nomi)
summary(data_cont)

rf_nomi <- randomForest(status ~ ., data=data_nomi, ntree=1000)
rf_nomi$confusion[, 'class.error']
importance(rf_nomi, type=2)

rf_cont <- randomForest(status ~ ., data=data_cont, ntree=1000)
rf_cont$confusion[, 'class.error']
importance(rf_cont, type=2)

nb <- naiveBayes(status ~ ., data=data)
nb

svm_classifier <- svm(status ~ ., data=data)
summary(svm_classifier)
confusionMatrix(data$status, predict(svm_classifier))
