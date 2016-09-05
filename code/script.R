#library used
library(xgboost)
library(mice)



train <- read.csv("train.csv")
test <- read.csv("test.csv")

############################ training DataSet ############################
#convert into interger
x <- train$mail_category
x <- gsub('mail_category_','',x)
x <- as.integer(x)
train$mail_category <- x

#convert into interger
x <- gsub('mail_type_','',train$mail_type)
x <- as.integer(x)
train$mail_type <- x

#convert into interger
x <- train$hacker_confirmation
x <- as.integer(x)
x <- x-1
x <- as.integer(x)
train$hacker_confirmation <- x

#convert into interger
x <- train$mail_id
x <- as.integer(x)
train$mail_id <- x

#convert into interger
x <- train$user_id
x <- as.integer(x)
train$user_id <- x

#convert into interger
x <- train$opened
x <- as.integer(x)
x <- x-1
x <- as.integer(x)
train$opened <- x

#convert into interger
x <- train$unsubscribed
x <- as.integer(x)
x <- x-1
x <- as.integer(x)
train$unsubscribed <- x

#convert into interger
x <- train$clicked
x <- as.integer(x)
x <- x-1
x <- as.integer(x)
train$clicked <- x

 
#Removing
drops <- c("hacker_timezone","click_time","open_time","unsubscribe_time")
train <- train[,!(names(train) %in% drops)]
train <- na.omit(train)

#target label
target <- train$opened

train$opened <- NULL

#covert into numeric
train <- sapply(train,as.numeric)


############################ testing DataSet ############################

#Removing hacker_timezone
test$hacker_timezone <- NULL

#convert into interger
x <- test$mail_category
x <- gsub('mail_category_','',x)
x <- as.integer(x)
test$mail_category <- x

#convert into interger
x <- gsub('mail_type_','',test$mail_type)
x <- as.integer(x)
test$mail_type <- x

#convert into interger
x <- test$hacker_confirmation
x <- as.integer(x)
x <- x-1
x <- as.integer(x)
test$hacker_confirmation <- x

#convert into interger
x <- test$mail_id
x <- as.integer(x)
test$mail_id <- x

#convert into interger
x <- test$user_id
x <- as.integer(x)
test$user_id <- x

#Missing Value imputation 
simple <- test[c("mail_type","mail_category","last_online")]
simple$last_online[is.na(simple$last_online)] <- median(simple$last_online,na.rm = T)

simple$mail_type=as.numeric(simple$mail_type)
simple$mail_type[is.na(simple$mail_type)]=median(simple$mail_type,na.rm = T)
simple$mail_category=as.numeric(simple$mail_category)
simple$mail_category[is.na(simple$mail_category)]=median(simple$mail_category,na.rm = T)

test$mail_type <- simple$mail_type
test$mail_category <- simple$mail_category
test$last_online <- simple$last_online

#covert into numeric
test <- sapply(test,as.numeric)

############################ Modeling ############################

#xgboot model
bstDense <- xgboost(data = (train), label = as.numeric(target), max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")

#predict
pred <- predict(bstDense, test)

prediction <- as.numeric(pred > 0.5)

#write the output
write.table(prediction, file="prediction.csv", row.names=FALSE, col.names=FALSE, sep=",")


