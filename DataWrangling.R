# Import the relavant packages
library(jsonlite)     #Importing JSON Files
library(glmnet)       #Building Linear Models
library(ggplot2)      #Plotting Graphs
library(tidyverse)    #Manipulating the Data
library(sandwich)     #Robust Standard Errors
library(lmtest)       #Testing the Linear Model
library(caret)        #Splitting Data into Train and Test
library(tree)         #Decision Trees
library(rpart)        #Decision Trees
library(rpart.plot)   #Plotting Decision Trees
library(ipred)        #Bagging
library(adabag)       #Boosting
library(randomForest) #Random Forest
library(psych)        #PCA

# Clear the R Workspace
cat("\014")  
rm(list=ls())
gc()

# Set Working Directory
setwd("D:/Documents/EC349_Project")

# Load in json files and save them as R datasets
business_data <- stream_in(file("Data/yelp_academic_dataset_business.json"))
saveRDS(business_data, file = "business_data.rds")

checkin_data <- stream_in(file("Data/yelp_academic_dataset_checkin.json"))
saveRDS(checkin_data, file = "checkin_data.rds")

tip_data <- stream_in(file("Data/yelp_academic_dataset_tip.json"))
saveRDS(tip_data, file = "tip_data.rds")

#Load in datasets
tip_data <- readRDS("Data/tip_data.rds")

checkin_data <- readRDS("Data/checkin_data.rds")

business_data <- readRDS("Data/business_data.rds")

load("Data/yelp_review_small.Rda")

load("Data/yelp_user_small.Rda")

##Explore the data
#Business Data:
names(business_data)
dim(business_data)
head(business_data)
(colSums(!is.na(business_data))/nrow(business_data))*100
business_data[c('name','address','postal_code','attributes','hours','categories')] <- list(NULL)
names(business_data) <- c('business_id','city','state','latitude','longitude','business_stars','business_review_count','is_open')
(colSums(!is.na(business_data))/nrow(business_data))*100
#business_data <- unnest(business_data,attributes)
#business_data %>% rowwise() %>% mutate(cat_list = list(strsplit(categories, ",")[[1]]))
#unique(unlist(business_data$categories))

#Checkin Data:
names(checkin_data)
head(checkin_data)
checkin_data <- checkin_data %>% mutate(checkin_number = 1 + str_count(date, ","))
checkin_data[c('date')] <- list(NULL)

#Review Data:
names(review_data_small)
dim(review_data_small)
summary(review_data_small)
review_data_small[c('text')] <- list(NULL)
review_data_small <- review_data_small %>% separate(date,c('year','month','day'))
review_data_small$year <- as.numeric(review_data_small$year)
review_data_small$month <- as.numeric(review_data_small$month)
review_data_small$day <- as.numeric(review_data_small$day)
(colSums(!is.na(review_data_small))/nrow(review_data_small))*100

#Tip Data
names(tip_data)
dim(tip_data)
head(tip_data)
tip_data %>% summary(compliment_count)
tip_data[c('text','date')] <- list(NULL) #Remove text column
tip_data <- tip_data %>% mutate(tip = 1)
names(tip_data) <- c('user_id','business_id','tip_compliments','tip')
#tip_data <- tip_data %>% separate(date,c('tip_year','tip_month','tip_day'))
#tip_data$tip_year <- as.numeric(tip_data$tip_year)
#tip_data$tip_month <- as.numeric(tip_data$tip_month)
#tip_data$tip_day <- as.numeric(tip_data$tip_day)
#tip_data[c('tip_year','tip_month','tip_day')] <- list(NULL)

#User Data
names(user_data_small)
dim(user_data_small)
summary(user_data_small)
user_data_small[c('name','friends','elite')] <- list(NULL)
user_data_small <- user_data_small %>% separate('yelping_since',c('yelping_since_year'))
user_data_small$yelping_since_year <- as.numeric(user_data_small$yelping_since_year)
names(user_data_small) <- c('user_id','user_review_count','yelping_since','user_useful','user_funny','user_cool','user_fans','user_stars','compliment_hot','compliment_more','compliment_profile','compliment_cute','compliment_list','compliment_note','compliment_plain','compliment_cool','compliment_funny','compliment_writer','compliment_photos')
(colSums(!is.na(user_data_small))/nrow(user_data_small))*100


#Join the two datasets together
review_data_small <- left_join(review_data_small,user_data_small,'user_id')
rm(user_data_small)
review_data_small <- left_join(review_data_small,business_data,'business_id')
rm(business_data)
review_data_small <- left_join(review_data_small,tip_data,by = c('business_id' = 'business_id','user_id'='user_id'))
rm(tip_data)
review_data_small <- left_join(review_data_small,checkin_data,'business_id')
rm(checkin_data)
(colSums(!is.na(review_data_small))/nrow(review_data_small))*100

#Remove the IDs to make modelling easier
review_data_small[c('review_id','user_id','business_id')] <- list(NULL)

#Split the data into train and test
set.seed(1)
parts = createDataPartition(review_data_small$stars, p = 1-(10002/nrow(review_data_small)), list = F)
train = review_data_small[parts, ]
test = review_data_small[-parts, ]
rm(parts)
rm(review_data_small)

(colSums(!is.na(train))/nrow(train))*100
#Sort out the missing values:
train %>% summary()
#User Review Count
ggplot(train, aes(user_review_count)) + geom_histogram()
train <- train %>% mutate(user_review_count = ifelse(is.na(user_review_count), 0, user_review_count))
ggplot(train, aes(user_review_count)) + geom_histogram
#Yelping Since
ggplot(train, aes(yelping_since)) + geom_histogram()
train <- train %>% mutate(yelping_since = ifelse(is.na(yelping_since), year, yelping_since))
ggplot(train, aes(yelping_since)) + geom_histogram()
#User Useful
ggplot(train, aes(user_useful)) + geom_histogram()
train <- train %>% mutate(user_useful = ifelse(is.na(user_useful), 0, user_useful),
                          user_funny = ifelse(is.na(user_funny), 0, user_funny),
                          user_cool = ifelse(is.na(user_cool), 0, user_cool),
                          user_fans = ifelse(is.na(user_fans), 0, user_fans),
                          compliment_hot = ifelse(is.na(compliment_hot), 0, compliment_hot),
                          compliment_more = ifelse(is.na(compliment_more), 0, compliment_more),
                          compliment_profile = ifelse(is.na(compliment_profile), 0, compliment_profile),
                          compliment_cute = ifelse(is.na(compliment_cute), 0, compliment_cute),
                          compliment_list = ifelse(is.na(compliment_list), 0, compliment_list),
                          compliment_note = ifelse(is.na(compliment_note), 0, compliment_note),
                          compliment_plain = ifelse(is.na(compliment_plain), 0, compliment_plain),
                          compliment_cool = ifelse(is.na(compliment_cool), 0, compliment_cool),
                          compliment_funny = ifelse(is.na(compliment_funny), 0, compliment_funny),
                          compliment_writer = ifelse(is.na(compliment_writer), 0, compliment_writer),
                          compliment_photos = ifelse(is.na(compliment_photos), 0, compliment_photos)
)
#User stars
ggplot(train, aes(user_stars)) + geom_histogram()
train <- train %>% mutate(user_stars = ifelse(is.na(user_stars), median(user_stars, na.rm=TRUE), user_stars))
ggplot(train, aes(user_stars)) + geom_histogram()

#Tip and Checkin Data
ggplot(train, aes(checkin_number)) + geom_histogram()
train <- train %>% mutate(checkin_number = ifelse(is.na(checkin_number),0,checkin_number))
ggplot(train, aes(tip)) + geom_histogram()
train <- train %>% mutate(tip = ifelse(is.na(tip),0,tip))
ggplot(train, aes(tip_compliments)) + geom_histogram()
train <- train %>% mutate(tip_compliments = ifelse(is.na(tip_compliments),0,tip))

#Do the same for the test data:
test <- test %>% mutate(user_review_count = ifelse(is.na(user_review_count), 0, user_review_count))
test <- test %>% mutate(yelping_since = ifelse(is.na(yelping_since), year, yelping_since))
test <- test %>% mutate(user_useful = ifelse(is.na(user_useful), 0, user_useful),
                          user_funny = ifelse(is.na(user_funny), 0, user_funny),
                          user_cool = ifelse(is.na(user_cool), 0, user_cool),
                          user_fans = ifelse(is.na(user_fans), 0, user_fans),
                          compliment_hot = ifelse(is.na(compliment_hot), 0, compliment_hot),
                          compliment_more = ifelse(is.na(compliment_more), 0, compliment_more),
                          compliment_profile = ifelse(is.na(compliment_profile), 0, compliment_profile),
                          compliment_cute = ifelse(is.na(compliment_cute), 0, compliment_cute),
                          compliment_list = ifelse(is.na(compliment_list), 0, compliment_list),
                          compliment_note = ifelse(is.na(compliment_note), 0, compliment_note),
                          compliment_plain = ifelse(is.na(compliment_plain), 0, compliment_plain),
                          compliment_cool = ifelse(is.na(compliment_cool), 0, compliment_cool),
                          compliment_funny = ifelse(is.na(compliment_funny), 0, compliment_funny),
                          compliment_writer = ifelse(is.na(compliment_writer), 0, compliment_writer),
                          compliment_photos = ifelse(is.na(compliment_photos), 0, compliment_photos)
)
test <- test %>% mutate(user_stars = ifelse(is.na(user_stars), median(user_stars, na.rm=TRUE), user_stars))
test <- test %>% mutate(checkin_number = ifelse(is.na(checkin_number),0,checkin_number))
test <- test %>% mutate(tip = ifelse(is.na(tip),0,tip))
test <- test %>% mutate(tip_compliments = ifelse(is.na(tip_compliments),0,tip))

(colSums(!is.na(test))/nrow(test))*100

save.image()

save(test, file = "test.Rda")
rm(test)
gc()

#Explore the data
ggplot(train, aes(x=stars)) + geom_bar()
ggplot(train, aes(x=user_review_count, y=stars)) + geom_point() + geom_smooth(method="lm")
ggplot(train, aes(x=business_review_count, y=stars)) + geom_point() + geom_smooth(method="lm")
ggplot(train, aes(x=user_stars, y=stars)) + geom_point() + geom_smooth(method="lm")
ggplot(train, aes(x=business_stars, y=stars)) + geom_point() + geom_smooth(method="lm")
ggplot(train, aes(x=year, y=stars)) + geom_point() + geom_smooth(method = "lm")

cor.test(test$user_stars,test$stars)
cor.test(test$business_stars,test$stars)

#Linear Model MSE = 1.503014
linear <- lm(stars ~ ., data = train[,-c(26,27)])
summary(linear)
linear_prediction <- predict(linear,newdata=test)
coeftest(linear, vcov = vcovHC(linear, type="HC3"))
mean((test$stars - linear_prediction)^2)

ridge_multi <- glmnet(train[,-c(1,26,27)],train[,1],family = "gaussian",alpha = 0)
multi_predict <- predict(ridge_multi, )

#Ridge Regression with Cross Validation MSE = 1.509343
ridge_cv <- cv.glmnet(as.matrix(train[,-c(1,26,27)]), as.matrix(train[,1]), alpha = 0, nfolds = 3, family = binomial(link="logit"))
plot(ridge_cv)
lambda_ridge_cv <- ridge_cv$lambda.min
ridge <- glmnet(train[,-c(1,26,27)], train[,1], alpha = 0, lambda = lambda_ridge_cv, thresh = 1e-12, family = binomial(link="logit"))
ridge_prediction <- predict(ridge, s = lambda_ridge_cv, newx = as.matrix(test[,-c(1,26,27)]))
mean((ridge_prediction - test[,1]) ^ 2)

#Ridge with Multinomial
ridge_cv_1 <- cv.glmnet(as.matrix(train[,-c(1,26,27)]), as.matrix(train[,1]), alpha = 0, nfolds = 3)
plot(ridge_cv_1)
lambda_ridge_cv_1 <- ridge_cv$lambda.min
ridge_1 <- glmnet(train[,-c(1,26,27)], train[,1], alpha = 0, lambda = lambda_ridge_cv_1, thresh = 1e-12)
ridge_prediction_1 <- predict(ridge, s = lambda_ridge_cv_1, newx = as.matrix(test[,-c(1,26,27)]))
mean((ridge_prediction - test[,1]) ^ 2)

#LASSO Regression with Cross Validation MSE = 1.506188
lasso_cv <- cv.glmnet(as.matrix(train[,-c(1,26,27)]), as.matrix(train[,1]), alpha = 1, nfolds = 3)
plot(lasso_cv)
lambda_LASSO_cv <- lasso_cv$lambda.min
LASSO<-glmnet(train[,-c(1,26,27)], train[,1], alpha = 1, lambda = lambda_LASSO_cv, thresh = 1e-12)
coef(LASSO)
LASSO_prediction <- predict(LASSO, s = lambda_LASSO_cv, newx = as.matrix(test[,-c(1,26,27)]))
mean((LASSO_prediction - test[,1]) ^ 2)

train$stars <- as.factor(train$stars)

#Decision Tree with 'tree' MSE 1.653435 (MSE 14.80175)
tree1 <- tree(stars ~ ., data = train)
tree1_prediction <- predict(tree1, newdata = test)
summary(tree1)
summary(tree1_prediction)
mean((tree1_prediction - test$stars)^2)

#Decision Tree with 'rpart' MSE = 1.653435 (MSE = 14.8)
tree2 <- rpart(stars ~ ., data = train[,-26,27])
rpart.plot(tree2)
tree2_prediction <- predict(tree2, newdata = test)
summary(tree2)
summary(tree2_prediction)
mean((tree2_prediction - test$stars)^2) 

#Decision Tree with Bagging
bag <- bagging(stars~., data=train1, nbagg = 5, coob = TRUE, control = rpart.control(minsplit = 10, cp = 1))
bag #MSE 1.3074

#Decision Tree with Boosting Error = 0.4973
boost <- boosting(stars~., data=train1, boos=TRUE, mfinal=10)
summary(boost)
boost_prediction <- predict(boost, newdata = test)

#Random Forests Error Rate = 0.671
set.seed(1312)
model_RF<-randomForest(stars~.,data=train1, ntree=100)
pred_RF_test = predict(model_RF, test)
mean(model_RF[["err.rate"]])


#review_data_small <- review_data_small %>% separate(date,c('year','month','day','hour','minute','second'))
#user_data_small <- user_data_small %>% separate(yelping_since, c('since_year','since_month','since_day'))



