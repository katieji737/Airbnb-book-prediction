# combind train x and train y
train=cbind(new_df_train, training_y111)

# check all nas in final train dataframe
sapply(train,function(train) sum(is.na(train)))

# check datatypes for all column
train$bed_type=as.factor(train$bed_type)#dummy
train$access=as.factor(train$access)
train$cancellation_policy=as.factor(train$cancellation_policy)
train$city_name=as.factor(train$city_name)#dummy
train$country=as.factor(train$country)
train$country_code=as.factor(train$country_code)
train$description=as.factor(train$description)
#train$experiences_offered=as.factor(train$experiences_offered)#all none level
train$host_about=as.factor(train$host_about)
train$host_has_profile_pic=as.factor(train$host_has_profile_pic)#dummy
train$host_identity_verified=as.factor(train$host_identity_verified)#dummy
train$host_is_superhost=as.factor(train$host_is_superhost)#dummy
train$host_response_time=as.factor(train$host_response_time)
train$house_rules=as.factor(train$house_rules)
train$instant_bookable=as.factor(train$instant_bookable)
train$interaction=as.factor(train$interaction)
train$is_business_travel_ready=as.factor(train$is_business_travel_ready)#dummy
train$is_location_exact=as.factor(train$is_location_exact)#dummy
train$name=as.factor(train$name)
train$neighborhood_overview=as.factor(train$neighborhood_overview)
train$notes=as.factor(train$notes)
train$property_type=as.factor(train$property_type)#dummy
train$require_guest_phone_verification=as.factor(train$require_guest_phone_verification)
train$require_guest_profile_picture=as.factor(train$require_guest_profile_picture)
train$requires_license=as.factor(train$requires_license)#dummy
train$room_type=as.factor(train$room_type)#dummy
train$state=as.factor(train$state)#dummy
train$summary=as.factor(train$summary)
train$transit=as.factor(train$transit)
train$space=as.factor(train$space)
train$high_booking_rate=as.factor(train$high_booking_rate)
sapply(train,class)

# drop columns we haven't cleaned
train <- subset(train, select = c(2,5,6,7,8,9,10,11,12,13,21,23,24,25,26,32,35,39,40,56))
sapply(train,function(train) sum(is.na(train)))

# select samples -total:10000,trian:7000,test:3000
set.seed(11224)
train.total = sample(nrow(train), 1*nrow(train)) 
sampleset = train[train.total,]
train.indicies = sample(nrow(sampleset), .9*nrow(sampleset)) 
train.samples = sampleset[train.indicies,]
test.samples = sampleset[-train.indicies,]

#linear model--RMSE
fit1 <- lm(train.samples$review_scores_rating~.,data=train.samples)
summary(fit1)
lin_preds_train <- predict(fit1,newdata=train.samples)
lin_preds_test <- predict(fit1,newdata=test.samples)
RMSE = sqrt(mean((lin_preds_train-train.samples$review_scores_rating)^2))

#logistic model--accuracy
fit2 <- glm(train.samples$high_booking_rate~.,data=train.samples,family="binomial")
summary(fit2)
log_preds <- predict(fit2,newdata=test.samples,type="response")
log_class <- ifelse(log_preds>.5,1,0)
table(test.samples$high_booking_rate,log_class)
#TEST
test1 <- subset(new_df_test, select = c(2,5,6,7,8,9,10,11,12,13,21,23,24,25,26,32,35,39,40))
log_preds_t <- predict(fit2,newdata=test1,type="response")
log_class_t<- ifelse(log_preds_t>.5,1,0)
log_class_t
write.csv(log_class_t, '/Users/katie/Desktop/test0426.csv')

#knn
#library(class)
library(FNN)
train.samples.X=train.samples[,c(1:19)]
train.samples.Y=train.samples[,c(20)]
knn.pred_train5=knn(train.samples.X,train.samples.X,train.samples.Y,k=5)
knn.pred_valid5=knn(train.samples.X,valid.X,train.profit,k=5)
valid_acc5=sum(ifelse(knn.pred_valid5==credit_valid$PROFITABLE,1,0))/nrow(credit_valid)

#navies-bayes
library(naivebayes)
library(klaR)
library(RTextTools)
library(e1071)
test.samples.Y=test.samples[,c(20)]
Naive_Bayes_Model0 = naiveBayes(train.samples.Y ~., data=train.samples)
Pred_NB0 <- predict(Naive_Bayes_Model0, test.samples)
NB_acc0=sum(ifelse(Pred_NB0==test.samples.Y,1,0))/nrow(test.samples)
NB_acc0





