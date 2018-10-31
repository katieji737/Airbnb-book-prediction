# combind train x and train y
train=cbind(new_df_train, training_y111)

# check all nas in final train dataframe
sapply(train,function(train) sum(is.na(train)))

# convert some fators to dummy variables
bed_type_dum=model.matrix(~bed_type+0,data=train)
city_name_dum=model.matrix(~city_name+0,data=train)
host_pro_pic_dum=model.matrix(~host_has_profile_pic+0,data=train)
host_iden_dum=model.matrix(~host_identity_verified+0,data=train)
host_sup_dum=model.matrix(~host_is_superhost+0,data=train)
instant_bookable_dum=model.matrix(~instant_bookable+0,data=train)
is_business_travel_ready_dum=model.matrix(~is_business_travel_ready+0,data=train)
is_location_exact_dum=model.matrix(~is_location_exact+0,data=train)
property_type_dum=model.matrix(~property_type+0,data=train)
requires_license_dum=model.matrix(~requires_license+0,data=train)
room_type_dum=model.matrix(~room_type+0,data=train)
state_dum=model.matrix(~state+0,data=train)

# convert to factors
train$access=as.factor(train$access)
train$cancellation_policy=as.factor(train$cancellation_policy)
train$country=as.factor(train$country)
train$country_code=as.factor(train$country_code)
train$description=as.factor(train$description)
train$host_about=as.factor(train$host_about)
train$host_response_time=as.factor(train$host_response_time)
train$house_rules=as.factor(train$house_rules)
train$instant_bookable=as.factor(train$instant_bookable)
train$interaction=as.factor(train$interaction)
train$name=as.factor(train$name)
train$neighborhood_overview=as.factor(train$neighborhood_overview)
train$notes=as.factor(train$notes)
train$require_guest_phone_verification=as.factor(train$require_guest_phone_verification)
train$require_guest_profile_picture=as.factor(train$require_guest_profile_picture)
train$summary=as.factor(train$summary)
train$transit=as.factor(train$transit)
train$space=as.factor(train$space)
train$high_booking_rate=as.factor(train$high_booking_rate)

# combine dummies
train_with_dum=data.frame(train,bed_type_dum,city_name_dum,host_pro_pic_dum,host_iden_dum,host_sup_dum,
                          instant_bookable_dum,instant_bookable_dum,is_business_travel_ready_dum,
                          is_location_exact_dum,property_type_dum,requires_license_dum,room_type_dum,state_dum)

# drop rows we haven't cleaned & dummy column
train_with_dum <- subset(train_with_dum, select = -c(bed_type,city_name,host_has_profile_pic,host_identity_verified,host_is_superhost,instant_bookable,is_business_travel_ready,
                                                     is_location_exact,property_type,requires_license,room_type,state,experiences_offered,
                                                     amenities,extra_people,first_review,host_verifications,license,review_scores_rating))


# select samples -total:10000,trian:7000,test:3000
set.seed(11224)
train.total = sample(nrow(train_with_dum), 1*nrow(train_with_dum)) 
sampleset = train_with_dum[train.total,]
train.indicies = sample(nrow(sampleset), .9*nrow(sampleset)) 
train.samples = sampleset[train.indicies,]
test.samples = sampleset[-train.indicies,]

library(randomForest)
train.samples=train.samples[!is.na(train.samples$price),]
sapply(train.samples,class)
sapply(train.samples,function(train.samples) sum(is.na(train.samples)))
fit3 <- randomForest(train.samples$high_booking_rate~., data=train.samples)
rf_preds <- predict(fit3,newdata=test.samples,type="response")
table(test.samples$high_booking_rate,rf_preds)
