pacman::p_load(psych, ggplot2, devtools, caret, rpart, rpart.plot, RColorBrewer, party, partykit, pROC, e1071, randomForest, gbm)

# load the data unless you already have it loaded
storedata <- read.csv(file.choose()) #choose the storeDataset.csv


# examine the dataset
str(storedata)


# fix the items that should have been categorical in the first place
storedata$urban <- as.factor(storedata$urban)
storedata$majorcity <- as.factor(storedata$majorcity)
storedata$storeshelf <-as.factor(storedata$storeshelf)



# Step 2 - Let's get to plotting the data to examine the cutoff, peaks, and valleys.
ggplot(data=storedata, aes(x=salesinthousandsrounded)) +
  geom_histogram(binwidth=1, boundary=.5, fill="white", color="black") +
  geom_vline(xintercept = 8, color="red", size=2) +
  labs(x = "Sales in Thousands of Dollars")


# Step 3 - The data is split around 8 so lets choose 8 (really 8k USD) as the cutoff
# between low/high so create a new variable called highsalestore and set it to yes/no
# based on 8. Basically stores that sold 8k of the product are highsale=yes.

storedata$highsale=ifelse(storedata$salesinthousandsrounded<=8,"No","Yes")
str(storedata) 
storedata$highsale <- as.factor(storedata$highsale)
str(storedata) 

# Step 4 - We are done with the salesinthousandsrounded for now so let's remove it
# since we want a tree we should also fix the storeshelf variable to be numeric
storedata$salesinthousandsrounded <- NULL
storedata$X <- NULL
storedata$storeshelf <- as.numeric(storedata$storeshelf)


# Step 5 - Time to split the data into a test and train dataset but we are doing this
# randomly and setting up half the data as training and the other half as testing
halfsample = sample(dim(storedata)[1], dim(storedata)[1]/2)
storedata.train = storedata[halfsample, ]
storedata.test = storedata[halfsample, ]


# Step 6 - Now we set a seed for replication and then setup crossvalidation
set.seed(123456)
cvcontrol <- trainControl(method="repeatedcv", number = 10, allowParallel=TRUE)


# Step 7 - Let's train a tree, use crossvalidation and tune the complexity parameter to 10
train.tree <- train(as.factor(highsale) ~ .,
                    data=storedata.train,
                    method="ctree",
                    trControl=cvcontrol,
                    tuneLength = 10)
train.tree #checkout your results




# Step 8 - plot the tree and review it.
plot(train.tree$finalModel, main="Regression Tree for Product High Sales")







# Step 9 - first obtain the predictions and then examine the dataset, we can use this to
# create a confusion matrix. Write down the accuracy obtained for reference.
tree.classTrain <- predict(train.tree, type="raw")
head(tree.classTrain)
confusionMatrix(storedata.train$highsale,tree.classTrain)


# Step 10 - My data had an accuracy of .8 which is quite good. Now let's apply this to the
# test data and see how it performs by examining the test confusion matrix
tree.classTest <- predict(train.tree, newdata = storedata.test, type="raw")
head(tree.classTest)
confusionMatrix(storedata.test$highsale,tree.classTest)


# My sample and model seem to be great, even the test data rendered an
# accuracy of .8, this isn't common but it is to be appreciated. However there are many
# ways to evaluate a model. Your results may not be the same due to sampling
# Step 11 - One way to evaluate the model is to use an ROC curve. The pROC package is
# excellent for this purpose, however you may also get the ROC curve by calculating the
# yes probabilities (or probability of 1's) in the model and then plotting it. Let's do that
# now.
tree.probs=predict(train.tree, newdata=storedata.test, type="prob")
head(tree.probs) #examine the data


#calcualte the ROC curve
rocCurve.tree <- roc(storedata.test$highsale,tree.probs[,"Yes"])


#plot the ROC curve, changing the c value will change the color
plot(rocCurve.tree,col=c(4))




# Step 13 - Our first attempt here is to use Bagging. We begin by optimizing the fit of a

train.bagg <- train(as.factor(highsale) ~ .,
                    data=storedata.train,
                    method="treebag",
                    trControl=cvcontrol,
                    importance=TRUE)
train.bagg #check out the model, my model accuracy was close to .8
# Step 14 - plot the variable importance and examine the result
plot(varImp(train.bagg)) #understandably, price is a big deal)

# Step 15 - As before, calculate and review the confusion matrix
bagg.classTrain <- predict(train.bagg, type="raw")
head(bagg.classTrain)
confusionMatrix(storedata.train$highsale,bagg.classTrain)




# Step 16 - Calculate and review the confusion matrix but this time for the test data
bagg.classTest <- predict(train.bagg, newdata = storedata.test, type="raw")
head(bagg.classTest)
confusionMatrix(storedata.test$highsale,bagg.classTest)




bagg.probs=predict(train.bagg, newdata=storedata.test, type="prob")
head(bagg.probs)

#calcualte the ROC curve
rocCurve.bagg <- roc(storedata.test$highsale,bagg.probs[,"Yes"])
#plot the ROC curve, changing the c value will change the color
plot(rocCurve.bagg,col=c(6)) #note how the curve doesn't look like a curve

# Step 17 - Repeat the prior procedures to get the AUC
auc(rocCurve.bagg)


# Step 18 - We are going to repeat steps 13-17 but we now build a random forest based
# classifier. We build the model, then predict, and calculate the confusion matrix. Then
# we calculate the ROC, plot it and finish with an AUC calculation.
train.rf <- train(as.factor(highsale) ~ .,
                  data=storedata.train,
                  method="rf",
                  trControl=cvcontrol,
                  importance=TRUE)
train.rf
rf.classTrain <- predict(train.rf, type="raw")
head(rf.classTrain)
confusionMatrix(storedata.train$highsale,rf.classTrain)
rf.probs=predict(train.rf, newdata=storedata.test, type="prob")
head(rf.probs)
rocCurve.rf <- roc(storedata.test$highsale,rf.probs[,"Yes"])
plot(rocCurve.rf,col=c(1))
auc(rocCurve.rf)



train.cf <- train(highsale ~ .,
                  data=storedata.train,
                  method="cforest",
                  trControl=cvcontrol)
train.cf
cf.classTrain <- predict(train.cf, type="raw")
head(cf.classTrain)
confusionMatrix(storedata.train$highsale,cf.classTrain)
cf.probs=predict(train.cf, newdata=storedata.test, type="prob")
head(cf.probs)
rocCurve.cf <- roc(storedata.test$highsale,cf.probs[,"Yes"])
plot(rocCurve.cf,col=c(2))
auc(rocCurve.cf)


# Step 20 - Similar to step 18 we will now run yet another random forest but this time
# with boosting. You should be a pro at this by now so just repeat the same steps to run
# the model, calculate the confusion matrix, plot the ROC and finish with the AUC.
train.gbm <- train(as.factor(highsale) ~ .,
                   data=storedata.train,
                   method="gbm",
                   verbose=F,
                   trControl=cvcontrol)
train.gbm
gbm.classTrain <- predict(train.gbm, type="raw")
head(gbm.classTrain)
confusionMatrix(storedata.train$highsale,gbm.classTrain)
gbm.classTest <- predict(train.gbm, newdata = storedata.test, type="raw")
head(gbm.classTest)
confusionMatrix(storedata.test$highsale,gbm.classTest)
gbm.probs=predict(train.gbm, newdata=storedata.test, type="prob")
head(gbm.probs)
rocCurve.gbm <- roc(storedata.test$highsale,gbm.probs[,"Yes"])
plot(rocCurve.gbm, col=c(3))
auc(rocCurve.gbm)



# Step 21 - The final step - We now plot all of the curves next to each other to see
# how each method performed.
plot(rocCurve.tree,col=c(4)) # this is our base model
plot(rocCurve.bagg,add=TRUE,col=c(6)) # color magenta is bagg
plot(rocCurve.rf,add=TRUE,col=c(1)) # color black is rf
plot(rocCurve.cf,add=TRUE,col=c(2)) # color red is cforest
plot(rocCurve.gbm,add=TRUE,col=c(3)) # color green is gbm



# if you wish to compare all AUC's in one shot
auc(rocCurve.tree) # this is our base model AUC
auc(rocCurve.bagg) # this is our bagged model AUC
auc(rocCurve.rf) # this is our random forest AUC
auc(rocCurve.cf) # this is our conditional inference tree AUC
auc(rocCurve.gbm) # this is gradient boosted model AUC