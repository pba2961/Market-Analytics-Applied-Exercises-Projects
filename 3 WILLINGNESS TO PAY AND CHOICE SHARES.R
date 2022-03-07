
library(mlogit) 
library(dplyr) 
library(conjoint) 
library(ggplot2)

sportscar<-read.csv(file.choose()) # load sportscarsurveyconjoint.csv 
str(sportscar, give.attr = FALSE)
head(sportscar)

sportscar$seat <- as.factor(sportscar$seat)

# Convert data to mlogit.data, varying = 4:8 indicates the attribute columns.
sportscar.ml <- mlogit.data(sportscar, shape = 'long', choice = 'choice', alt.var = 'alt')

summary(sportscar.ml)
# fit a choice model using mlogit() and assign the output to m1 # the alt attribute refers to the alternative identifier column
# the choice attribute refers to the customer choice identifier column
# conveniently for us, these are the alt and choice columns in our dataset

sportscar_model <- mlogit(choice ~ 0 + seat + trans + convert + price + price:segment , data = sportscar.ml)

# summarize the m1 object to see the output 
summary(sportscar_model)

WTP <- coef(sportscar_model)/- coef(sportscar_model)[5] 
WTP

## Predict choice share based on model 
predict_mnl <- function(model, products) {
data.model <- model.matrix(update(model$formula, 0 ~ .),
                           data = products)[,-1] 
utility <- data.model%*%model$coef
share <- exp(utility)/sum(exp(utility)) 
cbind(share, products)
}

# Create hypothetical data for choice share prediction 
car <- c(1,2,3)
price <- c(30, 30, 30)
seat <- factor(c(2, 2, 2), levels=c(2,4,5))
trans <- factor(c("auto", "manual","auto"), levels=c("auto", "manual"))
convert <- factor(c("yes", "yes", "no"), levels=c("no", "yes"))
segment <- factor(c("racer", "racer", "racer"), levels=c("basic", "fun", "racer"))
prod <- data.frame(car,seat, trans, convert, price, segment)


#examine what you just created
prod

# Predict choice shares of hypothetical 3-option sports car selection 
shares <- predict_mnl(sportscar_model, prod)
shares

#plot it to see it visually
ggplot(shares, aes(x = car, y = share, fill = car))+ geom_bar(stat = 'identity')+
  ylab('Predicted Market Share')+ xlab('Proposed Car Models')+ ggtitle('Choice Share of Car Models')




