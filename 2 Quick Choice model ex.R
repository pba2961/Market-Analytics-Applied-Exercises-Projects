#install.packages("mlogit") 
#install.packages("conjoint")

sportscar<-read.csv(file.choose()) # load sportscarsurveyconjoint.csv 
str(sportscar, give.attr = FALSE)
head(sportscar)

#install.packages("dfidx")

library(mlogit) 
library(dplyr) 
library(conjoint) 
library(ggplot2)


# Print crosstabs of chosen sportscars by transmission type 
chosen_by_transmission <- xtabs(choice ~ trans, data=sportscar) 
chosen_by_transmission

# Plot the chosen_by_trans object 
barplot(chosen_by_transmission)

# Print crosstabs of chosen sportscars by seat number 
xtabs(choice ~ seat, data=sportscar)


# Print crosstabs of chosen sportscars by convertible tops 
xtabs(choice ~ convert, data=sportscar)

# Print crosstabs of chosen sportscars by price 
xtabs(choice ~ price, data=sportscar)

# fit a choice model using mlogit() and assign the output to m1 # the alt attribute refers to the alternative identifier column
# the choice attribute refers to the customer choice identifier column
# conveniently for us, these are the alt and choice columns in our dataset

m1 <- mlogit(choice ~	trans + convert + price, data=sportscar, alt.var="alt", choice = "choice")

# summarize the m1 object to see the output 
summary(m1)


