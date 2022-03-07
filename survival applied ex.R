#Step 1 - Load the dataset
library(readxl)

dataphoto<-read_xls("C:/Users/Palam/Downloads/photoservicedataset.xls")
str(dataphoto, give.attr = FALSE) 
head(dataphoto)

dataphoto$sex <- as.factor(dataphoto$sex)




#Step 2 - load the relevant libraries

library(ggplot2) 
library(dplyr) 
library(readr) 
library(psych) 
library(rms) 
library(survival)



# Step 4 - Create the survival object using daysSinceFirstPurch and boughtAgain

survObjphoto <- Surv(dataphoto$datesincejoining, dataphoto$lostcustomer)


#Step 5 plot and check out the raw structure
plot(survObjphoto)




# Step 6 Compute and fit KM fitKMSimple <- survfit(survObj ~ 1) print(fitKMSimple)
fitKMSimple <- survfit(survObjphoto ~ 1)
print(fitKMSimple)
# Step 7 Determine the distributions of the predictor variables
dd <- datadist(dataphoto) 
options(datadist = "dd")

# Step 7 Plot the fit
plot(fitKMSimple, conf.int = FALSE, xlab = "datesincejoining", ylab
     = "Survival function", main = "Survival function")
# Step 8 Compute the Cox PH Model and print the results
fitCPHoto <- cph(Surv(datesincejoining, lostcustomer) ~ sex + photosprinted, data = dataphoto, x = TRUE, y = TRUE, surv = TRUE)
print(fitCPHoto)
# Step 9 Interpret the coefficients
exp(fitCPHoto$coefficients)
# Step 10 Plot the result summary
plot(summary(fitCPHoto), log = TRUE)
# Step 11 # Check proportional hazard assumption, print result and plot it
testCPH <- cox.zph(fitCPH) 
print(testCPH)
plot(testCPH, gender="male")



