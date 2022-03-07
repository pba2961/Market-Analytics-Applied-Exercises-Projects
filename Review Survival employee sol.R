library(survival) 
library(rms) 
library(ggplot2) 
library(dplyr) 
library(readr) 
library(psych)
library(lattice)
library(fomrula)
#install.packages("Formula")
library(Hmisc) 

empdata<-read.csv(file.choose())

#displays string representation of data in each col 
str(empdata, give.attr = FALSE)

empdata<-na.omit(empdata)


empdata2 <- empdata %>% subset(empdata$education!="High School",select=colnames(empdata)) %>% subset(empdata$education!="Primary School",select=colnames(empdata)) %>% subset(select = -employeeid)

empdata2$locationcode <- as.factor( ifelse(empdata2$locationcode<=50,"east", ifelse(empdata2$locationcode<=100,"midwest", ifelse(empdata2$locationcode<=150,"mountain","west")))) 
empdata2$totalcertificationhours <- as.factor( ifelse(empdata2$totalcertificationhours<=100,"0_100", ifelse(empdata2$totalcertificationhours<=200,"101_200", ifelse(empdata2$totalcertificationhours<=300,"201_300","300_ "))))
empdata2$yearsatfirm <- as.numeric(empdata2$yearsatfirm) 
empdata2<-na.omit(empdata2)


unique(empdata2[,"yearsatfirm"])
empdata2$yearsatfirm <- sub("<1", "0", empdata2$yearsatfirm) 
unique(empdata2[,"yearsatfirm"])
empdata2$yearsatfirm <- as.numeric(as.character(empdata2$yearsatfirm))
str(empdata, give.attr = FALSE)


mean(empdata2$yearsatfirm)

unique(empdata2[,"gender"])
empdata2$gender <- sub("^$", "Other", empdata2$gender) 
unique(empdata2[,"gender"])


empdata2$gender <- as.factor(empdata2$gender)

empdata2$hiredonexperience <- as.factor(empdata2$hiredonexperience) 
empdata2$currentnlyinschool <- as.factor(empdata2$currentnlyinschool) 
empdata2$hiredonexperience <- as.factor(empdata2$hiredonexperience) 
empdata2$education <- as.factor(empdata2$education) 
empdata2$degreefield <- as.factor(empdata2$degreefield) 
empdata2$company_type <- as.factor(empdata2$company_type) 
empdata2$employeesinfirm <- as.factor(empdata2$employeesinfirm)


empdata2$currentnlyinschool <- sub("^$", "Other", empdata2$currentnlyinschool) 
empdata2 <- empdata2[!empdata2$currentnlyinschool == "Other",] 
empdata2$currentnlyinschool <- as.factor(empdata2$currentnlyinschool)


unique(empdata2[,"currentnlyinschool"]) 
str(empdata2, give.attr = FALSE) 
unique(empdata2[,"yearsincurrentposition"])

# dropping "never" and ">4"
empdata2$yearsincurrentposition <- sub("^$", "never", empdata2$yearsincurrentposition) 
empdata2 <- empdata2[!empdata2$yearsincurrentposition == "never",]
empdata2 <- empdata2[!empdata2$yearsincurrentposition == ">4",] 
empdata2$yearsincurrentposition <- as.numeric(as.character(empdata2$yearsincurrentposition)) 
str(empdata2, give.attr = FALSE)


# making two histograms, one for quit = 0, one for qui1 = 1, based on years at firm 
ggplot(empdata2) + geom_histogram(aes(x = yearsatfirm,
fill = factor(quit))) + facet_grid( ~ quit) + theme(legend.position = "none")


# Surv(time, event), Create a survival object, usually used as a response variable in a model formula
# for right censored data, time is the follow up time
# The status indicator, here 0 = did not buy again, 1 = did buy again 
survObj <- Surv(empdata2$yearsatfirm, empdata2$quit)
str(survObj)

fitKMSimple <- survfit(survObj ~ 1)
print(fitKMSimple)


plot(fitKMSimple, conf.int = FALSE, xlab = "Years at firm", ylab = "Survival function", main = "Survival function")


fitCPH <- cph(Surv(yearsatfirm, quit) ~ gender +
                hiredonexperience + currentnlyinschool + education +
                degreefield + employeesinfirm + company_type + yearsincurrentposition
              + totalcertificationhours, data = empdata2, x = TRUE, y = TRUE, surv = TRUE)

print(fitCPH) 

#tuning
fitCPH2 <- cph(Surv(yearsatfirm, quit) ~ gender + hiredonexperience + currentnlyinschool + education + employeesinfirm + company_type + yearsincurrentposition
               , data = empdata2, x = TRUE, y = TRUE, surv = TRUE)

print(fitCPH2)

# will remove total certification hours from model and run again 
exp(fitCPH2$coefficients)
