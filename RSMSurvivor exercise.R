library(readxl)
RSMSurvivor<-read_excel("C:/Users/Palam/Downloads/RSMSurvivor.xls")

library(survival)
# Create a "survival object" for each observation, using time and churn data.
RSMSurvivor$survival <- Surv(RSMSurvivor $time, RSMSurvivor
                             $churned == 1)
# Fit a basic survival curve using the data
fit <- survfit(survival ~ 1, data = RSMSurvivor)
# Plot the survival curve and add a title!

plot(fit,lty=1,mark.time=FALSE,ylim=c(.75,1),xlab='Days since Subscribing',ylab='Percent Surviving')
title(main = "RSMSurvivor Survival Curve")

# Assuming you have everything loaded from the example available # Fit survival curves based on the female binary
fit <- survfit(survival ~ female, data = RSMSurvivor)

# Plot the results, add a legend, add a title to make it easy to #read
plot(fit, lty = 1:2, mark.time = FALSE, ylim=c(.75,1), xlab = 'Days since
Subscribing', ylab = 'Percent Surviving')
legend(20, .8, c('Male', 'Female'), lty=1:2, bty = 'n', ncol = 2)
title(main = " RSMSurvivor Survival Curves by Gender")

# Importantly, we need to run a log-rank test to see if the #curves are statistically different
survdiff(survival ~ female, data = RSMSurvivor)
