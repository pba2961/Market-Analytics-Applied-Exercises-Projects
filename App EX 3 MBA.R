library(arules) 
library(arulesViz)

cafedata <- read.transactions(file.choose(),
                                               format = "single",
                                               sep = ",", quote="",header = "TRUE",
                                               cols = c("orderno", "item"))

 
itemFrequencyPlot(cafedata,topN=5,type="absolute")

caferules <-apriori(cafedata, parameter = list(supp=0.001, conf=.7))

caferules <- sort(caferules, decreasing = TRUE, by="confidence")


inspect(caferules[1:5])


caferules<-apriori(data= cafedata, parameter=list(supp=0.001,conf = 0.7),
               appearance = list(default="lhs",rhs="Coffee"), control = list(verbose=F))

summary(caferules)

caferules<-sort(caferules, decreasing=TRUE,by="confidence") 
options(digits = 2)

inspect(caferules[1:5])
