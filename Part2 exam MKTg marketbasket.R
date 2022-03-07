library(arules) 
library(arulesViz)

transactionsbycolumnnames <- read.transactions(file.choose(),
                                               format = "single",
                                               sep = ",", header = "TRUE",
                                               cols = c("orderid", "menuitem"))

transactionsbycolumnnumbers <- read.transactions(file.choose(),
                                                 format = "single",
                                                 sep = ",", header = "TRUE", cols = c(1,3))


mexdata <- transactionsbycolumnnames 
transactionsbycolumnnumbers 
transactionsbycolumnnames
mexdata

itemFrequencyPlot(mexdata,topN=20,type="absolute")

#now lets try to see top 10 items
itemFrequencyPlot(mexdata,topN=10,type="absolute")


# Step 4 - Let us try to be conservative and extract some rules from the mexdataset. We are trying to use a support of 0.005 and a confidence of .80
mexrules <- apriori(mexdata, parameter = list(supp = 0.001, conf = 0.8,maxlen=5))
mexrules<-sort(mexrules, by="confidence", decreasing=TRUE) 
options(digits=2)
inspect(mexrules[1:10])


mexrules2<-apriori(data= mexdata, parameter=list(supp=0.001,conf = 0.8, maxlen=5),
                   appearance = list(default="lhs",rhs="Chicken Burrito"), control = list(verbose=F))

mexrules2<-sort(mexrules2, decreasing=TRUE,by="confidence") 
options(digits = 2)
inspect(mexrules2[1:5])


mexrules3<-apriori(data= mexdata, parameter=list(supp=0.001,conf = 0.8, maxlen=3),
                   appearance = list(default="lhs",rhs="Chicken Bowl"), control = list(verbose=F))

mexrules3<-sort(mexrules3, decreasing=TRUE,by="confidence") 
options(digits = 2)
inspect(mexrules3[1:5])

summary(mexrules3)
summary (mexrules)
summary(mexrules2)


mexrules4<-apriori(data= mexdata, parameter=list(supp=0.001,conf = 0.8, maxlen=3),
                   appearance = list(default="rhs",lhs="Chicken Bowl"), control = list(verbose=F))

mexrules4<-sort(mexrules4, decreasing=TRUE,by="confidence") 
options(digits = 2)
summary(mexrules4)
