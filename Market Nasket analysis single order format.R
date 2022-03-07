library(arules) 
library(arulesViz)

transactionsbycolumnnames <- read.transactions(file.choose(),
                                               format = "single",
                                               sep = ",", header = "TRUE",
                                               cols = c("invoiceid", "description"))

transactionsbycolumnnumbers <- read.transactions(file.choose(),
                                                 format = "single",
                                                 sep = ",", header = "TRUE", cols = c(1,3))

decordata <- transactionsbycolumnnames 
transactionsbycolumnnumbers 
transactionsbycolumnnames
decordata

itemFrequencyPlot(decordata,topN=20,type="absolute")

# Step 4 - Let us try to be conservative and extract some rules from the decordataset. We are trying to use a support of 0.005 and a confidence of .80
rules <- apriori(decordata, parameter = list(supp = 0.005, conf = 0.8, maxlen=3)) 
summary(rules)
rules<-sort(rules, by="confidence", decreasing=TRUE) 
options(digits=2)
inspect(rules[1:10])



# Step 5 - An alternative to this is to just extract the rules of length 3 WIth a more typical #support of .001 and a confidence of 0.8 and a maxlen of 3. Note that these two commands #are going to extract a very different set of rules

rules2 <- apriori(decordata, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
rules2<-sort(rules2, by="confidence", decreasing=TRUE) 
options(digits=2)
inspect(rules2[1:10])
