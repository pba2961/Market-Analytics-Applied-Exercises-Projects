customers_spend <-read.csv(file.choose()) #load ws_customers.csv
summary(customers_spend) # quickly review summary


#install.packages("dendextend")
library(dendextend) # if you wish to color dendrograms-optional 
library(psych)
library(dplyr) 
library(ggplot2)
library(readr)

# Step 2 Calculate Euclidean distance between customers

dist_customers <- dist(customers_spend)

# Step 3 Generate a complete linkage analysis

hc_customers <- hclust(dist_customers, method = "complete")

# Step 4 Plot the dendrogram

plot(hc_customers)

# Step 5 Create a cluster assignment vector at h = 15000

clust_customers <- cutree(hc_customers, h = 15000)

# Step 6 Generate the segmented customers dataframe

segment_customers <- mutate(customers_spend, cluster = clust_customers)

count(segment_customers, cluster)
segment_customers %>% group_by(cluster)%>% summarize_all(funs(mean(.)))
