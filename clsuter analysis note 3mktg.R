oes<-read.csv(file.choose()) # load oes.csv
summary(oes) # quickly review summary library(dendextend)
library(readr) 
library(psych) 
library(dplyr) 
library(ggplot2)

#install.packages("tibble")
#install.packages("cluster")
#install.packages("purrr")
library(tibble) 
library(cluster) 
library(tidyr) 
library(purrr)

# Calculate Euclidean distance between the occupations 
dist_oes <- dist(oes, method = 'euclidean')
hc_oes <- hclust(dist_oes, method = 'average')
# Create a dendrogram object from the hclust variable 
dend_oes <- as.dendrogram(hc_oes)
# Step 4 Plot the dendrogram # Plot the dendrogram 
plot(dend_oes)
# Use rownames_to_column to move the rownames into a column of the data frame

df_oes <- rownames_to_column(as.data.frame(oes), var = 'occupation')

# Step 5 Create a cluster assignment vector at h = 100000

cut_oes <- cutree(hc_oes, h = 100000)

# Step 6 Generate the segmented customers dataframe

clust_oes <- mutate(df_oes, cluster = cut_oes)

# Step 7 Lets make things beautiful - create a clean dataframe

# Create a tidy data frame by gathering the year and values into two columns 
gathered_oes <- gather(data = clust_oes,
key = year,

value = mean_salary,

-occupation, -cluster)
# Step 8 sort the clustering assignments

# View the clustering assignments by sorting the cluster assignment vector sort(cut_oes)
# Plot the relationship between mean_salary and year and color the lines by the assigned cluster

ggplot(gathered_oes, aes(x = year, y = mean_salary, color = factor(cluster)))+	geom_line(aes(group = occupation))


#install.packages("animation")
library(animation)
##  set  larger  'interval'  if  the  speed  is  too  fast 
ani.options(interval  =  1)
par(mar  =  c(3,  3,  1,  1.5),  mgp  =  c(1.5,  0.5,  0))

kmeans.ani()

# Use map_dbl to run many models with varying value of k (centers) 
tot_withinss <- map_dbl(1:10,	function(k){
  model <- kmeans(x = oes, centers = k) 
  model$tot.withinss})

# Generate a data frame containing both k and tot_withinss 
elbow_df <- data.frame(
k = 1:10,

tot_withinss = tot_withinss

)

# Plot the elbow plot

ggplot(elbow_df, aes(x = k, y = tot_withinss)) + geom_line() +
  scale_x_continuous(breaks = 1:10)

Use map_dbl to run many models with varying value of k 
sil_width <- map_dbl(2:10,	function(k){
  model <- pam(oes, k = k)
  
  model$silinfo$avg.width
  
})


# Generate a data frame containing both k and sil_width 
sil_df <- data.frame(
k = 2:10,

sil_width = sil_width

)


# Plot the relationship between k and sil_width 
ggplot(sil_df, aes(x = k, y = sil_width)) +
geom_line() + scale_x_continuous(breaks = 2:10)

