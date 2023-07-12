#load libraries
library(tidyverse)
library(cluster)
library(fpc)
library(ggplot2)
library(dplyr)
library(factoextra)

#set working directory
setwd("C:xxx")

#import data
credit<-read.csv("credit_cards.csv")
credit<- na.omit(credit)
View(credit)

#set random number seed in order to replicate the centroids chosen
set.seed(42)

#Drop our discrete variables
credit1 <- subset(credit, select = -c(CUST_ID))
View(credit1)

#normalize the data
credit_scaled<-scale(credit1)
credit_scaled <- na.omit(credit_scaled)
View(credit_scaled)

#determine clusters needed within sum of squares
fviz_nbclust(credit_scaled, kmeans, method = "wss") + 
  labs(subtitle = "Elbow Method")

#kmeans
km_output <- kmeans(credit_scaled, centers=4, nstart = 1000)
print(km_output)

#visualize results
fviz_cluster(km_output, data = credit_scaled,
             ggtheme = theme_minimal(),
             labelsize = 0) +
  labs(title = "K-means clustering",
       x = "Component 1", 
       y = "Component 2") + 
  theme(plot.title = element_text(hjust = 0.5)) 
  
#combining each observation's cluster assignment with unscaled data frame
#links clusters to original df
credit_clustered<-cbind(credit, clusterID=km_output$cluster)
View(credit_clustered)

#write data frame to CSV file to analyze in Excel/Python
write.csv(credit_clustered, "credit_clustered.csv")

#calculate variable averages for all non-normalized observations
summarize_all(credit_clustered, mean)

#Calculate variable averages for each cluster
credit_clustered %>%
  group_by(clusterID) %>%
  summarize_all(mean)