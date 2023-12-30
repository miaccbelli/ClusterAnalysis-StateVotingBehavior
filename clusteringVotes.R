# Mia Belli
# 12/15/23
# Clustering with Votes for Republican Candidates in Presidential Elections

#### LOADING PACKAGES & DATA - CLUSTERING ####

# For Clustering
install.packages("pastecs")
install.packages("cluster") # kmeans and hierarchical
install.packages("factoextra") # visualize clusters

library(pastecs)
library(cluster)
library(factoextra)

# load data - clustering dataset
help("votes.repub")
data("votes.repub")
class(votes.repub)

#### DATA EXPLORATION - CLUSTERING ####
# summary and info abt attributes
names(votes.repub) # grabbing attribute names
row_names <- rownames(votes.repub)
print(row_names) # seeing the row names = states
head(votes.repub) # looking at the first 6 rows in the data set
tail(votes.repub) # last 6 rows of dataset
str(votes.repub) # gives us attributes, their types, total vars and rows
summary(votes.repub)
pastecs::stat.desc(votes.repub)

# general visualization and other exploration - finding top/bottom states with republican votes based on mean percentages
top_states <- rownames(votes.repub)[order(-rowMeans(votes.repub))[1:5]] # states with the lowest mean percentage of votes for the Republican candidate across all elections
print(top_states)

bottom_states <- rownames(votes.repub)[order(rowMeans(votes.repub))[1:5]] # states with the lowest mean percentage of votes for the Republican candidate across all elections
print(bottom_states)

top_states_data <- votes.repub[top_states, ]
barplot(t(top_states_data), beside = TRUE, col = c("red", "blue", "green", "purple", "orange"),
        main = "Mean Percentages of Votes for Republican Candidate (Top 5 States)",
        xlab = "States", ylab = "Mean Percentage of Votes",
        names.arg = top_states)  # Use names.arg to specify the names of the states on the x-axis
legend("topright", legend = top_states, fill = c("red", "blue", "green", "purple", "orange"))

bottom_states_data <- votes.repub[bottom_states, ]
barplot(t(bottom_states_data), beside = TRUE, col = c("red", "blue", "green", "purple", "orange"),
        main = "Mean Percentages of Votes for Republican Candidate (Bottom 5 States)",
        xlab = "States", ylab = "Mean Percentage of Votes",
        names.arg = bottom_states)  # Use names.arg to specify the names of the states on the x-axis
legend("topright", legend = bottom_states, fill = c("red", "blue", "green", "purple", "orange"))

#### PRE-PROCESSING ####
is.na(votes.repub) # finding null values in table 
sum(is.na(votes.repub)) # finding total null values 

stat.desc(votes.repub)

# finding the attributes with the highest number of NA/Nulls
na_counts <- colSums(is.na(votes.repub))
attributes_with_most_na <- names(na_counts)[na_counts > 0]
print(attributes_with_most_na) #  "X1856" "X1860" "X1864" "X1868" "X1872" etc attributes with highest # of NA

# eliminating the 26 attributes/elections but creating a new subset
data_without_na_attributes <- votes.repub[, setdiff(names(votes.repub), attributes_with_most_na)]

# double checking that there are no longer null values
na_counts_after_subset <- colSums(is.na(data_without_na_attributes))
print(na_counts_after_subset)

# creating a new dataset that omits the subset containing all those null attributes
data2 <- na.omit(data_without_na_attributes) 
sum(is.na(data2)) # finding total null values 
summary(data2) # double checking what the new data set looks like
head(data2) # this data 2 will be used for k means but hierarchical we can use the original votes.repub 

# standardizing ranges of numeric attributes
str(data2) # all numeric
str(votes.repub) # all numeric

data2 <- scale(data2)
votes.repub <- scale(votes.repub)

View(data2)
View(votes.repub)

#### CLUSTERING ####

# K-MEANS clustering 
# Distance Matrix
cluster.dist <- get_dist(data2) 
cluster.dist

cluster.dist <- get_dist(data2, method = 'pearson')
cluster.dist

# visualize the matrix
factoextra::fviz_dist(cluster.dist)
factoextra::fviz_nbclust(data2, kmeans, method = "wss") 

set.seed(123) #set the seed to set the random clusters and make it repeatable for the future
k5 = kmeans(data2, centers = 5, nstart = 25)
k5 #summary of the kmeans clusters and estimates
k5$cluster #s hows which objects are part of which cluster
k5$betweenss #shows the sum of distances between all the clusters
k5$withinss 
#larger betweenss means clusters are more different from each other, and smaller withinss means

set.seed(123)
k6 = kmeans(data2, centers = 6, nstart = 25)
k6
k6$cluster
k6$betweenss
k6$withinss

set.seed(123)
k7 = kmeans(data2, centers = 7, nstart = 25)
k7
k7$cluster
k7$betweenss
k7$withinss

# finally visualize the best k cluster - being cluster 6
fviz_cluster(k6, data = data2)


# HIERARCHICAL clustering 
# note: using the votes.repub bc it can handle the null

#create hierarchical clusters using methods >> choose the one with the highest agglomerative coefficient (ac)
# ac values closer to 1 are preferred

hc.single = agnes(votes.repub, method = 'single')
hc.single #summary of the hierarchical clusters
# use the ac to choose the best hierarchical cluster
# higher values of ac means better coherent clusters that capture the similarities between data objects in the dendrogram

hc.complete = agnes(votes.repub, method = 'complete')
hc.complete$ac #the complete link (or MAX) method has the ac of .8623

hc.average = agnes(votes.repub, method = 'average')
hc.average$ac #the average method has the ac of .7490

plot(hc.complete)
# Low height implies high similarity, while high height implies high dissimilarity
