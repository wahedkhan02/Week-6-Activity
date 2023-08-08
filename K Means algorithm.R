#K-Means Clustering

require("datasets")
data("iris") # load Iris Dataset
str(iris) #view structure of dataset

summary(iris) #view statistical summary of dataset

head(iris, 3) #view top  rows of dataset

#Preprocess the dataset
#Since clustering is a type of Unsupervised Learning, 
#we would not require Class Label(output) during execution of our algorithm. 
#We will, therefore, remove Class Attribute “Species” and store it in another variable. 
#We would then normalize the attributes between 0 and 1 using our own function.

iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new, 3)

head(iris.class, 3)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)

#Apply the K-means clustering algorithm
result<- kmeans(iris.new,3) #aplly k-means algorithm with no. of centroids(k)=3
result$size # gives no. of records in each cluster

result$centers # gives value of cluster center datapoint value(3 centers for k=3)

result$cluster #gives cluster vector showing the custer where each record falls

#Verify results of clustering
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.new[c(1,2)], col=result$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.new[c(1,2)], col=iris.class)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.new[c(3,4)], col=result$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=iris.class)

table(result$cluster,iris.class)

#Results of the table show that Cluster 1 corresponds to Virginica, 
#Cluster 2 corresponds to Versicolor 
#and Cluster 3 to Setosa.

#Total number of correctly classified instances are: 36 + 47 + 50= 133
#Total number of incorrectly classified instances are: 3 + 14= 17

#How did the model do?
The model achieved a decent performance but appears to struggle in correctly classifying the instances in the "virginica" class, as all instances in Cluster 2 were misclassified as "virginica" instead of the correct class "versicolor."

#TASK: Accuracy = number of correctly classified/(total classified) = ?
#i.e our model has achieved ?% accuracy!
To calculate the number of correctly classified instances:
  
Cluster 1 was correctly classified as "versicolor" (46 instances) and "virginica" (50 instances).
Cluster 2 was correctly classified as "setosa" (33 instances).
Cluster 3 was misclassified as "setosa" instead of "versicolor" (4 instances).
Therefore, the total number of correctly classified instances is 46 + 50 + 33 = 129.

To calculate the accuracy:
  
  Accuracy = (129 / 150) * 100

Calculating the above expression:
  
  Accuracy ≈ 86%

Hence, the model achieved an accuracy of approximately 86%.


