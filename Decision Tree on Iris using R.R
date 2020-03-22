# Let us induct a decision tree on the iris dataset in R.
#Displaying a few records of the predefined iris data set
#Here we are going to predict the species of an iris flower
#on the basis of Sepal length, Sepal width, Petal length, and Petal width
"The points at which the tree is split can be called decision boundaries. In this example, any data point that 
falls in the region where Petal.Length < 2.4 is classified as setosa"
head(iris)
# Splitting the data into train & test
set.seed(50)
iris_size <- nrow(iris)
train_data_size <- floor(0.75*iris_size)
train_indices <- sample(1:iris_size,train_data_size)
train_data <- iris[train_indices,]
test_data <- iris[-train_indices,]
# Inducting a decision tree to predict the species
library(rpart)
library(rpart.plot)
iris_dt <- rpart(Species~.,data = train_data)
rpart.plot(iris_dt,type = 3,extra = 1,round=0)
# Using the ggplot2 library for visualization
library(ggplot2)
ggplot(train_data, aes(x = Petal.Length,y = Petal.Width, color = Species)) +
  geom_point(size = 2) + 
  scale_x_continuous(breaks = seq(0,max(train_data$Petal.Length),0.5)) +
  scale_y_continuous(breaks = seq(0,max(train_data$Petal.Width),0.5)) +
  geom_hline(yintercept = 1.8) + geom_vline(xintercept = 2.4)


# Using predict() function to make predictions
predicted_species_train <- predict(iris_dt,train_data[,-5],type="class")
addmargins(table(train_data$Species,predicted_species_train))
#

# Using predict() function to make predictions on test data
predicted_species_test <- predict(iris_dt,test_data[,-5],type="class")
addmargins(table(test_data$Species,predicted_species_test))

