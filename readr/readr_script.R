setwd("C:/Users/user/Desktop/R DIRECTORY")
##readr
#reading diffnt data types into R
library(readr)
library(data.table)
library(datasets)
data(iris)
read_csv("iris.csv",col_names = TRUE)
read_csv("iris.csv", col_types = list(
  Sepal.Length = col_double(),
  Sepal.Width = col_double(),
  Petal.Length = col_double(),
  Petal.Width = col_double(),
  Species = col_factor(c("setosa", "versicolor", "virginica"))
))
read_csv("iris.csv", col_types = list(
  Species = col_factor(c("setosa", "versicolor", "virginica"))
)
