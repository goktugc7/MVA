#Libraries
library(dplyr)
library(missForest)
library(DMwR)
library(mice)
library(BaylorEdPsych)
library(mvnmle)
#Data Reading
DataRusset = read.delim("/Users/goktugcengiz/Desktop/Datasets/Russet_ineqdata2.txt", header = TRUE, sep="\t", dec =".")
#Data Understanding
str(DataRusset)
summary(DataRusset)
apply(DataRusset, 2, function(x) sum(is.na(x))) #Missing Value Detection
md.pattern(DataRusset) #Missing Value Visualization
t = LittleMCAR(DataRusset) 
attributes(t)
t$p.value #0.7910595
#Data Preprocessing
#Missing Value Imputation with RandomForest
DataRusset = missForest(DataRusset)
DataRusset = DataRusset$ximp
DataRusset$ecks = round(DataRusset$demo)
DataRusset$Rent = round(DataRusset$demo)
sum(is.na(DataRusset))
#Defining as X Matrix 
X = matrix(DataRusset)
#principal component analysis
#pca = prcomp(DataRusset, scale. = T)