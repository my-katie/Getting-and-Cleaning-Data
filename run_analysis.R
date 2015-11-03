# Getting and Cleaning data Assignment
# Data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# Create one R script called run_analysis.R that does the following.  
# 1) Merges the training and the test sets to create one data set. 
# 2) Extracts only the measurements on the mean and standard deviation for each measurement
# 3) Uses descriptive activity names to name the activities in the data set 
# 4) Appropriately labels the data set with descriptive variable names.  
# 5) Creates a second, independent tidy data set with the average of each variable for 
#    each activity and each subject.  

setwd("C:/Users/leeks0/Desktop/DataScience")

# Load libraries
library(data.table)
library(dplyr)

# Read supporting metadata
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# Format training and test data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# Combine training and test data
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# Name the columns
colnames(features) <- t(featureNames[2])

# Merge the data (Q1)
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

# Extracts only measurements with mean or standard in them (Q2)
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

# Add activity and subject columns to the list
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

# Create extractedData with the requiredColumns
extractedData <- completeData[,requiredColumns]
dim(extractedData)

# Use descriptive activity names to name the activities in the data set. (Q3)
# The activity field in extractedData is originally of numeric type. 
# We need to change its type to character so that it can accept activity names. 
# The activity names are taken from metadata activityLabels.

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

# Label the data set with descriptive variable name appropriately (Q4)
names(extractedData)

# Replaces the acronyms in extractedData
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

names(extractedData)

# Create a second independent tidy data set with the average of each variable 
# for each activity and each subject
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "TidyData.txt", row.names = FALSE)
