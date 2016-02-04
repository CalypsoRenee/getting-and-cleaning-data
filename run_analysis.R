## Getting & Cleaning Data - Week 4 Assignment

########################################################################################
# Setup Environment & Get Data
########################################################################################

#Load some handy packages
library(plyr)
library(dplyr)
library(reshape2)

#Set working directory
setwd("~/Desktop/DataScienceSpecialization/GettingAndCleaningData/Assignment/getting-and-cleaning-data")

#Download data set
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/dataset.zip",method="curl")
#Unzip file
unzip(zipfile="./data/dataset.zip",exdir="./data")
#Get list of files
files <- list.files("./data/UCI HAR Dataset")
files

#Read in all Activity files
activityTest <- read.table("./data/UCI HAR Dataset/test/y_test.txt",header=FALSE, quote="\"")
activityTrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt",header = FALSE, quote="\"")
#Look into Activity variables
str(activityTest)
str(activityTrain)

#Read in all Subject files
subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", header=FALSE, quote="\"")
subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", header=FALSE, quote="\"")
#Look into Subject variables
str(subjectTest)
str(subjectTrain)

#Read in all Features files
featuresTest <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header=FALSE, quote="\"")
featuresTrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header=FALSE, quote="\"")
#Look into Features variables
str(featuresTest)
str(featuresTrain)

########################################################################################
# Merge the training and the test sets to create one data set.
########################################################################################

#Merge each of the above sets together & assign variable names
mergeActivity <- rbind(activityTest, activityTrain)
names(mergeActivity) <- c("Activity")
head(mergeActivity)
mergeSubject <- rbind(subjectTest, subjectTrain)
names(mergeSubject) <- c("Subject")
head(mergeSubject)
mergeFeatures <- rbind(featuresTest, featuresTrain)
#read in file for features variable labels & assign mergeFeatures variable names
featuresLabels <- read.table("./data/UCI HAR Dataset/features.txt", header=FALSE, quote="\"")
head(featuresLabels)
names(mergeFeatures) <- featuresLabels$V2
head(mergeFeatures)

#Merge all 3 sets to create data frame 
mergeSubjAct <- cbind(mergeSubject, mergeActivity)
head(mergeSubjAct)
mergeAllData <- cbind(mergeSubjAct, mergeFeatures)
head(mergeAllData)

########################################################################################
# Extracts only the measurements on the mean and standard deviation for each measurement
########################################################################################

#Create subset 
subsetAllData <- mergeAllData[,grepl("Subject|Activity|mean\\(\\)|std\\(\\)", names(mergeAllData))] 
head(subsetAllData)
str(subsetAllData)
colNames <- colnames(subsetAllData)
colNames

########################################################################################
# Use descriptive activity names to name the activities in the data set
########################################################################################

#Read in the descriptive activity label names from activity_labels.txt file & fix labels
activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header=FALSE, quote="\"")
head(activityLabels)
activityLabels <- rename(activityLabels, c("V1"="Activity", "V2"="ActivityLabel"))
#Factorize Activity column & Merge activity names with subsetAllData
subsetAllData$Activity <- factor(subsetAllData$Activity, labels = activityLabels[,2])
head(subsetAllData)

########################################################################################
# Appropriately label the data set with descriptive variable names
########################################################################################

#Clean up variable names
names(subsetAllData)<-gsub("^t", "time", names(subsetAllData))
names(subsetAllData)<-gsub("^f", "frequency", names(subsetAllData))
names(subsetAllData)<-gsub("Acc", "Accelerometer", names(subsetAllData))
names(subsetAllData)<-gsub("Gyro", "Gyroscope", names(subsetAllData))
names(subsetAllData)<-gsub("Mag", "Magnitude", names(subsetAllData))
names(subsetAllData)<-gsub("BodyBody", "Body", names(subsetAllData))
names(subsetAllData)<-gsub("-mean", "Mean", names(subsetAllData))
names(subsetAllData)<-gsub("-std", "Std", names(subsetAllData))
names(subsetAllData)<-gsub("\\()", "", names(subsetAllData))

########################################################################################
# Create 2nd,independent data set with averages of each variable for each 
# activity and each subject
########################################################################################

#Melt data frame to get means (long-form data frame)
meltData <- melt(subsetAllData, id = c("Subject", "Activity"), variable.name="Measurement", value.name = "Value")
head(meltData)

#Create summary data frame & compute means (wide-form tidy data frame)
merged_summary <- dcast(meltData, Subject + Activity ~ Measurement,
                        mean, value.var = "Value")
head(merged_summary)

#Write tidy data set to txt file
write.table(merged_summary, "tidydata.txt", row.names = FALSE, sep = "\t")







