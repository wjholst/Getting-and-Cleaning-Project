#
# This script, run_analysis.R,  does the following. 
#  - Merges the training and the test sets to create one data set.
#  - Extracts only the measurements on the mean and standard deviation for each measurement. 
#  - Uses descriptive activity names to name the activities in the data set
#  - Appropriately labels the data set with descriptive variable names. 
#  - From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject.

# Establish the data folder and download the zip file

zipfile<- "zipfile.zip"
if (!file.exists(zipfile)) {

    fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl,destfile=zipfile)

    dateDownloaded<-date()
    dateDownloaded
    # 
    # Extract the downloaded zip file
    #
    unzip(zipfile)
}
    
#  files manually moved from the download zip file to the extracted folder "UCI HAR Dataset"
#
#  Load the training and test sets into memory
#
#  
# 
# Load data into memory

xtraining<-read.table("UCI HAR Dataset/train/X_train.txt")
xtest<-read.table("UCI HAR Dataset/test/X_test.txt")
ytraining<-read.table("UCI HAR Dataset/train/y_train.txt")
ytest<-read.table("UCI HAR Dataset/test/y_test.txt")
features<-read.table("UCI HAR Dataset/features.txt")
subjectTrain<-read.table("UCI HAR Dataset/train/subject_train.txt")
subjectTest<-read.table("UCI HAR Dataset/test/subject_test.txt")
activity<-read.table("UCI HAR Dataset/activity_labels.txt")

# select the features that are either mean or standard deviation and
# create labels for column heads

featuresToRead <- grep(".*mean.*|.*std.*", features[,2])
featureLabels <- features[featuresToRead,2]
# 
# Cleanup for readability
#
featureLabels <-gsub("()","",featureLabels,fixed=TRUE)
featureLabels <-gsub("mean"," Mean ",featureLabels)
featureLabels <-gsub("std"," StDev ",featureLabels)

# get the mean and std features only

trainingFeatures<-xtraining[,featuresToRead]
testFeatures<-xtest[,featuresToRead]

# create complete training and test arrays and combine them

testSet<-cbind(subjectTest,ytest,testFeatures)
trainingSet<-cbind(subjectTrain,ytraining,trainingFeatures)
combinedSet<-rbind(trainingSet,testSet)

# assign column names to combined set

names(combinedSet)<-c("Subject","Activity",featureLabels)

# substititute the activity name for activityID

combinedSet[,2]<-activity[combinedSet[,2],2]
head(combinedSet)

# The combinedSet is now all entries for 30 subjects with 6 activies each
# The final dimension should be 180 by 81 (79 mean variables and the 
# subject and activity groups)

#install.packages("plyr")

library(plyr)

# find the mean of all the variables, grouped by subject and activity
# Note: this is for purpose of this exercise only.  It is mathematically 
# incorrect to calculated the mean of a set of means unless the number of
# original observations in each set is identical, which we have not 

meansSet=ddply(combinedSet, .(Subject, Activity), function(x) colMeans(x[, 3:81]))

# write to a text file

write.csv(meansSet,"combinedMeansTidy.txt",row.names = FALSE, quote = FALSE)
