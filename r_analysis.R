
## This R script use the data collected from the accelerometers from the Samsung Galaxy S smartphone and make a clean tidy dataset.


library(dplyr)


##############################################################################
# STEP 1 - Get data and merge the training and test data sets
##############################################################################

# download zip file containing data if it hasn't already been downloaded
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

filepath <- "UCI HAR Dataset"
if (!file.exists(filepath)) {
  unzip(zipFile)
}

# read training data
trainingSubjects <- read.table(file.path(filepath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(filepath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(filepath, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(filepath, "test", "subject_test.txt"))
testValues <- read.table(file.path(filepath, "test", "X_test.txt"))
testActivity <- read.table(file.path(filepath, "test", "y_test.txt"))

# read features, don't convert text labels to factors
features <- read.table(file.path(filepath, "features.txt"), as.is = TRUE)


# read activity labels
activities <- read.table(file.path(filepath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

activity_sum <- rbind(
  cbind(trainingSubjects,trainingValues,trainingActivity),
  cbind(testSubjects,testValues,testActivity)
)

colnames(activity_sum) <- c("subject",features[,2],"activity")

##############################################################################

## step 2 - Extract only mean and standard deviation

columns_std_mean <- grepl("subject|activity|mean|std",colnames(activity_sum))

activity_sum <- activity_sum[,columns_std_mean]

##############################################################################


##############################################################################

## step 3 - Uses descriptive activity names to name the activities in the data set

##############################################################################


activity_sum$activity <- factor(activity_sum$activity, levels = activities[,1], labels = activities)


##############################################################################

##step 4 Appropriately labels the data set with descriptive variable names.

##############################################################################

activity_sum_cols <- colnames(activity_sum)

activity_sum_cols <- gsub("[\\(\\)-]", "", activity_sum_cols)

## expand abbreviations and clean up names
activity_sum_cols <- gsub("^f", "frequencyDomain", activity_sum_cols)
activity_sum_cols <- gsub("^t", "timeDomain", activity_sum_cols)
activity_sum_cols <- gsub("Acc", "Accelerometer", activity_sum_cols)
activity_sum_cols <- gsub("Gyro", "Gyroscope", activity_sum_cols)
activity_sum_cols <- gsub("Mag", "Magnitude", activity_sum_cols)
activity_sum_cols <- gsub("Freq", "Frequency", activity_sum_cols)
activity_sum_cols <- gsub("mean", "Mean", activity_sum_cols)
activity_sum_cols <- gsub("std", "StandardDeviation", activity_sum_cols)

activity_sum_cols <- gsub("BodyBody", "Body", activity_sum_cols)

colnames(activity_sum) <- activity_sum_cols

##############################################################################

## step 5 -From the data set in step 4, creates a second, independent tidy data set with the average of 
##each variable for each activity and each subject.

##############################################################################

activity_mean <- activity_sum %>%
  group_by(subject,activity) %>%
  summarise_all(funs(mean))

## Extract the tidy data to a .txt format

write.table(activity_mean,"tidy_data.txt", row.names = FALSE,quote = FALSE )


