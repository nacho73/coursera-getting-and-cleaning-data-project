library(dplyr)

# Download and unzip file.

zUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

zFile <- "UCI HAR Dataset.zip"

if (!file.exists(zFile)) {
  
  download.file(zUrl, zFile, mode = "wb")
  
}

dataPath <- "UCI HAR Dataset"

if (!file.exists(dataPath)) {
  
  unzip(zFile)
  
} 

# Read files as tables and assign these to variables

trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))

testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))

trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt")) 

testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))

trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))  

testActivity <- read.table(file.path(dataPath, "test", "y_test.txt")) 

features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE) 

activities <- read.table(file.path(dataPath, "activity_labels.txt"))

colnames(activities) <- c("activityId", "activityLabel")

# Step 1 - Merge the training and the test sets to create one data set

training_all<-cbind(trainingSubjects,  trainingValues, trainingActivity)

test_all<-cbind(testSubjects, testValues, testActivity)

userActivity<-rbind(training_all,test_all) 

colnames(userActivity) <- c("subject", features[, 2], "activity")


# Step 2 - Extract only the measurements on the mean and standard deviation for each measurement

Mean_and_std <- grepl("subject|activity|mean|std", colnames(userActivity))   

userActivity <- userActivity[, Mean_and_std]


# Step 3 - Use descriptive activity names to name the activities in the data set   

userActivity$activity <- factor(userActivity$activity, 
                                
                                levels = activities[, 1], labels = activities[, 2])
                                

# Step 4 - Appropriately label the data set with descriptive variable names

# get column names

userActivityCols <- colnames(userActivity)

# remove special characters

userActivityCols <- gsub("[\\(\\)-]", "", userActivityCols)

# expand abbreviations

userActivityCols <- gsub("tBody", "TimeBody", userActivityCols)

userActivityCols <- gsub("tGravity", "TimeGravity", userActivityCols)

userActivityCols <- gsub("fBody", "FrequencyBody", userActivityCols)

userActivityCols <- gsub("Acc", "Accelerometer", userActivityCols)

userActivityCols <- gsub("Gyro", "Gyroscope", userActivityCols)

userActivityCols <- gsub("Mag", "Magnitude", userActivityCols)

userActivityCols <- gsub("std", "StandardDeviation", userActivityCols)

# More new labels

userActivityCols <- gsub("mean", "Mean", userActivityCols)
userActivityCols <- gsub("BodyBody", "Body", userActivityCols)

# use new labels as column names

colnames(userActivity) <- userActivityCols


# Step 5 - Create a second, independent tidy set with the average of each variable for each activity       
# and each subject

userActivityMeans <- userActivity %>% 
  
  group_by(subject, activity) %>%
  
  summarise_all(funs(mean)) 

write.table(userActivityMeans, "tidy_data.txt", row.names = FALSE, 
            
            quote = FALSE)
