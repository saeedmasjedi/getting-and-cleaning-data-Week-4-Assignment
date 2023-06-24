library(dplyr)

### 0 - get data
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

### 1 - Merges the training and the test sets to create one data set.
## read data
# train
X_train <- read.table('UCI HAR Dataset/train/X_train.txt')
y_train <- read.table('UCI HAR Dataset/train/y_train.txt')
subject_train <- read.table('UCI HAR Dataset/train/subject_train.txt')

# test
X_test <- read.table('UCI HAR Dataset/test/X_test.txt')
y_test <- read.table('UCI HAR Dataset/test/y_test.txt')
subject_test <- read.table('UCI HAR Dataset/test/subject_test.txt')

# features
features <- read.table('UCI HAR Dataset/features.txt')

# activity labels
activity_labels = read.table('UCI HAR Dataset/activity_labels.txt')


## feature names
colnames(X_train) <- features[,2]
colnames(y_train) <- 'ActivityID'
colnames(subject_train) <- 'SubjectID'

colnames(X_test) <- features[,2]
colnames(y_test) <- 'ActivityID'
colnames(subject_test) <- 'SubjectID'

colnames(activity_labels) <- c('ActivityID', 'ActivityType')

## merge columns
train_ds <- cbind(X_train, subject_train, y_train)
test_ds <- cbind(X_test, subject_test, y_test)

ds <- rbind(train_ds, test_ds)

### 2 - Extracts only the measurements on the mean and standard deviation for 
###     each measurement. 
col_names <- colnames(ds)
selected_features <- (grepl("ActivityID", col_names) | grepl("SubjectID", col_names)|
                      grepl("mean..", col_names) | grepl("std...", col_names))

sub_ds <- ds[, selected_features]

### 3 - Uses descriptive activity names to name the activities in the data set
sub_ds_with_activity_names <- merge(sub_ds, activity_labels,
                                    by='ActivityID',
                                    all.x = TRUE)

### 4 - Appropriately labels the data set with descriptive variable names. 
## all samples have labels


### 5 - From the data set in step 4, creates a second, independent tidy data set
###     with the average of each variable for each activity and each subject.
sub_ds_Means <- sub_ds_with_activity_names %>% 
                group_by(SubjectID, ActivityType) %>%
                summarise_each(funs(mean))

write.table(sub_ds_Means, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
