# 
# Getting and Cleaning Data - Course Project
#
# run_analysis.R
# Kenneth Linton, 23-Nov-14
#

# You should create one R script called run_analysis.R that does the following:
#   1. Merges the training and the test sets to create one data set
#   2. Extracts only the measurements on the mean and standard deviation for each measurement
#   3. Uses descriptive activity names to name the activities in the data set
#   4. Appropriately labels the data set with descriptive variable names
#   5. From the data set in step 4, creates a second, independent tidy data set 
#      - with the average of each variable for each activity and each subject

# constants
ACTIVITIES = c("activity_id", "activity_label")

# load required packages
if (!require("data.table")){ install.packages("data.table") }
if (!require("reshape2"))  { install.packages("reshape2")   }

# load 'activity labels' and 'features', selecting labels (column 2) only
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
features        <- read.table("./UCI HAR Dataset/features.txt")[,2]

# TEST: load and process

# load 'X_test', 'y_test' and 'subject_test'
X_test          <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test          <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test    <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# features: extract only mean() and std() - drop meanFreq()
extract_features <- grepl("mean\\(\\)|std\\(\\)", features)

# X_test: name features, and extract only mean and std
names(X_test)   <- features
X_test          <- X_test[, extract_features]

# y_test: import activity labels
y_test[, 2]     <- activity_labels[y_test[, 1]]
names(y_test)   <- ACTIVITIES

# subject_test: set name of column
names(subject_test) <- "subject"

# column-bind subject_test, y_test and X_test into test_data
test_data <- cbind(as.data.table(subject_test), y_test, X_test)

# TRAIN: load and process

# load 'X_train', 'y_train' and 'subject_train'
X_train         <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train         <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train   <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# X_train: name features, and extract only mean and std
names(X_train)  <- features
X_train         <- X_train[, extract_features]

# y_train: import activity labels
y_train[, 2]     <- activity_labels[y_train[, 1]]
names(y_train)   <- ACTIVITIES

# subject_train: set name of column
names(subject_train) <- "subject"

# column-bind subject_train, y_train and X_train into train_data
train_data <- cbind(as.data.table(subject_train), y_train, X_train)

# MERGE

# merge test and train data
data = rbind(test_data, train_data)

# LABEL

# label merged data-set
id_labels   <- c("subject", ACTIVITIES)
data_labels <- setdiff(colnames(data), id_labels)

# melt data: variables are either id variables or measured variables
melt_data <- melt(data, id=id_labels, measure.vars = data_labels)

# MEAN & WRITE

# calculate average for each activity and each subject & write it out
tidy_data <- dcast(melt_data, subject + activity_label ~ variable, mean)
write.table(tidy_data, file="./tidy_data.txt", row.name=FALSE)
