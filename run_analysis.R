run_analysis <- function()
{
    library(dplyr)

# read in data and labels
    subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
    y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
    X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
    subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
    y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
    X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
    features <- read.table("UCI HAR Dataset/features.txt")

# merge the two datasets
    subject <- rbind(subject_train, subject_test)
    y <- rbind(y_train, y_test)
    X <- rbind(X_train, X_test)

#label columns of y (activities) and subject
    colnames(subject) <- "subject"
    colnames(y) <- "activities"

# selecting only the columns with mean and standard deviation
    sel_mean <- features[grep("mean()", features$V2), ]
    sel_std <- features[grep("std()", features$V2), ]
    selection <- rbind(sel_mean, sel_std)
    X_sel <- X[,selection$V1]

# label columns of X_sel
    colnames(X_sel) <- selection$V2

# add subject numbers and activity columns to the main data and order according to subject
    data <- cbind(subject, y, X_sel)
    data <- data[order(data$subject),]

# relabel the activity-factors
    data[,2] <- revalue(as.character(data[,2]), c("1"="Walking", "2"="Walking upstairs", "3"="Walking downstairs", "4"="Sitting", "5"="Standing", "6"="Laying"))

#calculate mean per subject per activity
    tidy_data <- ddply(data, .(subject, activities), colwise(mean))

# return dataset
    tidy_data
}
