activity_labels <- read.table("activity_labels.txt", header = FALSE)
features <- read.table("features.txt", header = FALSE)

subject_test <- read.table("test/subject_test.txt", header = FALSE)
colnames(subject_test) <- "subjects"

x_test <- read.table("test/X_test.txt", header = FALSE)
y_test <- read.table("test/y_test.txt", header = FALSE)
colnames(y_test) <- "tests"

subject_train <- read.table("train/subject_train.txt", header = FALSE)
colnames(subject_train) <- "subjects"

x_train <- read.table("train/X_train.txt", header = FALSE)
y_train <- read.table("train/y_train.txt", header = FALSE)
colnames(y_train) <- "tests"

features1 <- features[,2] ## extracts the features as a list
x_test_labelled <- x_test
colnames(x_test_labelled) <- features1 ## labels x_test columns using features
x_train_labelled <- x_train
colnames(x_train_labelled) <- features1 ## labels x_train columns using features

means_stds_measures <- grep("mean[^F]|std", features1, value = FALSE)
## extracts labels containing "mean" or "std", but excludes "meanFreq"
x_test_meanstd <- x_test_labelled[, means_stds_measures]
x_train_meanstd <- x_train_labelled[, means_stds_measures]
## subsets x_test and x_train to only measurements on mean and std

test_set <- cbind(subject_test, y_test, x_test_meanstd)
train_set <- cbind(subject_train, y_train, x_train_meanstd)
test_train <- rbind(test_set, train_set)
## merges the training and the test sets

test_train_activitylabelled <- merge(test_train, activity_labels, 
                                     by.x = "tests", by.y = "V1", all = TRUE)
## name the activities ("tests" column) in the data set using activity labels to 
## add a column with the descriptions of the activities ("V1" column)
colnames(test_train_activitylabelled)[colnames(test_train_activitylabelled) == "V2"] <- 
        "activity"
## renames the column "activity"
test_train_activitylabelled <- 
        subset(test_train_activitylabelled, select = c(1:2, 69, 3:68))
## re-arranges column sequence
test_train_ordered <- 
        test_train_activitylabelled[order(test_train_activitylabelled$subjects, 
                                          test_train_activitylabelled$tests),]
## order according to subjects then activities

options(warn = - 1) ## supresses warnings due to char values
avg_sub_act <- aggregate(test_train_ordered, 
                         by = list(test_train_ordered$tests, 
                                   test_train_ordered$subjects), 
                         FUN = "mean")
## creates a second, independent tidy data set with the average of each variable
## for each activity and each subject.