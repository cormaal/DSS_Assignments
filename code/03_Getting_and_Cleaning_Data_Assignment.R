## For the assignment this file should be renamed run_analysis.R

library(dplyr)


## Read in the list of variables and create a vector of variable names
## Make some of the variable names more descriptive

feature_names <- read.table("./data/UCI_HAR_Dataset/features.txt")
features <- feature_names$V2
features <- gsub("^t", "time.", features)
features <- gsub("^f", "freq.", features)


## Read in the subject lists from the test and train data
## Row bind the two dataframes and add the variable name

subject_test <- read.table("./data/UCI_HAR_Dataset/test/subject_test.txt")
subject_train <- read.table("./data/UCI_HAR_Dataset/train/subject_train.txt")
subjects <- rbind(subject_test, subject_train)
colnames(subjects) <- "subject"


## Read in the activity lists from the test and train data
## Row bind the two dataframes and add the variable name
## Make the variable data descriptive

activity_test <- read.table("./data/UCI_HAR_Dataset/test/y_test.txt")
activity_train <- read.table("./data/UCI_HAR_Dataset/train/y_train.txt")
activities <- rbind(activity_test, activity_train)
colnames(activities) <- "activity"

activities$activity[activities$activity == 1] <- "walking"
activities$activity[activities$activity == 2] <- "walking_upstairs"
activities$activity[activities$activity == 3] <- "walking_downstairs"
activities$activity[activities$activity == 4] <- "sitting"
activities$activity[activities$activity == 5] <- "standing"
activities$activity[activities$activity == 6] <- "lying"


## Read in the test and train data
## Row bind the two dataframes and add the variable name (i.e. features vector)

test_measurements <- read.table("./data/UCI_HAR_Dataset/test/X_test.txt")
train_measurements <- read.table("./data/UCI_HAR_Dataset/train/X_train.txt")
all_measurements <- rbind(test_measurements, train_measurements)
colnames(all_measurements) <- features


## Bind the three dataframes into a complete dataset
## Select only the measurements on the mean and standard deviation for each measurement

complete_dataset <- bind_cols(subjects, activities, all_measurements) %>%
        select(subject, activity, contains("mean()"), contains("std()"))

## Group the data by subject and then by activity
## Summarise the data by calculating the mean value for all variables in the grouped data

grouped_dataset <- group_by(complete_dataset, subject, activity) %>%
        summarise(across("time.BodyAcc-mean()-X":"freq.BodyBodyGyroJerkMag-std()", mean))
        



# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)



      