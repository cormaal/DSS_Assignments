## For the assignment this file should be renamed run_analysis.R

library(dplyr)


feature_names <- read.table("./data/UCI_HAR_Dataset/features.txt")
features <- feature_names$V2
features <- gsub("^t", "time.", features)
features <- gsub("^f", "freq.", features)


subject_test <- read.table("./data/UCI_HAR_Dataset/test/subject_test.txt")
colnames(subject_test) <- "subject"

subject_train <- read.table("./data/UCI_HAR_Dataset/train/subject_train.txt")
colnames(subject_train) <- "subject"

subject <- rbind(subject_test, subject_train)


activity_test <- read.table("./data/UCI_HAR_Dataset/test/y_test.txt")
colnames(activity_test) <- "activity"

activity_train <- read.table("./data/UCI_HAR_Dataset/train/y_train.txt")
colnames(activity_train) <- "activity"

activities <- rbind(activity_test, activity_train)

activities$activity[activities$activity == 1] <- "walking"
activities$activity[activities$activity == 2] <- "walking_upstairs"
activities$activity[activities$activity == 3] <- "walking_downstairs"
activities$activity[activities$activity == 4] <- "sitting"
activities$activity[activities$activity == 5] <- "standing"
activities$activity[activities$activity == 6] <- "lying"


test_data <- read.table("./data/UCI_HAR_Dataset/test/X_test.txt")
colnames(test_data) <- features

train_data <- read.table("./data/UCI_HAR_Dataset/train/X_train.txt")
colnames(train_data) <- features

all_data <- rbind(test_data, train_data)


complete_dataset <- bind_cols(subject, activities, all_data) %>%
        select(subject, activity, contains("mean()"), contains("std()"))
        
        
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



      