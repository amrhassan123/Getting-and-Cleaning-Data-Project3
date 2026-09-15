dataset_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip_file <- "UCI_HAR_Dataset.zip"

if (!file.exists(zip_file)) {
        download.file(dataset_url, destfile = zip_file, method = "curl")
}

if (!file.exists("UCI HAR Dataset")) {
        unzip(zip_file)
}


library(dplyr)

# ==========================================
# STEP 0: Load Raw Data Files
# ==========================================
data_dir <- "UCI HAR Dataset"

# Load feature names and activity labels
features <- read.table(file.path(data_dir, "features.txt"), col.names = c("index", "feature"))
activities <- read.table(file.path(data_dir, "activity_labels.txt"), col.names = c("code", "activity"))

# Load training data
subject_train <- read.table(file.path(data_dir, "train", "subject_train.txt"), col.names = "subject")
x_train       <- read.table(file.path(data_dir, "train", "X_train.txt"), col.names = features$feature)
y_train       <- read.table(file.path(data_dir, "train", "y_train.txt"), col.names = "code")

# Load test data
subject_test  <- read.table(file.path(data_dir, "test", "subject_test.txt"), col.names = "subject")
x_test        <- read.table(file.path(data_dir, "test", "X_test.txt"), col.names = features$feature)
y_test        <- read.table(file.path(data_dir, "test", "y_test.txt"), col.names = "code")


# ==========================================
# STEP 1: Merge training & test sets
# ==========================================
x_merged       <- rbind(x_train, x_test)
y_merged       <- rbind(y_train, y_test)
subject_merged <- rbind(subject_train, subject_test)

merged_data    <- cbind(subject_merged, y_merged, x_merged)


# ==========================================
# STEP 2: Extract mean & standard deviation measurements
# ==========================================
tidy_extracted <- merged_data %>%
        select(subject, code, matches("mean\\.\\.|std\\.\\."))


# ==========================================
# STEP 3: Use descriptive activity names
# ==========================================
tidy_extracted$code <- activities$activity[match(tidy_extracted$code, activities$code)]
tidy_extracted <- rename(tidy_extracted, activity = code)


# ==========================================
# STEP 4: Appropriately label dataset with descriptive variable names
# ==========================================
names(tidy_extracted) <- names(tidy_extracted) %>%
        gsub("^t", "Time", .) %>%
        gsub("^f", "Frequency", .) %>%
        gsub("Acc", "Accelerometer", .) %>%
        gsub("Gyro", "Gyroscope", .) %>%
        gsub("Mag", "Magnitude", .) %>%
        gsub("BodyBody", "Body", .) %>%
        gsub("\\.mean\\.\\.", "Mean", .) %>%
        gsub("\\.std\\.\\.", "STD", .) %>%
        gsub("\\.", "", .)


# ==========================================
# STEP 5: Create 2nd independent tidy data set (averages per subject & activity)
# ==========================================
final_tidy_data <- tidy_extracted %>%
        group_by(subject, activity) %>%
        summarise(across(everything(), mean), .groups = "drop")

# Export output file for submission
write.table(final_tidy_data, "tidy_dataset.txt", row.name = FALSE)