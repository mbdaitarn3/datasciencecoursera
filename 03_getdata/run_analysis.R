## Programm Assignement:
## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
print(paste("Start Program ",Sys.time()))

## Check if the packages data.table are installed
if (!library("data.table",logical.return = TRUE)) {
  install.packages("data.table")
}

## Check if the packages reshape2 are installed
if (!library("sqldf",logical.return = TRUE)) {
  install.packages("sqldf")
}

## Load the packages data.table and reshape2
library("data.table")
library("sqldf")

# Load: activity labels
## In the activity labels we have 
##		1 WALKING
##		2 WALKING_UPSTAIRS
##		3 WALKING_DOWNSTAIRS
##		4 SITTING
##		5 STANDING
##		6 LAYING

print(paste("Start Elaboration ",Sys.time()))
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

# Load: data column names
## Column Name of the observation
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

# Extract only the measurements on the mean and standard deviation for each measurement.
extract_features <- grepl("mean|std", features)

# Load and process X_test & y_test data.
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")


## y_test contain for each measurement the activity (WALKING, SI_T_TING etc..)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

## subject_test contain for each measurement the number of volunteers
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Add column name on the data.table X_Test
names(X_test) = features

# Extract a subset of the measurements on the mean and standard deviation. This subset is in 
# the vector extract_features <- grepl("mean|std", features)

X_test = X_test[,extract_features]

# Load activity labels
# Add activity Label on data.frame y_test like this:
#     Activity_ID     Activity_Label
#1              5           STANDING
#2              5           STANDING
#3              5           STANDING
#4              5           STANDING
#5              5           STANDING
#6              5           STANDING
#7              5           STANDING
#8              5           STANDING
#9              5           STANDING
#
y_test[,2] = activity_labels[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity_Label")
names(subject_test) = "subject"

# Bind data
# Combine the column number of volunteers with activity and measurement
test_data <- cbind(as.data.table(subject_test), y_test, X_test)
# the result of test_data is like this :

#     subject Activity_ID   Activity_Label tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y tBodyAcc-std()-Z
#   1:       2           5         STANDING         0.2571778       -0.02328523       -0.01465376       -0.9384040      -0.92009078       -0.6676833
#   2:       2           5         STANDING         0.2860267       -0.01316336       -0.11908252       -0.9754147      -0.96745790       -0.9449582
#   3:       2           5         STANDING         0.2754848       -0.02605042       -0.11815167       -0.9938190      -0.96992551       -0.9627480
#   4:       2           5         STANDING         0.2702982       -0.03261387       -0.11752018       -0.9947428      -0.97326761       -0.9670907
#   5:       2           5         STANDING         0.2748330       -0.02784779       -0.12952716       -0.9938525      -0.96744548       -0.9782950
#


# Load and process X_train & y_train data.
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

names(X_train) = features

# Extract only the measurements on the mean and standard deviation for each measurement.
X_train = X_train[,extract_features]

# Load activity data
y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity_Label")
names(subject_train) = "subject"

# Bind data
train_data <- cbind(as.data.table(subject_train), y_train, X_train)

# End for the training data


#----------------------------------------------------------

# Merge test and train data
data = rbind(test_data, train_data)


# To generate tidy data 
# i wil use the package sqldf with SQL and Group by


 cln  = names(data)
 ncln = length(colnames(data))
 newColNames = c(colnames(data)[1],colnames(data)[3:ncln]) 
 

 cfields=" "
# In the cfields we paste the dynamic sql for every column
for (i in 4:ncln ) {
  if (i==ncln)  {
   cfields = paste(cfields,'avg(\"',cln[i],'\")',sep="")
  } else { 
   cfields = paste(cfields,'avg(\"',cln[i],'\"),',sep="") 
  } 
}


 sqlst = paste('select subject,Activity_Label,',cfields,' from data group by subject,Activity_Label')
 tidy_data2 = sqldf(sqlst)
 names(tidy_data2)=newColNames


write.table(tidy_data2, file = "./tidy_data.txt")

print(paste("End Program ",Sys.time()))