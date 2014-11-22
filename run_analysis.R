#merge training and test sets

t1 <- read.table("train/X_train.txt")
t2 <- read.table("test/X_test.txt")
X <- rbind(t1, t2)

t1 <- read.table("train/subject_train.txt")
t2 <- read.table("test/subject_test.txt")
S <- rbind(t1, t2)

t1 <- read.table("train/y_train.txt")
t2 <- read.table("test/y_test.txt")
Y <- rbind(t1, t2)

#standard and mean measurements

features <- read.table("features.txt")
goodFeatures <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, goodFeatures]
names(X) <- features[goodFeatures, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

#name activities

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

#label data set

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

#2nd date set w/activity names

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt")