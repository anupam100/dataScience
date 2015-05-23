# reading various files both test and train data. I have unziped these files in my current working directroty

train.df<-read.table("X_train.txt")
trainLabel.df<-read.table("y_train.txt")
table(trainLabel.df)
trainSubject.df <- read.table("subject_train.txt")
testData.df <- read.table("X_test.txt")
testLabel.df <- read.table("y_test.txt") 
table(testLabel.df) 
testSubject.df <- read.table("subject_test.txt")
# binding the test and train data
mergeddata.df <- rbind(train.df, testData.df)
joinLabel.df <- rbind(trainLabel.df, testLabel.df)
joinSubject.df <- rbind(trainSubject.df, testSubject.df)


# taking mean and std deviation from feature

features.df <- read.table("features.txt")

meanSearch <- grep("mean\\(\\)|std\\(\\)", features.df[, 2])
mergeddata.df <- mergeddata.df[, meanSearch]



names(mergeddata.df) <- gsub("\\(\\)", "", features.df[meanSearch, 2]) 
names(mergeddata.df) <- gsub("mean", "Mean", names(mergeddata.df)) 
names(mergeddata.df) <- gsub("std", "Std", names(mergeddata.df)) 
names(mergeddata.df) <- gsub("-", "", names(mergeddata.df)) 

# change the column name to one mentioned in activity

activity <- read.table("activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel.df[, 1], 2]
joinLabel.df[, 1] <- activityLabel
names(joinLabel.df) <- "activity"

 
names(joinSubject.df) <- "subject"
cleanedData <- cbind(joinSubject.df, joinLabel.df, mergeddata.df)
dim(cleanedData) # 10299*68
write.table(cleanedData, "merged_data.txt")

 
subjectLen <- length(table(joinSubject.df)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject.df)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}

write.table(result, "output.txt", row.names = FALSE)

