require (plyr)

#function add suffix
addSuffix<- function(x, suffix) {
  if (!(x %in% c("Subject","Activity"))) {
    paste(x,suffix, sep="")
  }
  else{
    x
  }
}

# retrieve data

pathfile<-file.path(getwd(),"UCI HAR Dataset")

pathfiletest<-file.path(pathfile, "test")
pathfiletrain<-file.path(pathfile, "train")

xtest<-read.table(file.path(pathfiletest,"X_test.txt"))
ytest<-read.table(file.path(pathfiletest,"Y_test.txt"))
subjecttest<-read.table(file.path(pathfiletest,"subject_test.txt"))

xtrain<-read.table(file.path(pathfiletrain,"X_train.txt"))
ytrain<-read.table(file.path(pathfiletrain,"Y_train.txt"))
subjecttrain<-read.table(file.path(pathfiletrain,"subject_train.txt"))

#retrieve activity labels 
activitylabels<-read.table(file.path(pathfile,
                                     "activity_labels.txt"),
                           col.names = c("Id", "Activity")
)

#retrieve features labels
featurelabels<-read.table(file.path(pathfile,
                                    "features.txt"),
                          colClasses = c("character")
)

# Step 1 Merges the training and the test sets 
traindata<-cbind(cbind(xtrain, subjecttrain), ytrain)
testdata<-cbind(cbind(xtest, subjecttest), ytest)
sensordata<-rbind(traindata, testdata)

sensorlabels<-rbind(rbind(featurelabels, c(562, "Subject")), c(563, "Id"))[,2]
names(sensordata)<-sensorlabels

# Step 2 Extracts only the measurements on the mean and standard deviation 
sensordatameanstd <- sensordata[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(sensordata))]

# Step 3 descriptive activity names to name the activities in the data set
sensordatameanstd <- join(sensordatameanstd, activitylabels, by = "Id", match = "first")
sensordatameanstd <- sensordatameanstd[,-1]

# Step 4 labeling the data set with descriptive names.
names(sensordatameanstd) <- gsub("([()])","",names(sensordatameanstd))
#norm names
names(sensordatameanstd) <- make.names(names(sensordatameanstd))

# Step 5 from dataset in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject 
finaldata<-ddply(sensordatameanstd, c("Subject","Activity"), numcolwise(mean))
#improve column names
finaldataheaders<-names(finaldata)
finaldataheaders<-sapply(finaldataheaders, addSuffix, ".mean")
names(finaldata)<-finaldataheaders

# write file
write.table(finaldata, file = "tidy_dataset.txt", row.name=FALSE)
 
