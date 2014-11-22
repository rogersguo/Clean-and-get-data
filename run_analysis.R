## Script cleans sensor data from Galaxy S device for analysis.
## 1. Merges the training and test sets to create "master" data set.
## 2. Cleans all column names, reformatting several.
## 3. Decodes activity label by showing the corresponding activity name.
## 4. Outputs that file as "masterData.txt".
## 5. Calculates average for 70 key mean and std dev variables. 
## 6. Outputs calcs by activity and subject as "activitySubjectMeans.txt".

## Load required libraries.
library(plyr)

## If data directory exists, change to that directory.
## Else, create data directory, download and unzip raw data files.
if(file.exists("C:/Users/Jian/Documents/R")) {
  setwd("C:/Users/Jian/Documents/R")
} else {
  dir.create("C:/Users/Jian/Documents/R")
  fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile="zipfile.zip")
  setwd("C:/Users/Jian/Documents/R")
  unzip("zipfile.zip", files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = ".", unzip = "internal",
        setTimes = FALSE)
}

## Read raw data files into memory for processing.
trainSubj<-read.table("C:/Users/Jian/Documents/R/UCI HAR Dataset/train/subject_train.txt",header=FALSE)
trainY<-read.table("C:/Users/Jian/Documents/R/UCI HAR Dataset/train/y_train.txt",header=FALSE)
trainX<-read.table("C:/Users/Jian/Documents/R/UCI HAR Dataset/train/X_train.txt",header=FALSE)
testSubj<-read.table("C:/Users/Jian/Documents/R/UCI HAR Dataset/test/subject_test.txt",header=FALSE)
testY<-read.table("C:/Users/Jian/Documents/R/UCI HAR Dataset/test/y_test.txt",header=FALSE)
testX<-read.table("C:/Users/Jian/Documents/R/UCI HAR Dataset/test/X_test.txt",header=FALSE)
features<-read.table("C:/Users/Jian/Documents/R/UCI HAR Dataset/features.txt",header=FALSE)
activityLabels<-read.table("C:/Users/Jian/Documents/R/UCI HAR Dataset/activity_labels.txt",header=FALSE)

## Join train and test files.
subjects<-rbind(trainSubj,testSubj)
activities<-rbind(trainY,testY)
readings<-rbind(trainX,testX)

## Set counter maximums for later.
tmp<-count(unique(subjects)); subjectNum<-sum(tmp$freq)
tmp<-count(unique(activityLabels)); labelNum<-sum(tmp$freq)

## Decode activity label and put result in new activity name column.
activities$activityName<-activityLabels[activities$V1,2]

## Clean feature names removing problematic text
cleanFeatures<-gsub("([()])", "", features[,2])
cleanFeatures<-gsub("([-])","",cleanFeatures)
cleanFeatures<-gsub("([,])","",cleanFeatures)
colnames(readings)<-cleanFeatures

## Join columns to create master file renaming some columns.
master<-cbind(activities,subjects)
colnames(master)<-c("activityLabel","activityName","activitySubject")
master<-cbind(master,readings[,41:46],readings[,214:215],readings[,253:254],
              readings[,266:271],readings[,345:350],readings[,424:429],
              readings[,503:504],readings[,516:517],
              readings[,529:530],readings[,542:543])

## Remove working files
rm(subjects,activities,readings,features,cleanFeatures)

## Sort master - used for debugging master file creation only.
master<-master[order(master$activitySubject,desc(master$activityLabel)),]

## Initialize return matrix for processing.
output<-matrix(NA,labelNum*subjectNum,length(master))
output<-as.data.frame(output)
colnames(output)<-names(master)
outputCtr<-1

## Couldn't find an easy way to melt/dcast or data.table/.SD given the
for(i in 1:labelNum){
  tmpAct<-master[master$activityLabel==i,]
  for(j in 1:subjectNum){
    tmpActSubj<-tmpAct[tmpAct$activitySubject==j,]
    output[outputCtr,1]<-i
    output[outputCtr,2]<-as.character(activityLabels[i,2])
    output[outputCtr,3]<-j
    for(k in 1:(length(master)-3)){
      output[outputCtr,k+3]<-mean(tmpActSubj[,k+3])
    }
    outputCtr<-outputCtr+1
  }
}
## Save output files.
write.table(output,"tidy.txt",row.names=F,col.names=F) 

