### run_analysis.r

### setwd("Coursera/Data Clean") 
setInternet2(use = TRUE) 
library(reshape2)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 

### Load the data, Labels and Subject info
jdata<-   NULL
jlabel<-   NULL
jsubject<-  NULL

### Unzip the data
zipFile <- tempfile() 
dataDir <- "UCI HAR Dataset" 
if(!file.exists(dataDir)) {
  download.file(url, zipFile) 
  unzip(zipFile, exdir = ".") } 

### Load the data
trainData<- NULL
testData<- NULL
trainLabel<- NULL
testLabel<- NULL
trainSubject<- NULL
testSubject<- NULL

### Create function to read the data
readData <- function(path) { 
  read.table(file.path(dataDir, path)) 
}
### Read Training and Test data  for Measurements, Activities and Subjects
trainData <<- readData("train/X_train.txt")  
testData  <<- readData("test/X_test.txt") 
trainLabel  <<- readData("train/y_train.txt")  
testLabel  <<- readData("test/y_test.txt") 
trainSubject  <<- readData("train/subject_train.txt")  
testSubject  <<- readData("test/subject_test.txt") 

### Join the train and test data for each type
jdata <- rbind(trainData, testData)
jlabel <- rbind(trainLabel, testLabel)
jsubject <- rbind(trainSubject, testSubject)

### Load the features and the activities
featureNames <- readData("features.txt")[, 2] 
actNames<- readData("activity_labels.txt")[,2]

### Label the Variable Names for the measurements which they represent
names(jdata)<- featureNames

### Find the columns which are Means and Stds
fmean <- grepl("mean()", featureNames)   
fstd <- grepl("std()", featureNames) 
meanstds <- fmean | fstd 

###  Only keep the columns which are Means and stds - Tidy data of Means and Stds
datmeanstd<- jdata[,meanstds]

### Create a Tidy Data Set of just the Means and Stds of the data by Activity
### Combine the means and standards into one data set by Activity
combineddat<- cbind(datmeanstd, jlabel)

### Create averages for each of the 6 activities
xmelt<- melt(combineddat,
             id=c("V1"), 
             measure.vars=names(datmeanstd)) ### Melt data for activity-measurement
Xtidy <- dcast(xmelt, V1 ~ variable, mean)   ### Combine and average each
colnames(Xtidy)[1]<- "Activity"              ### Rename the first column
Xtidy$Activity<- actNames                    ### Replace 1st column with descriptive names

write.table(Xtidy, 
            "tidytxtbyactivity.txt", 
            sep=",", 
            row.names=FALSE)  

### Create a Tidy Data Set of just the Means and Stds of the data by Subject
#### Start over for subject
combineddat<- NULL
xmelt<- NULL
xtidy<- NULL

### Combine the means and standards into one data set by Subject
combineddat<-  cbind(datmeanstd, jsubject)

### Create averages for  each of the 30 subjects
xmelt<- melt(combineddat,
             id=c("V1"), 
             measure.vars=names(datmeanstd))   ### Melt data for subject-measurement
Xtidy <- dcast(xmelt, V1 ~ variable, mean)     ### Combine and average each
colnames(Xtidy)[1]<- "Volunteer_ID"            ### Rename the first column

write.table(Xtidy, 
            "tidytxtbysubject.txt", 
            sep=",", 
            row.names=FALSE)  

