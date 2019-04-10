#run_analysis.R

library(plyr)
library(dplyr)
library(sqldf)
library(cli)

#Linux
setwd("/home/blackcherry/Dropbox/DataScienceSpecialisation/Getting&CleaningData/Week4/ProjectData")

#Windows
setwd("C:/Users/cohara/Dropbox/DataScienceSpecialisation/Getting&CleaningData/Week4/ProjectData")

if(!file.exists("./tidiedData")){dir.create("./tidiedData")}

#Read the train and text file for each feature. Ready to merge

#filenames
testtrain=c("test","train")

#Loop through the two files generating data farmaes for each with proper vriable names, and activity labels
for(j in 1:2){
	#Read the subject numbers and make it a data frame
      subject<-readLines(paste("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/",testtrain[j],"/subject_",testtrain[j],".txt",sep=""))
      subject<-as.data.frame(subject)
      
	#Read the activity numbers nd make it a data frame
      activity = matrix(scan(paste("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/",testtrain[j],"/y_",testtrain[j],".txt",sep="")),ncol=1,byrow=TRUE)
      activity<-as.data.frame(activity)
	#Give the variable a meaningful name
      names(activity)<-"activity"
	activity$activity<-as.factor(activity$activity)
	
	#Read the activity labels (corresponding to the numbers of activity above)
	activity_labels <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt",sep=" ")
	activity_labels<-as.data.frame(activity_labels[,2])
	#Give the variable a meaningful name
	names(activity_labels)<-"activity_labels"

	#Assign the activity_labels to each activity
	activity$activity <- revalue(activity$activity,c("1"=as.character(activity_labels$activity_labels[1]),"2"=as.character(activity_labels$activity_labels[2]),"3"=as.character(activity_labels$activity_labels[3]),"4"=as.character(activity_labels$activity_labels[4]),"5"=as.character(activity_labels$activity_labels[5]),"6"=as.character(activity_labels$activity_labels[6])))
              
      #Read the test/train dataset and make it a data frame
	data = matrix(scan(paste("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/",testtrain[j],"/X_",testtrain[j],".txt",sep="")),nrow=nrow(activity),byrow=TRUE)
      data<-as.data.frame(data)
      
	#Read the features, i.e. the variable names for the dataset
      features <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt",sep=" ")
      features<-as.data.frame(features[,2])
      names(features)<-"features"
      features$features<-as.character(features$features)
      
	#Assign the features to the data frame to give meaningful names to the variables
      names(data)<-features$features

      #Create a data frame that consists of the subjects, activity, and the data, with meaningful names
	data2<-cbind(subject,activity,data)
	#Assign that data to a meaningful data frame name
      assign(paste(testtrain[j],sep=""),data2)

	#Remove all temporary data from memory
      rm(data,data2,subject,activity,features,activity_labels)
}

#Make each of the variable names in the test and train datasets unique (so that when the two datasets are merged later all the variables will be retained)
names(test)<-make.names(names(test), unique = TRUE, allow_ = TRUE)
names(train)<-make.names(names(train), unique = TRUE, allow_ = TRUE)

#Append the test dataset to the train dataset
test_train<-bind_rows(train,test)

#Subset only the variables that are the mean or standard deviation
test_train_mean_std<-test_train %>% select(contains("mean"), contains("std"))

#Bind back in the subject and activity variables
test_train_mean_std<-cbind("subject"=test_train$subject,"activity"=test_train$activity,test_train_mean_std)
as.factor(test_train$subject)

#Arrange the dataset in order by subject number
test_train_mean_std$subject<-as.numeric(test_train_mean_std$subject)
test_train_mean_std<-arrange(test_train_mean_std,subject)
test_train_mean_std$subject<-as.factor(test_train_mean_std$subject)

#Creates an independent tidy data set with the average of each variable for each activity and each subject
MeanStd_Summarise<-test_train_mean_std %>% group_by(activity,subject,add = TRUE) %>% summarise_all(funs(mean))

write.csv(MeanStd_Summarise, file = "./tidy_data.csv")

