library(stringr)
library(dplyr)

#get the data
#folders where the files are
filep<- "UCI HAR Dataset/"
filep_tst<- paste0(filep,"test/")
filep_tr<- paste0(filep,"train/")

#get labels
act_labels<- read.table(paste0(filep,"/activity_labels.txt"))
col_label<- read.table(paste0(filep,"/features.txt"))

#create function to download data
getdata<- function(file,folder=filep_tst) {
            read.table(paste0(folder,file)) 
            }
#create function to format data and combine into 1 dataset
format<- function(){
  sub$subject<- as.factor(sub$V1)
  names(data)<- col_label$V2
  #clean up column names
  colnames(data)<- sub("\\(\\)","",colnames(data))
  colnames(data)<- gsub("-|\\(|\\)|,","_",colnames(data))
  colnames(data)<- gsub("_$","",colnames(data))
  colnames(data)<- gsub("(Body)+","Body",colnames(data))
  
  cbind(subject=sub$subject,n_act=act$V1,data)
}

#download 3 test datasets
sub<- getdata("subject_test.txt")
act<- getdata("y_test.txt")
data<- getdata("x_test.txt")

#format and combine datasets into single test dataset
test<- format()

#download 3 training datasets
sub<- getdata("subject_train.txt",filep_tr)
act<- getdata("y_train.txt",filep_tr)
data<- getdata("x_train.txt",filep_tr)

#format and combine datasets into single training dataset
train<-  format()

#add together test and training data
combined_data<- rbind(test,train)

#clean up other datasets
rm(sub,act,data,test,train)

#keep only id, mean, and std variables
combined_data<-  combined_data[,grepl("[Mm]ean|std|act|subject",colnames(combined_data))]
combined_data<-  combined_data[,!grepl("meanFreq",colnames(combined_data))]

#get labels for activity and reorder rows
combined_data<- merge(act_labels,combined_data,by.x="V1",by.y="n_act")
combined_data$V2<- as.factor(combined_data$V2)
combined_data<- select(combined_data,-V1)
names(combined_data)[1]<- "activity"
combined_data<- arrange(combined_data,subject,activity)

#create new data set of averages
#group the dataset
averages<- group_by(combined_data,subject,activity)

#find average for each variable
averages<- summarize_if(averages, is.numeric, mean, na.rm = TRUE)

#save file
#write.table(averages,"average_data.txt",row.name=FALSE)

