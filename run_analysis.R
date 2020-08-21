library(tidyverse)

wd<-getwd()

## read activity Labels
activity_labels<- read_table2(paste0(wd,"/UCI HAR Dataset/activity_labels.txt"),col_names = c("Activity.Label","Activity"))

## read feature labels
Features<- read_table(paste0(wd,"/UCI HAR Dataset/features.txt"),col_names = FALSE)
Features<- as.vector(as.data.frame(Features)[,1])
Features<- str_split_fixed(Features," ",n=2)
ColNames<- Features[,2]

## Read Test Data
YTest<-   read_table(paste0(wd,"/UCI HAR Dataset/test/y_test.txt"),col_names = c("Activity.Label"))
XTest<-   read_table(paste0(wd,"/UCI HAR Dataset/test/X_test.txt"),col_names = ColNames)
SubTest<- read_table(paste0(wd,"/UCI HAR Dataset/test/subject_test.txt"),col_names = c("Subject"))

## Bind Test Data to one Data.frame
Test<- cbind(SubTest,YTest,XTest)

## Read Training Data
XTrain<-   read_table(paste0(wd,"/UCI HAR Dataset/train/X_train.txt"),col_names = ColNames)
YTrain<-   read_table(paste0(wd,"/UCI HAR Dataset/train/y_train.txt"),col_names = c("Activity.Label"))
SubTrain<- read_table(paste0(wd,"/UCI HAR Dataset/train/subject_train.txt"),col_names = c("Subject"))

## Bind Training Data to one Data Frame
Train<- cbind(SubTrain,YTrain,XTrain)

## Combine Training and Test Data in one Data Frame
Data<- rbind(Train%>%mutate(Dataset="Train"),Test%>%mutate(Dataset="Test"))

## Remove all not required columns
Data<- Data%>%select(Activity.Label,Subject,(colnames(Data)[str_detect(colnames(Data),pattern="mean")]|colnames(Data)[str_detect(colnames(Data),pattern="std")]))

## Set descriptive Activity names
Data<- Data%>%left_join(.,activity_labels)%>%select(!Activity.Label)

## Set appropriate variable labels
CL<- colnames(Data)

CL<- sapply(CL,str_replace,pattern="^t",replacement="time.")
CL<- sapply(CL,str_replace,pattern="^f",replacement="frequency.")
CL<- sapply(CL,str_replace,pattern="BodyBody",replacement="Body")
CL<- sapply(CL,str_replace,pattern="Body",replacement="body.")
CL<- sapply(CL,str_replace,pattern="Gravity",replacement="gravity.")
CL<- sapply(CL,str_replace,pattern="Gyro",replacement="gyroscope.")
CL<- sapply(CL,str_replace,pattern="Acc",replacement="accelerometer.")
CL<- sapply(CL,str_replace,pattern="-mean\\(\\)",replacement="mean.")
CL<- sapply(CL,str_replace,pattern="-meanFreq\\(\\)",replacement="mean.frequency.")
CL<- sapply(CL,str_replace,pattern="-std\\(\\)",replacement="standard.deviation")
CL<- sapply(CL,str_replace,pattern="Mag",replacement="magnitude.")
CL<- sapply(CL,str_replace,pattern="Jerk",replacement="jerk.")
CL<- sapply(CL,str_replace,pattern="\\.$",replacement="")
CL<- sapply(CL,str_replace,pattern="\\.-",replacement="-")

colnames(Data)<- CL

## Create Data frame copy
Data.copy<-Data

## Summarise Mean values for each subject and activity
Data.copy<- Data.copy%>%group_by(Subject,Activity)%>%
                        select(!colnames(Data.copy)[str_detect(colnames(Data.copy),pattern="standard")])%>%
                        summarise_all(mean)
                              
