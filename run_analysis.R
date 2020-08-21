library(tidyverse)

## load data from the internet
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","assignment_data.zip")

## manually unzip


wd<-getwd()

activity_labels<- read_table2(paste0(wd,"/UCI HAR Dataset/activity_labels.txt"),col_names = c("Activity.Label","Activity"))

Features<- read_table(paste0(wd,"/UCI HAR Dataset/features.txt"),col_names = FALSE)
Features<- as.vector(as.data.frame(Features)[,1])
Features<- str_split_fixed(Features," ",n=2)
ColNames<- Features[,2]

YTest<-   read_table(paste0(wd,"/UCI HAR Dataset/test/y_test.txt"),col_names = c("Activity.Label"))
XTest<-   read_table(paste0(wd,"/UCI HAR Dataset/test/X_test.txt"),col_names = ColNames)
SubTest<- read_table(paste0(wd,"/UCI HAR Dataset/test/subject_test.txt"),col_names = c("Subject"))

Test<- cbind(SubTest,YTest,XTest)

XTrain<-   read_table(paste0(wd,"/UCI HAR Dataset/train/X_train.txt"),col_names = ColNames)
YTrain<-   read_table(paste0(wd,"/UCI HAR Dataset/train/y_train.txt"),col_names = c("Activity.Label"))
SubTrain<- read_table(paste0(wd,"/UCI HAR Dataset/train/subject_train.txt"),col_names = c("Subject"))

Train<- cbind(SubTrain,YTrain,XTrain)

Data<- rbind(Train%>%mutate(Dataset="Train"),Test%>%mutate(Dataset="Test"))

Data<- Data%>%select(Activity.Label,Subject,(colnames(Data)[str_detect(colnames(Data),pattern="mean")]|colnames(Data)[str_detect(colnames(Data),pattern="std")]))

Data<- Data%>%left_join(.,activity_labels)%>%select(!Activity.Label)

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

Data.copy<-Data

Data.copy<- Data.copy%>%group_by(Subject,Activity)%>%
                        select(!colnames(Data.copy)[str_detect(colnames(Data.copy),pattern="standard")])
                              
Data.copy<- Data.copy%>%summarise_all(mean)
