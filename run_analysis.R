url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile="SamsungData.zip",method="curl")

#Please create a new folder named PROJECT and put 'activity_labels.txt', 'features.txt', 'subject_test.txt',
#'subject_train.txt', 'X_train.txt', 'X_test.txt', 'y_test.txt' and 'y_train.txt' into the PROJECT folder.

#Sets the working directory
setwd("./UCI HAR Dataset/PROJECT")

train_subject <- read.table("subject_train.txt",header=F)
test_subject <- read.table("subject_test.txt",header=F)
test_X <- read.table("X_test.txt",header=F)
test_Y <- read.table("y_test.txt",header=F)
train_X <- read.table("X_train.txt",header=F)
train_Y <- read.table("y_train.txt",header=F)

features <- read.table("features.txt",header=F)
labels <- read.table("activity_labels.txt",header=F)

names(train_X) <- features[,2]
names(test_X) <- features[,2]
names(train_Y) <- "Activity_code"
names(test_Y) <- "Activity_code"
names(train_subject) <- "Subject"
names(test_subject)<-"Subject"

#Using regular expression to extract only the measurements on the mean and standard deviation
selected <- grep("([mM]ean|std)[/(][/)]$",names(test_X))

train_X2 <- train_X[,selected]
test_X2 <- test_X[,selected]

training <- cbind(train_subject,train_X2,train_Y)
test <- cbind(test_subject,test_X2,test_Y)

all <- rbind(training,test)

#Uses descriptive activity names to name the activities in the data set
all$Activity <- factor(all$Activity_code,labels=labels[,2])

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject
#Integrates each data set 

copy <- all
second <- data.frame()

for(i in 2:(length(all)-2)){
  tem_data <- data.frame(Subject=c(1:30))
  summary_info <- data.frame(tapply(copy[,i],INDEX=list(copy$Subject,copy$Activity_code),FUN=mean))
  names(summary_info) <- labels[,2]
  tem_data <- cbind(tem_data,summary_info)
  second <- rbind(second,tem_data)
  
}

#Create a new variable named Kinds to represent to different column variables such as tBodyAccMag-mean()
second$Kinds <- rep(names(test_X2),each=30)
second <- second[,c(1,8,2,3,4,5,6,7)]

#Output a second, independent tidy data set with the average of each variable for each activity and each subject.
write.table(second,"processedSamsungData.txt",quote=F,row.names=F)
