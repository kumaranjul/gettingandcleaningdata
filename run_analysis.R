#==>dataset: combining test and train data along with the target variables and subjects
#==>summary: category wise mean of all columns containing the group wise mean and standard 
#            deviation of measured accelerations grouped by subjects and activity label
#==>column names of dataset are extracted from features.txt file and cleaned to removed all characters
#other than alphanumeric ones and all
#====================================================================================================
library(stringr)
#Importing train data and corresponding target value with subject list, test data and corresponding target value with subject list and names of features.
Xtrain<-read.table("C:/Users/KUMAR ANJUL/Downloads/Compressed/UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
Ytrain<-read.table("C:/Users/KUMAR ANJUL/Downloads/Compressed/UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
Xtest<-read.table("C:/Users/KUMAR ANJUL/Downloads/Compressed/UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
Ytest<-read.table("C:/Users/KUMAR ANJUL/Downloads/Compressed/UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("C:/Users/KUMAR ANJUL/Downloads/Compressed/UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
subject_train<-read.table("C:/Users/KUMAR ANJUL/Downloads/Compressed/UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
features<-read.table("C:/Users/KUMAR ANJUL/Downloads/Compressed/UCI HAR Dataset/UCI HAR Dataset/features.txt")
#====================================================================================================
#Setting correct names for dataframes created
names(Ytrain)<-"activity_label"
names(Ytest)<-"activity_label"
names(Xtrain)<-str_replace_all((features$V2),"[[:punct:]]","")
names(Xtest)<-str_replace_all((features$V2),"[[:punct:]]","")
names(subject_test)<-"subject"
names(subject_train)<-"subject"
#=====================================================================================================
#Combining predictor variables, target values and corresponding subject list to make test and train data
train<-cbind(Xtrain,Ytrain,subject_train)
test<-cbind(Xtest,Ytest,subject_test)
#=====================================================================================================
#Combining test and train data to dataframe called dataset
dataset<-rbind(train,test)

vec<-grep("mean+|std+",features$V2)#Selecting column names with pattern "mean" and "std" in them
dataset<-dataset[,c(vec,562,563)]#Combining selected columns, subject list and activity label
dataset$activity_label<-as.factor(dataset$activity_label)#converting activity labels to factor
levels(dataset$activity_label)<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")#Changing levels to more meaningful values to achieve step 3
#=====================================================================================================
#Creating summary dataset to get groupwise average values grouped by subject and activity labels 
summary<-aggregate(x = dataset[[names(dataset)[2]]],              
          by = list(dataset$subject,dataset$activity_label),             
          FUN = mean)
names(summary)[2:1]=names(dataset)[80:81]
names(summary)[3]=names(dataset)[1]

for(i in 2:(ncol(dataset)-2)){#traversing to summarise all columns one by one
  summary[,i+2]<-aggregate(x = dataset[[names(dataset)[i]]],                
                         by = list(dataset$subject,dataset$activity_label),             
                         FUN = mean)[,3]
  names(summary)[i+2]<-names(dataset)[i]
}

