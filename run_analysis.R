library(dplyr)

run_analysis<-function(){
##Load features 
features <- read.table(file="./UCI HAR Dataset/features.txt", header=FALSE, sep="", stringsAsFactors=FALSE)
##Generate vector with mean() and std()
v<-features$V2[grepl("-mean()",features$V2,fixed=TRUE) | grepl("-std()",features$V2,fixed=TRUE)]

##Load test and train dataset
df_x1 <- read.table(file="./UCI HAR Dataset/train/X_train.txt", header=FALSE, sep="")
df_x2 <- read.table(file="./UCI HAR Dataset/test/X_test.txt", header=FALSE, , sep="")

##merge test and train
dfx<-rbind(df_x1,df_x2)
colnames(dfx)=v
dfx<-dfx[v]

## remove initial data.frame
rm(df_x1,df_x2)
library(dplyr)
df2 <-group_by(df,Activity,Subject)

## load activity
df_y1 <- read.csv(file="./UCI HAR Dataset/train/y_train.txt", header=FALSE, sep="")
df_y2 <- read.csv(file="./UCI HAR Dataset/test/y_test.txt", header=FALSE, sep="")

##merge activiy test and train & remove original
dfy <- rbind(df_y1,df_y2)
colnames(dfy)="Activity"
rm(df_y1,df_y2)

## load activity names
a <- read.table(file="./UCI HAR Dataset/activity_labels.txt", header=FALSE, sep="", stringsAsFactors=TRUE)
colnames(a)=c("id","Activity")

## create a vector of activity name
dfy_a<-data.frame(a[dfy[,1],2])
colnames(dfy_a)="Activity"


## load subject merge train and test 
df_s1 <- read.csv(file="./UCI HAR Dataset/train/subject_train.txt", header=FALSE, sep="")
df_s2 <- read.csv(file="./UCI HAR Dataset/test/subject_test.txt", header=FALSE, sep="")

dfs <-rbind(df_s1,df_s2)
colnames(dfs)="Subject"
dfs$Subject <- as.factor(dfs$Subject)
df<-cbind(dfx,dfy_a,dfs)
rm(dfx,dfy,dfs,dfy_a,df_s1,df_s2)
df2<-group_by(df,Activity,Subject)%>%summarise_each(funs(mean),1:66)
return(df2)
##write.table(df2,file="dataset.txt",row.name=FALSE)
}