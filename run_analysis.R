run_analysis <- function(){
  library("reshape2")
  
  # read test data
  x.test  <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header=FALSE)
  
  # read train data
  x.train  <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header=FALSE)
  
  # merge test and train data set
  raw.data.set  <- rbind(x.test,x.train)
  
  # read features to set colonums names of my merged data set
  col.names  <- read.table("./data/UCI HAR Dataset/features.txt", header=FALSE)$V2
  colnames(raw.data.set)  <- col.names
  
  #index of column names that has mean and std in it
  final.col.index  <- grep(".*mean\\(\\)|.*std\\(\\)", colnames(raw.data.set))
  
  #subset data frame to mean and std columns discard all other data columns
  working.data.set  <-  raw.data.set[,final.col.index] 
  #head(working.data.set,2)
  
  final.col.names <- colnames(working.data.set)
  final.col.names <- gsub('\\(|\\)',"", final.col.names)
  #final.col.names
  
  colnames(working.data.set) <- final.col.names
  
  activity.id  <- rbind(read.table("./data/UCI HAR Dataset/test/y_test.txt", header=FALSE),read.table("./data/UCI HAR Dataset/train/y_train.txt", header=FALSE) )
  working.data.set$activity_id  <-activity.id$V1
  #head(working.data.set,2)
  
  # set Activity Labels
  activity.labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header=FALSE, col.names=c("activity_id", "activity_name"))
  working.data.set  <-  merge(working.data.set,activity.labels, by.x="activity_id",by.y="activity_id")
  #head(working.data.set,2)
  
  subject.id  <- rbind(read.table("./data/UCI HAR Dataset/test/subject_test.txt", header=FALSE),read.table("./data/UCI HAR Dataset/train/subject_train.txt", header=FALSE) )
  working.data.set$subject_id  <-subject.id$V1
  
  tidy.data.set.title <- c("activity_id", "activity_name", "subject_id")
  final.tidy.data.set  <- melt(working.data.set, id=tidy.data.set.title, measure.var=setdiff(colnames(working.data.set), tidy.data.set.title))
  
  dcast(final.tidy.data.set, activity_name + subject_id ~ variable, mean)
  
  write.table(final.tidy.data.set,"tidydata.txt",row.names=FALSE,sep="\t", quote = FALSE)
  
}

