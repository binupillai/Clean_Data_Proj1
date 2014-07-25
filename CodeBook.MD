Data
=================================================================================
One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S 
smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 



Transformations
==================================================================================
1.Read test and train data
2.Merge test and train data
3.Read column names from features.txt and set to the merged data
4.Create a working data frame with only mean and standard deviation columns
5.Clean column names. Remove special characters from the column names
6.Read test and train activity ids 
7.Merge test and train activity ids to working data set
8.Read activity label and merge it with working data set by activity_id
9.Read test and train subject data
10.Merge test and train subject data
11.Add subject_id column to working data set and assign subject id form above step    
12.Melt data set to reshape data set
13.Recast the data set to add average of each variable for each activity and subject
14.Write the tidy data set to tidydata.txt into working directory


Tidy Data Variables
================================================================================
1.activity_id :- The activity ID performed by each person.

2.activity_name :- The activity name performed by each person.

3.subject_id:-The subject ID of who performed the activity for each window sample.

4.variable :- Different accelerometer readings.

5.value :- Mean of each variable for each activity and subject.
