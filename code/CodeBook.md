---
title: "CodeBook"
author: "Alexander Cormack"
date: "2022-11-01"
output:
  html_document:
    df_print: paged
---

# The Source Data

The source data used for this assignment consists essentially of seven files


### features.txt

The file contains a dataframe with 561 observations and 2 variables.
V1 is an integer variable containing the integers from 1 to 561 in ascending order
V2 is a character vector listing all of the features measured in the Samsung Galaxy S smartphone accelerometer trial.
The "features.info.txt" file contains the following information about the features measured.


**Feature Selection**

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ 

tGravityAcc-XYZ 

tBodyAccJerk-XYZ 

tBodyGyro-XYZ 

tBodyGyroJerk-XYZ 

tBodyAccMag 

tGravityAccMag 

tBodyAccJerkMag 

tBodyGyroMag 

tBodyGyroJerkMag 

fBodyAcc-XYZ 

fBodyAccJerk-XYZ 

fBodyGyro-XYZ 

fBodyAccMag 

fBodyAccJerkMag 

fBodyGyroMag 

fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

mean(): Mean value 

std(): Standard deviation 

mad(): Median absolute deviation 

max(): Largest value in array 

min(): Smallest value in array 

sma(): Signal magnitude area 

energy(): Energy measure. Sum of the squares divided by the number of values. 

iqr(): Interquartile range 

entropy(): Signal entropy 

arCoeff(): Autorregresion coefficients with Burg order equal to 4 

correlation(): correlation coefficient between two signals 

maxInds(): index of the frequency component with largest magnitude 

meanFreq(): Weighted average of the frequency components to obtain a mean frequency 

skewness(): skewness of the frequency domain signal 

kurtosis(): kurtosis of the frequency domain signal 

bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window. 

angle(): Angle between to vectors. 

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean

tBodyAccMean

tBodyAccJerkMean

tBodyGyroMean

tBodyGyroJerkMean

The complete list of variables of each feature vector is available in 'features.txt'


### subjects_test.txt

The file contains a dataframe with 2947 observations and 1 variable.
V1 is an integer variable representing the subjects for which measurements were made in the test pool.
The file contains the following unique values:

| V1 |  n  |
|:--:|:---:|
|  2 | 302 |
|  4 | 317 |
|  9 | 288 |
| 10 | 294 |
| 12 | 320 |
| 13 | 327 |
| 18 | 364 |
| 20 | 354 |
| 24 | 381 |


### subjects_train.txt

The file contains a dataframe with 7352 observations and 1 variable.
V1 is an integer variable representing the subjects for which measurements were made in the train pool.
The file contains the following unique values:

| V1 |  n  |
|:--:|:---:|
|  1 | 347 |
|  3 | 341 |
|  4 | 302 |
|  6 | 325 |
|  7 | 308 |
|  8 | 281 |
| 11 | 316 |
| 14 | 323 |
| 15 | 328 |
| 16 | 366 |
| 17 | 368 |
| 19 | 360 |
| 21 | 408 |
| 22 | 321 |
| 23 | 372 |
| 25 | 409 |
| 26 | 392 |
| 27 | 376 |
| 28 | 382 |
| 29 | 344 |
| 30 | 383 |


### y_test.txt

The file contains a dataframe with 2947 observations and 1 variable.
V1 is an integer variable (1-6) representing the activities for which measurements were made in the test pool.

The "activity_labels.txt" maps the integer values to the specfic activities:

1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING

The file contains the following unique values:

| V1 |  n  |
|:--:|:---:|
|  1 | 496 |
|  2 | 471 |
|  3 | 420 |
|  4 | 491 |
|  5 | 532 |
|  6 | 537 |


### y_train.txt

The file contains a dataframe with 7352 observations and 1 variable.
V1 is an integer variable (1-6) representing the activities for which measurements were made in the train pool.

The "activity_labels.txt" maps the integer values to the specfic activities:

1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING

The file contains the following unique values:

| V1 |   n  |
|:--:|:----:|
|  1 | 1226 |
|  2 | 1073 |
|  3 |  986 |
|  4 | 1286 |
|  5 | 1374 |
|  6 | 1407 |


### X_test.txt

The file contains a dataframe with 2947 observations and 561 variables.
The variables are all numeric floats ranging from 1 to -1 each with 9 decimal places.
The variables map to the "features.txt file" above.
The observations map to the "subject.txt" and "activity.txt" files above.


### X_train.txt

The file contains a dataframe with 2947 observations and 561 variables.
The variables are all numeric floats ranging from 1 to -1 each with 9 decimal places.
The variables map to the "features.txt file" above.
The observations map to the "subject.txt" and "activity.txt" files above.


# The code

As the assignment requires data to be selected and summarised, the dplyr library is loaded at line 3.


### First block (lines 9-12)

The first block of code deals with the "features.txt" file.
Since we are dealing with a .txt file the data is read in with the read.table() function (line 9).
So that we can use the list of features later as variable names, we create a vector of the feature names (line 10).
To make some of the feature names more descriptive we substitute "t" at the beginning of labels for "time." and "f" for "freq." (lines 11-12).


### Second block (lines 18-21)

The second block of code deals with the subject files.
The files are read in and then row bound with the test file on top and the train file below (lines 18-20).
This order is important to remember so that the data from the subjects, the activities and the measurements match.
We then change the variable name into a descriptive title (line 21).


### Third block (lines 28-38)

The third block of code deals with the activity files.
The files are read in and then row bound with the test file on top and the train file below (lines 28-30).
This order is important to remember so that the data from the subjects, the activities and the measurements match.
We then change the variable name into a descriptive title (line 31).
Finally, we substitute the integer data for the descriptive activities reported in the "activity_labels.txt" file (lines 33-38).


### Fourth block (lines 44-47)

The fourth block of code deals with the measurement files.
The files are read in and then row bound with the test file on top and the train file below (lines 44-46).
This order is important to remember so that the data from the subjects, the activities and the measurements match.
We then add variable names using the "features" vector created in the first block of code (line 47).


### Fifth block (lines 53-54)

The fifth block of code brings all of the above together.
We bind the three dataframes (subjects, activities and measurements) to create a complete dataset (line 53).
Then we select only the variables reporting the mean and standard deviation for each measurement (line 54).

### Sixth block (lines 59-60)

The sixth block of code creates a second, independent tidy data set with the average of each variable for each activity and each subject.
We group the data first by subject and then by activity (line 59).
Lastly, we summarise the data by using the "across" parameter and the mean() function to calculate the average for each variable (line 60).
The final output is a dataframe with 180 observations (30 subjects times 6 activities) and 68 variables (subject, activity plus the 66 "mean" measurements).
