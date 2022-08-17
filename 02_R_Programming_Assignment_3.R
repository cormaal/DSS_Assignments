# File:     Programming_Assignment_3.R
# Project:  R Programming
# Author:   Alexander Cormack
# Date:     16 August 2022

# INTRODUCTION ###########################

# Download the file ProgAssignment3-data.zip file containing the data for Programming Assignment 3 from
# the Coursera web site. Unzip the file in a directory that will serve as your working directory. When you
# start up R make sure to change your working directory to the directory where you unzipped the data.

# The data for this assignment come from the Hospital Compare web site (http://hospitalcompare.hhs.gov)
# run by the U.S. Department of Health and Human Services. The purpose of the web site is to provide data and
# information about the quality of care at over 4,000 Medicare-certified hospitals in the U.S. This dataset
# essentially covers all major U.S. hospitals. This dataset is used for a variety of purposes, including determining
# whether hospitals should be fined for not providing high quality care to patients (see http://goo.gl/jAXFX
# for some background on this particular topic).

# The Hospital Compare web site contains a lot of data and we will only look at a small subset for this
# assignment. The zip file for this assignment contains three files
# • outcome-of-care-measures.csv: Contains information about 30-day mortality and readmission rates
#   for heart attacks, heart failure, and pneumonia for over 4,000 hospitals.
# • hospital-data.csv: Contains information about each hospital.
# • Hospital_Revised_Flatfiles.pdf: Descriptions of the variables in each file (i.e the code book).

# A description of the variables in each of the files is in the included PDF file named
# Hospital_Revised_Flatfiles.pdf. This document contains information about many other files that are
# not included with this programming assignment. You will want to focus on the variables for
# Number 19 (\Outcome of Care Measures.csv") and Number 11 (\Hospital Data.csv"). You may find it useful
# to print out this document (at least the pages for Tables 19 and 11) to have next to you while you work
# on this assignment. In particular, the numbers of the variables for each table indicate column indices
# in each table (i.e. \Hospital Name" is column 2 in the outcome-of-care-measures.csv file).


# 1 PLOT THE 30-DAY MORTALITY RATES FOR HEART ATTACK ###########################

# Make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset),

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

# Because we originally read the data in as character (by specifying colClasses = "character" we need to
# coerce the column to be numeric. You may get a warning about NAs being introduced but that is okay.


# 2 FINDING THE BEST HOSPITAL IN THE STATE ###########################

# Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.

# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the first hospital in that set should be chosen
# (i.e. if hospitals \b", \c", # and \f" are tied for best, then hospital \b" should be returned).

# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message \invalid state". If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message \invalid outcome".

# Here is some sample output from the function.

# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"

# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"

# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"

# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"

# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state

# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome


library(data.table)
library(dplyr)

best <- function(state, outcome) {
        
        # create a vector containing the abbreviations of all US states
        state_codes <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
                         "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                         "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
                         "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                         "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
        
        # check if the 'state' argument is included in the vector and stop if FALSE
        if (state %in% state_codes == FALSE) {
                stop("invalid state")
        }
        
        # create a vector containing the three possible outcomes
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        # check if the 'outcome' variable is included in the vector and stop if FALSE
        if (outcome %in% outcomes == FALSE) {
                stop("invalid outcome")
        }
        
        # read in the columns with hospital name, state code and the three outcomes (N.B. requires data.table)
        df <- fread("outcome-of-care-measures.csv", select = c(2, 7, 11, 17, 23))
        
        # change the column names so they are more concise
        colnames(df) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
        
        # change all instances of 'Not Available' to <NA>
        df[df == "Not Available"] <- NA
        
        # coerce the outcome columns from character to numeric (N.B. requires dplyr)
        df <- df %>% mutate_at(c('heart attack', 'heart failure', 'pneumonia'), as.numeric)
        
        # filter the dataframe by the inputted state and outcome
        sh <- select(filter(df, State == state), c(Hospital, State, all_of(outcome)))
        
        # filter out the NAs
        good <- complete.cases(sh)
        sh_clean <- sh[good, ]
        
        # find the minimum value in the outcome column
        minimum <- min(sh_clean[, 3])
        
        # find the hospital(s) with the minimum value in the outcome column
        results <- select(filter(sh_clean, sh_clean[, 3] == minimum), c(Hospital, State, all_of(outcome)))
        
        # sort the results by alphabetical order (if there are any ties)
        results_ordered <- results[order(results$Hospital), ]
        
        # select the first-ranked hospital name
        final_result <- results_ordered[1, 1]
        
        # drop the column name
        names(final_result) <- NULL
        
        # return the result
        final_result
}

# Notes ###########################

# When an invalid argument is passed to the 'best' function the stop() function is correctly executed.
# However, the system enters debugging mode ... I would like to know how to avoid this.

# 3 RANKING HOSPITALS BY OUTCOME IN A STATE ###########################

# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call

# rankhospital("MD", "heart failure", 5)

# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values "best", "worst", or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.

# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken alphabetically by using the hospital name.

# The function should check the validity of its arguments. If an invalid state value is passed to rankhospital,
# the function should throw an error via the stop function with the exact message "invalid state". If an invalid
# outcome value is passed to rankhospital, the function should throw an error via the stop function with
# the exact message "invalid outcome".

# Here is some sample output from the function.

# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"

# > rankhospital("MD", "heart attack", "worst")
# 3
# [1] "HARFORD MEMORIAL HOSPITAL"

# > rankhospital("MN", "heart attack", 5000)
# [1] NA


library(data.table)
library(dplyr)

rankhospital <- function(state, outcome, num = "best") {
        
        # check if arguments are valid
        state_codes <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
                         "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                         "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
                         "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                         "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
        if (!state %in% state_codes) {
                stop("invalid state")
        }
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if (!outcome %in% outcomes) {
                stop("invalid outcome")
        }
        extremes <- c('best', 'worst')
        if (!num %in% extremes & !is.numeric(num)) {
                stop("The input for hospital rank must be 'best', 'worst' or a number")
        }

        # read in the columns with hospital name, state code and the three outcomes (N.B. requires data.table)
        df <- fread("outcome-of-care-measures.csv", select = c(2, 7, 11, 17, 23))
        
        # change the column names so they are more concise
        colnames(df) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
        
        # change all instances of 'Not Available' to <NA>
        df[df == "Not Available"] <- NA
        
        # coerce the outcome columns from character to numeric (N.B. requires dplyr)
        df <- df %>% mutate_at(c('heart attack', 'heart failure', 'pneumonia'), as.numeric)
        
        # filter the dataframe by the inputted state and outcome
        sh <- select(filter(df, State == state), c(Hospital, State, all_of(outcome)))
        
        # filter out the NAs
        good <- complete.cases(sh)
        sh_clean <- sh[good, ]
        
        # rank the selected state hospitals by outcome and alphabetically (to handle ties)
        sh_ranked <- arrange(sh_clean, sh_clean[, 3], sh_clean[, 1])
        
        # handle the rank argument for 'best','worst' and out of range
        if(num == "best") {
                num <- 1
        }
        if(num == "worst") {
                num <- nrow(sh_ranked)
        }
        if(num > nrow(sh_ranked)) {
                return(NA)
        }
        # select hospital name according to the ranking argument
        final_result <- sh_ranked[num, 1]
        
        # drop the column name
        names(final_result) <- NULL
        
        # return the result
        final_result
}

# Notes ###########################

# When an invalid argument is passed to the 'rankhospital' function the stop() function is correctly executed.
# However, the system enters debugging mode ... I would like to know how to avoid this.




