# File:     Programming_Assignment_1.R
# Project:  R Programming
# Author:   Alexander Cormack
# Date:     11 August 2022

library(tidyverse)
library(fs)


# INTRODUCTION ###########################

# For this first programming assignment you will write three functions that are meant
# to interact with dataset that accompanies this assignment. The dataset is contained
# in a zip file specdata.zip that you can download from the Coursera web site. 



# DATA ###################################

# The zip file contains 332 comma-separated-value (CSV) files containing pollution
# monitoring data for fine particulate matter (PM) air pollution at 332 locations
# in the United States. Each file contains data from a single monitor and the ID number
# for each monitor is contained in the file name. For example, data for monitor 200
# is contained in the file "200.csv". Each file contains three variables:

# Date: the date of the observation in YYYY-MM-DD format (year-month-day)

# sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)

# nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)

# For this programming assignment you will need to unzip this file and create the directory
# 'specdata'. Once you have unzipped the zip file, do not make any modifications to the files
# in the 'specdata' directory. In each file you'll notice that there are many days where
# either sulfate or nitrate (or both) are missing (coded as NA). This is common with air
# pollution monitoring data in the United States.



# PART 0NE ################################

# Write a function named 'pollutantmean' that calculates the mean of a pollutant
# (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean'
# takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers,
# 'pollutantmean' reads that monitors' particulate matter data from the directory specified
# in the 'directory' argument and returns the mean of the pollutant across all of the monitors,
# ignoring any missing values coded as NA.

# Sample output

# pollutantmean("specdata", "sulfate", 1:10)
# [1] 4.064128

# pollutantmean("specdata", "nitrate", 70:72)
# [1] 1.706047

# pollutantmean("specdata", "sulfate", 34)
# [1] 1.477143

# pollutantmean("specdata", "nitrate")
# [1] 1.702932


pollutantmean <- function(directory, pollutant, id = 1:332) {
  
    ## create a list of all file paths in the directory
    file_paths <- fs::dir_ls(directory)
    
    ## subset the list of file paths to get the required files
    my_file_paths <- file_paths[id]
    
    ## create an empty numeric vector to store values read from files
    poll_data <- c()
    
    ## loop over the subset list of files
    for (i in seq_along(my_file_paths)) {
      
        ## read the files into a dataframe
        df <- read.csv((my_file_paths[i]))
        
        ## extract pollutant data to a vector
        poll <- df[ , pollutant]
        
        ## create a logical vector of the NA cases
        poll_na <- is.na(poll)
        
        ## remove the NA values
        poll_no_na <- poll[!poll_na]
        
        ## store the the available values in pollutant data vector
        poll_data <- c(poll_data, poll_no_na)
    }
    
    ## find the mean of the relevant pollutant data
    mean(poll_data)
}



# PART TWO ################################

# Write a function that reads a directory full of files and reports the number of completely
# observed cases in each data file. The function should return a data frame where the first column
# is the name of the file and the second column is the number of complete cases.

# Sample output

# cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
# print(cc$nobs)
# [1] 228 148 124 165 104 460 232

# cc <- complete("specdata", 54)
# print(cc$nobs)
# [1] 219

# RNGversion("3.5.1")  
# set.seed(42)
# cc <- complete("specdata", 332:1)
# use <- sample(332, 10)
# print(cc[use, "nobs"])
# [1] 711 135  74 445 178  73  49   0 687 237


complete <- function(directory, id = 1:332) {
  
    ## create a list of all file paths in the directory
    file_paths <- fs::dir_ls(directory)
    
    ## subset the list of file paths to get the required files
    my_file_paths <- file_paths[id]
    
    ## create empty numeric vectors to store values read from files
    id <- c()
    nobs <- c()
    
    ## loop over the subset list of files
    for (i in seq_along(my_file_paths)) {
      
        ## read the files into a dataframe
        df <- read.csv((my_file_paths[i]))
        
        ## create a logical vector of the complete cases
        good <- complete.cases(df)
        
        ## store the no of compelte rows in the nobs vector
        nobs <- c(nobs, nrow(df[good, ]))
        
        ## store the id of the file in the id vector
        id <- c(id, df[1, 4])
    }
    
    ## create and return the specified dataframe
    df <- data.frame(id, nobs)
    df
}



# PART THREE ################################

# Write a function that takes a directory of data files and a threshold for complete cases
# and calculates the correlation between sulfate and nitrate for monitor locations where the number
# of completely observed cases (on all variables) is greater than the threshold. The function should
# return a vector of correlations for the monitors that meet the threshold requirement. If no monitors
# meet the threshold requirement, then the function should return a numeric vector of length 0.

# Sample output

# cr <- corr("specdata")                
# cr <- sort(cr)   
# RNGversion("3.5.1")
# set.seed(868)                
# out <- round(cr[sample(length(cr), 5)], 4)
# print(out)
# [1]  0.2688  0.1127 -0.0085  0.4586  0.0447

# cr <- corr("specdata", 129)                
# cr <- sort(cr)                
# n <- length(cr)    
# RNGversion("3.5.1")
# set.seed(197)                
# out <- c(n, round(cr[sample(n, 5)], 4))
# print(out)
# [1] 243.0000   0.2540   0.0504  -0.1462  -0.1680   0.5969

# cr <- corr("specdata", 2000)                
# n <- length(cr)                
# cr <- corr("specdata", 1000)                
# cr <- sort(cr)
# print(c(n, round(cr, 4)))
# [1]  0.0000 -0.0190  0.0419  0.1901


corr <- function(directory, threshold = 0) {
    
    ## create a list of all file paths in the directory
    file_paths <- fs::dir_ls(directory)
    
    ## create an empty numeric vector to store the correlation coefficients
    corr_data <- c()
      
    ## loop through all the files
    for (i in seq_along(file_paths)) {
        
        ## read the files into a dataframe
        df <- read.csv((file_paths[i]))
        
        ## create a logical vector of the complete cases
        good <- complete.cases(df)
        
        ## create a dataframe with only complete cases
        good_df <- df[good, ]
        
        ## check if the number of complete cases is above the threshold
        if(nrow(good_df) > threshold) {
            
            ## add the correlation coefficient of sulfate and nitrate to the numeric vector
            corr_data <- c(corr_data, cor(good_df[2], good_df[3]))
        }
    }
    ## return the numeric vector of correlation coefficients of all the stations above the threshold
    corr_data
}



# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)