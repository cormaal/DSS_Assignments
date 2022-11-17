## Some code from a peer assignment showing (EDA_W1) how to download and setup the data

URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip"
destFile = "./data.txt"
if(!file.exists(destFile)) {download.file(URL, destfile = destFile)}

df <- read.table(destFile, header=TRUE, sep=";", quote=" ", na.strings="?")



## Some more code from another peer assignment

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
household_zip <- "household.zip"
download.file(url, destfile = household_zip,method = "curl", mode="wb")
unzip("household.zip",exdir = getwd())
list.files()
full_data <- read.table("household_power_consumption.txt",header = TRUE,
                        sep = ";")
head(full_data)
dim(full_data)
sub_data <- subset(full_data,Date %in% c("1/2/2007","2/2/2007"))
head(sub_data)
sub_data$Date <- as.Date(x=sub_data$Date,format="%d/%m/%Y")
sub_data$Global_active_power <- as.numeric(sub_data$Global_active_power)




