## Some code from a peer assignment showing (EDA_W1) how to download and setup the data

URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip"
destFile = "./data.txt"
if(!file.exists(destFile)) {download.file(URL, destfile = destFile)}

df <- read.table(destFile, header=TRUE, sep=";", quote=" ", na.strings="?")




