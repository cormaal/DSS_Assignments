
header <- read.table("./data/household_power_consumption.txt", nrows = 1, sep = ";", header = TRUE)

col_names <- variable.names(header)

data <- read.table("./data/household_power_consumption.txt", skip = 66637, nrows = 2880, sep = ";", col.names = col_names)



