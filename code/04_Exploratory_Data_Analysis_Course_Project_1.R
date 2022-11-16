## Load dplyr to support the data preparation

library(dplyr)

## N.B. The size of the uncompressed source file "household_power_consumption.txt" is approximately 130 Mb.
## As this exceeds the GitHub limit and I had trouble setting up large file transfer in Github,
## I deleted several hundred thousand lines from the source file to be able to push it to my Github repo.

## I first inspected the source file with Notepad++ and identified the data required for this project (i.e. rows 66638-69518).
## Lines 17-18 read in the first line and create a vector with the variable names.
## Line 19 creates a dataframe by reading in the required data and separating the columns by the designated separator.
## The variable names created in line 18 are added as column names.
## Line 20 adds a new "datetime" column that pastes the values of the Date and Time columns together.
## Line 21 drops the Date and Time columns are they are no longer needed.
## Lastly Line 22 converts the datetime column from character to Date class - our dataset is clean and tidy!

header <- read.table("./data/household_power_consumption.txt", nrows = 1, sep = ";", header = TRUE)
col_names <- variable.names(header)
hpc <- read.table("./data/household_power_consumption.txt", skip = 66637, nrows = 2880, sep = ";", col.names = col_names) %>%
        mutate(datetime = paste(Date, Time)) %>%
        select(3:10) %>%
        mutate(datetime = strptime(datetime, format = "%d/%m/%Y %H:%M:%OS"))


## Line 29 opens the file device with specified file name and dimensions
## Line 31 plots the histogram with the specific colour, title and x axis label
## Line 33 explicily closes the graphics device

png(filename = "./output/plot1.png", width = 480, height = 480)

with(hpc, hist(Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)"))

dev.off()


## Line 40 opens the file device with specified file name and dimensions
## Line 42 plots the line graph (type = "l") with the default colour and the specific y axis label
## Line 44 explicily closes the graphics device

png(filename = "./output/plot2.png", width = 480, height = 480)

with(hpc, plot(datetime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)"))

dev.off()


## Line 54 opens the file device with specified file name and dimensions
## Line 56 plots the first line graph (type = "l") with the the specific x&y axis labels, colour and line type
## Line 57 adds the second line with the specific colour and line type
## Line 58 adds the third line with the specific colour and line type
## Line 59 adds the legend in the appropriate place, with the specific line types and colours and the appropriate legends
## Line 61 explicily closes the graphics device

png(filename = "./output/plot3.png", width = 480, height = 480)

with(hpc, plot(datetime, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering", col = "black", lty = 1))
with(hpc, lines(datetime, Sub_metering_2, col = "red", lty = 1))
with(hpc, lines(datetime, Sub_metering_3, col = "blue", lty = 1))
legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.off()


## Line 73 opens the file device with specified file name and dimensions
## Line 75 sets up the graphics device to add four plots row-wise in a 2x2 matrix
## Line 77 plots the first line graph (top left)
## Line 79 plots the second line graph (top right)
## Lines 81-84 plot the third line graph (bottom left)
## Line 86 plots the fourth line graph (bottom right)
## Line 86 explicily closes the graphics device
## Line 88 returns the graphics device to the 1x1 display

png(filename = "./output/plot4.png", width = 480, height = 480)

par(mfrow = c(2, 2))

with(hpc, plot(datetime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power"))

with(hpc, plot(datetime, Voltage,  type = "l"))

with(hpc, plot(datetime, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering", col = "black", lty = 1))
with(hpc, lines(datetime, Sub_metering_2, col = "red", lty = 1))
with(hpc, lines(datetime, Sub_metering_3, col = "blue", lty = 1))
legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

with(hpc, plot(datetime, Global_reactive_power,  type = "l"))

dev.off()

par(mfrow = c(1, 1))
