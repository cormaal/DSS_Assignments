library(dplyr)

header <- read.table("./data/household_power_consumption.txt", nrows = 1, sep = ";", header = TRUE)
col_names <- variable.names(header)
hpc <- read.table("./data/household_power_consumption.txt", skip = 66637, nrows = 2880, sep = ";", col.names = col_names) %>%
        mutate(datetime = paste(Date, Time)) %>%
        select(3:10) %>%
        mutate(datetime = strptime(datetime, format = "%d/%m/%Y %H:%M:%OS"))


png(filename = "./output/plot1.png", width = 480, height = 480)
hist(hpc$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.off()


png(filename = "./output/plot2.png", width = 480, height = 480)
with(hpc, plot(datetime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)"))
dev.off()

png(filename = "./output/plot3.png", width = 480, height = 480)
with(hpc, plot(datetime, Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub metering", lty = 1))
lines(hpc$datetime, hpc$Sub_metering_2, col = "red", lty = 1)
lines(hpc$datetime, hpc$Sub_metering_3, col = "blue", lty = 1)
legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()



