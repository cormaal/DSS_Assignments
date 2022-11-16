library(dplyr)

header <- read.table("./data/household_power_consumption.txt", nrows = 1, sep = ";", header = TRUE)
col_names <- variable.names(header)
epc_df <- read.table("./data/household_power_consumption.txt", skip = 66637, nrows = 2880, sep = ";", col.names = col_names) %>%
        mutate(Date = paste(Date, Time)) %>%
        select(1, 3:9) %>%
        mutate(Date = strptime(Date, format = "%d/%m/%Y %H:%M:%OS"))


png(filename = "./output/plot1.png", width = 480, height = 480)
hist(epc_df$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.off()


png(filename = "./output/plot2.png", width = 480, height = 480)
with(epc_df, plot(Date, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)"))
dev.off()









