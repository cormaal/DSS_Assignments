## Assignment

## The overall goal of this assignment is to explore the National Emissions Inventory database and see what it says
## about fine particulate matter pollution in the United states over the 10-year period 1999–2008.
## You may use any R package you want to support your analysis.


## Questions

## You must address the following questions and tasks in your exploratory analysis.
## For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.

## 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
##    Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

## 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510"fips == "24510") from 1999 to 2008?
##    Use the base plotting system to make a plot answering this question.

## 3. Of the four types of sources indicated by the typetype (point, nonpoint, onroad, nonroad) variable,
##    which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
##    Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

## 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
        
## 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
        
## 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California
##    (fips == "06037"fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?



## Load the packages needed to complete the assignment
library(dplyr)
library(ggplot2)

## Read in the two assignment files
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")


## Create the first dataframe - group by year and sum emissions
NEI_Q1 <- group_by(NEI, year) %>%
        summarise(total_PM2.5 = sum(Emissions))

## Plot the totals, add an appropriate title, axis labels and a trend line
png(filename = "./output2/plot1.png", width = 480, height = 480)
plot(NEI_Q1, main = "1999-2008 United States total PM2.5 emissions from all sources", xlab = "Year", ylab = "Total PM2.5 (tonnes)")
abline(lm(total_PM2.5 ~ year, NEI_Q1), col = "blue", lwd = 2)
dev.off()


## Create the second dataframe - filter by the required county, group by year and sum emissions
NEI_Q2 <- filter(NEI, fips == "24510") %>%
        group_by(year) %>%
        summarise(total_PM2.5_BC = sum(Emissions))

## Plot the totals, add an appropriate title, axis labels and a trend line
png(filename = "./output2/plot2.png", width = 480, height = 480)
plot(NEI_Q2, main = "1999-2008 Baltimore City total PM2.5 emissions from all sources", xlab = "Year", ylab = "Total PM2.5 (tonnes)")
abline(lm(total_PM2.5_BC ~ year, NEI_Q2), col = "blue", lwd = 2)
dev.off()


## Create the third dataframe - filter by the required county, group by year and source type and sum emissions
NEI_Q3 <- filter(NEI, fips == "24510") %>%
        group_by(year, type) %>%
        summarise(total_by_type_BC = sum(Emissions))

## Plot the totals, apply the group function, add an appropriate title, axis labels and a legend title
png(filename = "./output2/plot3.png", width = 480, height = 480)
ggplot(NEI_Q3, aes(year, total_by_type_BC, group=type, color=type)) + 
        labs(title = "1999-2008 Baltimore City total emissions by type",
             x = "Year", y = "Total PM2.5 by type (tonnes)", colour = "Type") + 
        geom_point() + geom_line()
dev.off()


## Create a dataframe from the SCC table and select the two columns of interest. Slice the dataframe to include only coal powered sectors
SCC_Q4 <- select(SCC, SCC, EI.Sector)
SCC_Q4 <- SCC[grep("Coal", SCC$EI.Sector), ]

## Create the fourth dataframe by merging on the SCC column, grouping by year and sector and finding the median emissions
NEI_Q4 <- merge(NEI, SCC_Q4) %>%
        group_by(year, EI.Sector) %>%
        summarise(median_by_source = median(Emissions))

## Plot the medians, apply the group function, add an appropriate title, axis labels and a legend title, and use facets for the three graphs
png(filename = "./output2/plot4.png", width = 960, height = 480)
ggplot(NEI_Q4, aes(year,median_by_source)) + 
        labs(title = "1999-2008 United States median emissions from coal combustion-related sources",
             x = "Year", y = "Median PM2.5 (tonnes)") + 
        geom_point() + geom_line() + facet_wrap(~EI.Sector, scales = "free")
dev.off()


## Create a dataframe from the SCC table and select the two columns of interest. Slice the dataframe to include only vehicular sources
SCC_Q5 <- select(SCC, SCC, EI.Sector)
SCC_Q5 <- SCC_Q5[grep("Vehicles", SCC_Q5$EI.Sector), ]

## Create the fifth dataframe by merging on the SCC column, filtering by the desired county, 
## grouping by year and sector and finding the median emissions
NEI_Q5 <- merge(NEI, SCC_Q5) %>%
        filter(fips == "24510") %>%
        group_by(year, EI.Sector) %>%
        summarise(median_by_source = median(Emissions))

## Plot the medians, add an appropriate title, axis labels and a legend title, and use facets for the four graphs
png(filename = "./output2/plot5.png", width = 960, height = 480)
ggplot(NEI_Q5, aes(year,median_by_source)) + 
        labs(title = "1999-2008 median motor vehicle emissions in Baltimore City",
             x = "Year", y = "Median PM2.5 (tonnes)") + 
        geom_point() + geom_line() + facet_wrap(~EI.Sector, scales = "free")
dev.off()


## Create a dataframe from the SCC table and select the two columns of interest. Slice the dataframe to include only vehicular sources
SCC_Q6 <- select(SCC, SCC, EI.Sector)
SCC_Q6 <- SCC_Q6[grep("Vehicles", SCC_Q6$EI.Sector), ]

## Create the sixth dataframe by merging on the SCC column, filtering by the desired counties, 
## grouping by year and sector and finding the median emissions
NEI_Q6 <- merge(NEI, SCC_Q6) %>%
        filter(fips == "24510" | fips == "06037") %>%
        group_by(year, EI.Sector, fips) %>%
        summarise(median_by_source = median(Emissions))

NEI_Q6$fips[NEI_Q6$fips == "24510"] <- "Baltimore City"
NEI_Q6$fips[NEI_Q6$fips == "06037"] <- "Los Angeles County"

## Plot the medians, apply the group function, add an appropriate title, axis labels and a legend title, and use facets for the four graphs
png(filename = "./output2/plot6.png", width = 960, height = 480)
ggplot(NEI_Q6, aes(year,median_by_source, group=fips, color=fips)) + 
        labs(title = "1999-2008 median motor vehicle emissions: Baltimore City vs. Los Angeles County",
             x = "Year", y = "Median PM2.5 (tonnes)", color = "US County") + 
        geom_point() + geom_line() + facet_wrap(~EI.Sector, scales = "free")
dev.off()