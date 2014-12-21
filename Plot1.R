#plot 1

#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from
#all sources for each of the years 1999, 2002, 2005, and 2008.

#note: I always create R scripts that can be run stand-alone
#      (that is, I do not assume that the current working directory contains the raw data files)
#      So, I check if the files currently exist. If they don't, then I attempt to download them

fileClassUrl <- "Source_Classification_Code.rds"
fileSummryUrl <- "summarySCC_PM25.rds"

if (file.exists(fileClassUrl) == FALSE || file.exists(fileSummryUrl) == FALSE)
{
    #attempt to download and extract the data file
    
    filezipUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    
    temp <- tempfile()
    
    download.file(filezipUrl,temp, method='curl')
    
    unzip(temp)    #unzip file to local working directory
    
    unlink(temp)   #remove temp file since no longer needed
}

#read the data into memory
data_classification <- readRDS(fileClassUrl)

data_summarySCC <- readRDS(fileSummryUrl)


#some info on what is contained:
#data_classification : 11717 samples of 15 variables (lots of missing values not all marked as NA). The SCC column is unique IDs
#data_summarySCC : 6497651 samples of 6 variables (all data is available (not NA)). Only 1999, 2002, 2005 and 2008 are contained

#load packages
if (require("dplyr") == FALSE)
{
    install.packages("dplyr")
    library(dplyr)
}

#group the data by year and calculate the sum of Emissions
plotData <- group_by(data_summarySCC, year) %>%
    summarize(total = sum(Emissions))

attach(plotData)

opar <- par(no.readonly=TRUE) #save original par settings

#Plot1 creation

png("plot1.png") #note: width and height is default 480 pixels, so no need to specify size

#note: I decided to plot points, but make the points large and use color compliments (red and blue) to make them 'pop'. I did not
#      wish to use a line chart (implies data continuity that does not exist) and did not think that bar plot was appropriate.
#      Also used a dashed line for the linear fit to help illustrate the trend line

plot(year, total, pch=21, col="blue",bg="red", xlab="year", ylab=expression("PM"[2.5]*" (Tons)"), main="Total Emissions All Sources")
abline(lm(total ~ year, plotData), lwd = 1, lty=3, col="blue")

par(opar)

dev.off() #close and save the file to disk

detach(plotData)
