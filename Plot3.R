#plot 3

#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
#Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

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

if (require("ggplot2") == FALSE)
{
    install.packages("ggplot2")
    library(ggplot2)
}

#note the following:
#data_classification$Data.Category is a 6 level factor: (Biogenic,Event,Nonpoint,Nonroad,Onroad,Point) with no NA values
#data_summarySCC$type is a string with 4 values: ("POINT","NONPOINT","ON-ROAD","NON-ROAD") with no NA values

plotDataBalt <- mutate(data_summarySCC, type = as.factor(type)) %>%
    group_by(year, type) %>%
    filter(fips == "24510") %>%
    mutate(totalsByYear = sum(Emissions) ) 

#Plot3 creation

png("plot3.png") #note: width and height is default 480 pixels, so no need to specify size

#note: I decided to plot points, but make the points large and use color compliments (red and blue) to make them 'pop'. I did not
#      wish to use a line chart (implies data continuity that does not exist) and did not think that bar plot was appropriate.
#      Also used a dashed line for the linear fit to help illustrate the trend line

qplot(year, totalsByYear, data=plotDataBalt, facets = . ~ type, geom = c("point","smooth"), method = "lm")


dev.off() #close and save the file to disk
