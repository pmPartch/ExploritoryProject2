#plot 4

#Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

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
if (require("reshape2") == FALSE)
{
    install.packages("reshape2")
    library(reshape2)
}

#looking for coal only sources, so use SCC.Level.Three and create a regular expression to search for 
#iether Coal or Lignite
coalOnly <- grepl("^.*Coal|Lignite.*$", data_classification$SCC.Level.Three )

#having issues with creating syntax for word-not-followed-by-word with R regex...so split into to grep's
CoalMining <- grepl("^.*Coal Mining.*$", data_classification$SCC.Level.Three )

#find the SCC numbers based on the classification search
coalSCCids <- data_classification[coalOnly & !CoalMining,]$SCC

coalOnlyData <- data_summarySCC[data_summarySCC$SCC %in% coalSCCids,]

#remove unneeded columns (SCC, Polluntant, type)
coalOnlyData2 <- coalOnlyData[,-c(2,3,5)]

sum(!complete.cases(coalOnlyData2)) == 0 #verify no missing data (should be TRUE)

#now reshape the data
coalmdf <- melt(coalOnlyData2, id=c("fips","year"))

finalCoaldf <- dcast(coalmdf, year~variable,sum)

attach(finalCoaldf)

opar <- par(no.readonly=TRUE) #save original par settings

#Plot4 creation

png("plot4.png") #note: width and height is default 480 pixels, so no need to specify size

#note: I decided to plot points, but make the points large and use color compliments (red and blue) to make them 'pop'. I did not
#      wish to use a line chart (implies data continuity that does not exist) and did not think that bar plot was appropriate.
#      Also used a dashed line for the linear fit to help illustrate the trend line

plot(year, Emissions, pch=21, col="blue",bg="red", xlab="year", ylab=expression("Emissions (Tons)"), main="Total Coal Emissions Across US")
abline(lm(Emissions ~ year, finalCoaldf), lwd = 1, lty=3, col="blue")

par(opar)

dev.off() #close and save the file to disk

detach(finalCoaldf)
