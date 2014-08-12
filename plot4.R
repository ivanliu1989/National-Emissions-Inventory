plot4 <- function(){
    # set project work directory 
    setwd("C:/Documents and Settings/Macro/Desktop/Ivandata/National-Emissions-Inventory")
    
    # read data from provided data source
    nei.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    nei.file <- basename(nei.url)
    if(!(file.exists(nei.file)))
        download.file(nei.url,destfile=nei.file,method="auto")
    nei.file.list <- basename(unzip(nei.file))
    #nei.file.list
    dir()
    
    # This first line will likely take a few seconds. Be patient!
    NEI <- readRDS(nei.file.list[2])
    SCC <- readRDS(nei.file.list[1])
    
    # Subset SCC dataset
    criteria.1 <- "Coal"
    criteria.2 <- "Combustion"
    index <- grepl(criteria.1,SCC$Short.Name, fix=T)
    index.2 <- grepl(criteria.2, SCC$Short.Name, fix=T)
    SCC.index <- SCC[index & index.2,]
    
    # Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
    library(plyr)
    
    # Subset NEI datasets
    NEI.index <- SCC.index$SCC
    NEI.plot4 <- NEI[which(NEI$SCC %in% NEI.index),]
    
    # Sum PM2.5 value by year
    NEI.plot4 <- ddply(NEI.plot4, .(year),summarize, totalbyyear=sum(Emissions))
    
    #generate and output bar chart 
    library(ggplot2)
    png("plot4.png", height=480,width=480)
    g <- ggplot(NEI.plot4, aes(year, totalbyyear))
    g + geom_line() + labs(x="Year") + labs(y="PM2.5 Emissions") + 
        geom_smooth(method="lm", size=1.5, linetype=3,se=F) +
        labs(title=paste("Coal-related PM2.5 Emissions in US","during 1999 and 2008",sep="\n"))
    dev.off()
}