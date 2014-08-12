plot1 <- function(){
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
    
    # Merge variables together to get a new dataset for analysis if required
    # For plot1, it is not required.
    
    # Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
    # Using the base plotting system, make a plot showing the total PM2.5 emission 
    # from all sources for each of the years 1999, 2002, 2005, and 2008.
    library(plyr)
        
    # Sum PM2.5 value by year
    NEI.plot1 <- ddply(NEI, .(year),summarize, totalbyyear=sum(Emissions))
    attach(NEI.plot1)
    library(RColorBrewer)
    cols <- brewer.pal(4,"Accent")
    pal <-colorRampPalette(cols)
    png("plot1.png", height=480,width=480)
    par(ps=12, bg="transparent",bty="l",las=1,cex=1, mar = c(3, 4, 3.5, 5))    
    barplot(totalbyyear,year, main="Total PM2.5 Emissions for 1999, 2002, 2005 and 2008", 
            col=pal(4),ylab="PM2.5 emissions")
    legend("topright",legend=c(1999,2002,2005,2008),lty=1,fill=pal(4),bty="n")
    detach(NEI.plot1)
    dev.off()
        
}