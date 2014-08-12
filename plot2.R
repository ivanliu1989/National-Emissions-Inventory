plot2 <- function(){
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
    
    # Subset NEI dataset
    NEI.Maryland <- NEI[which(NEI$fips=="24510" & NEI$year %in% c(1999, 2002,2005,2008)),c(1,4,6)]
    
    # Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
    # Use the base plotting system to make a plot answering this question.
    library(plyr)
    
    # Sum PM2.5 value by year
    NEI.plot2 <- ddply(NEI.Maryland, .(year),summarize, totalbyyear=sum(Emissions))
    attach(NEI.plot2)
    
    # Allocate color to different years
    library(RColorBrewer)
    cols <- brewer.pal(4,"Accent")
    pal <-colorRampPalette(cols)
    
    #generate and output bar chart 
    png("plot2.png", height=480,width=480)
    par(ps=12, bg="transparent",bty="l",las=1,cex=1, mar = c(3, 4, 3.5, 5))    
    barplot(totalbyyear,year, main=paste("Total PM2.5 Emissions in the Baltimore City", "in 1999, 2002, 2005 and 2008",sep="\n"), 
            col=pal(4),ylab="PM2.5 emissions")
    legend("topright",legend=c(1999,2002,2005,2008),lty=1,fill=pal(4),bty="n")
    detach(NEI.plot2)
    dev.off()
}