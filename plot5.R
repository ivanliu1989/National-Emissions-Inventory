plot5 <- function(){
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
    criteria.1 <- "Motor"
    criteria.2 <- "Vehicle"
    index <- grepl(criteria.1,SCC$Short.Name, fix=F)
    index.2 <- grepl(criteria.2, SCC$Short.Name, fix=F)
    SCC.subset <- subset(x=SCC,subset=(index | index.2),select=c(1,3))
    
    # How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
    library(plyr)
    
    # Subset NEI datasets
    NEI.plot5 <- NEI[which(NEI$SCC %in% as.vector(SCC.subset$SCC)),]
    NEI.plot5 <- NEI.plot5[which(NEI.plot5$fips == "24510"),]
    
    # Sum PM2.5 value by year
    NEI.plot5 <- ddply(NEI.plot5, .(year),summarize, totalbyyear=sum(Emissions))
    
    #generate and output bar chart 
    library(ggplot2)
    png("plot5.png", height=480,width=480)
    g <- ggplot(NEI.plot5, aes(year, totalbyyear))
    g + geom_point(size = 6) + labs(x="Year") + labs(y="PM2.5 Emissions") + 
        geom_smooth(method="lm", size=1.5, linetype=3,se=F) +
        labs(title=paste("Emissions from Motor Vehicle Sources in Baltimore City","during 1999 and 2008",sep="\n"))
    dev.off()
}