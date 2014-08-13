plot6 <- function(){
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
    
    # Compare emissions from motor vehicle sources in Baltimore City with emissions from 
    # motor vehicle sources in Los Angeles County, California (fips == "06037"). 
    # Which city has seen greater changes over time in motor vehicle emissions?
    library(plyr)
    
    # Subset NEI datasets
    city.Baltimore <- "24510"
    city.Angeles <- "06037"
    NEI.plot6 <- NEI[which(NEI$SCC %in% as.vector(SCC.subset$SCC)),]
    NEI.plot6 <- NEI.plot6[which(NEI.plot6$fips %in% c(city.Baltimore, city.Angeles)),]
    
    # Sum PM2.5 value by year
    ct <- matrix(c("24510","06037","Baltimore","Angeles"),nrow=2)
    colnames(ct)<-c("fips","city")
    NEI.plot6 <- merge(NEI.plot6, ct, by="fips")
    NEI.plot6 <- ddply(NEI.plot6, .(year, city),summarize, totalbyyear=sum(Emissions))
    
    # generate and output bar chart 
    library(ggplot2)
    png("plot6.png", height=480,width=480)
    g <- ggplot(NEI.plot6, aes(year, totalbyyear))
    g + geom_point(size = 6, aes(color=city)) + labs(x="Year") + labs(y="PM2.5 Emissions") + 
        geom_smooth(method="lm", size=1.5, linetype=3,se=F) +
        labs(title=paste("Emissions from Motor Vehicle Sources","during 1999 and 2008",sep="\n")) +
        facet_grid(.~city)
    dev.off()
}