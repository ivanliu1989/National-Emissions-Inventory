plot3 <- function(){
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
    NEI.Maryland <- NEI[which(NEI$fips=="24510" & NEI$year %in% c(1999, 2002,2005,2008)),c(1,4,5,6)]
    
    # Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
    # variable, which of these four sources have seen decreases in emissions from 1999–2008 for
    # Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2
    # plotting system to make a plot answer this question.
    library(plyr)
    
    # Sum PM2.5 value by year
    NEI.plot3 <- ddply(NEI.Maryland, .(year,type),summarize, totalbyyear=sum(Emissions))
        
    #generate and output bar chart 
    library(ggplot2)
    png("plot3.png", height=480,width=480)
    g <- ggplot(NEI.plot3, aes(year, totalbyyear))
    g + geom_point(size=4,aes(color=type)) + 
        facet_grid(.~type) + 
        geom_smooth(method="lm", size=1.5, linetype=3,se=F) + 
        theme_bw(base_family = "Times", base_size=12) + 
        labs(x = "Year") + labs(y = "Total PM2.5 Emissions") +
        labs(title="Total PM2.5 Emissions of Four types across 1999 to 2008") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    dev.off()
}