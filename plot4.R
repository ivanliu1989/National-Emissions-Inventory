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
    
    # Merge datasets
    NEI.all <- merge(NEI,SCC[,c(1,3)], by="SCC",all=F)
    # Subset NEI dataset
    a <- "Coal"
    b <- "combustion"
    index <- grepl(a,SCC$Short.Name, fix=T)
    index.2 <- grepl(b, SCC$Short.Name, fix=T)
    SCC.index <- SCC[index,]
    
    # Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
    library(plyr)
    
    # Sum PM2.5 value by year
    NEI.plot4 <- ddply(NEI.Maryland, .(year,type),summarize, totalbyyear=sum(Emissions))
    
    #generate and output bar chart 
    library(ggplot2)
    png("plot4.png", height=480,width=480)
    g <- ggplot(NEI.plot4, aes(year, totalbyyear))
    
    dev.off()
}