#depends on fread and plyr packages  
require(plyr)
require(data.table)
require(ggplot2)
csv_path <- "K:/Kamal/R-Project/big/utility_csv/"
csv_files <- list.files(path=csv_path,pattern="*.csv$",full.names=TRUE)
output_dir<- "K:/Kamal/R-Project/big/output/"
pdf_name <- paste(output_dir,substring(csv_files[1],36,121),".pdf",sep="")

for(c in csv_files)
{
    temp_csv_df<-data.frame(data.table::fread(paste(c)))
    if(!exists("csv_df")) csv_df<- temp_csv_df else csv_df<-rbind.fill(csv_df,temp_csv_df)
    rm(temp_csv_df)
    csv_df<-subset(csv_df,DateLocal>="2014-08-01 00:00:00" & DateLocal<="2014-08-31 23:30:00")    # subsetting data for august 2014
}

n_meters<-unique(csv_df$Meter)     # utility meters
pdf(file=pdf_name, width=11.2, height=8,  pointsize = 30)    # generate pdf file
for(m in n_meters)
{
    utility_data<-csv_df[csv_df$Meter==m,]
    utility_data$DateLocal<-as.POSIXct(utility_data$DateLocal)
    utility_data$kWh<-as.numeric(utility_data$kWh)
   
    y_min <- as.numeric(min(utility_data$kWh))
    y_min <- if(y_min < 0) y_min <- y_min*1.15 else y_min <- 0  
    y_max <- as.numeric(max(utility_data$kWh))*2
    
    #define min and max of x
    utility_data$DateLocal<-as.POSIXct(utility_data$DateLocal)
    x_min <- min(utility_data$DateLocal) - 4*60*60
    x_max <- max(utility_data$DateLocal) + 4*60*60
   
    p <- ggplot(data=utility_data, aes(x =  DateLocal, y=blank))
    
    p <- p + geom_line(aes(y = kWh), size = .2, na.rm=T)#utility data
    
    y_plotrange <- c(y_min,y_max)
    x_plotrange <- c(x_min, x_max)
    p <- p + coord_cartesian(ylim = y_plotrange) + coord_cartesian(xlim = x_plotrange)
    
    
    p <- p + xlab("Local Timestamp") + scale_x_datetime(breaks = "1 day") + 
        theme(axis.title.x = element_text(size = 12,face="bold",color="#3385FF"))+theme(axis.text.x=element_text(angle=90,size=9,vjust=0.6,color="black"))
    
    p <- p + ylab("Consumption (kWh)") + theme(axis.title.y = element_text(size = 12,face="bold",color="#3385FF"))
    
    
    p <- p + ggtitle(paste("Utility Meter : ", m, sep="")) + theme(plot.title = element_text(size = 14,colour="#1F3D5C",face="bold"))
    
    
    p<-p+theme(axis.text.y=element_text(face="bold",size=9,color="black"))
   
    print(p)
    
    rm(utility_data)
}
dev.off()
