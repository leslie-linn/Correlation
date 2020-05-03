corr<-function(directory, threshold=0){
        
        files.list=list.files(directory, full.names=TRUE, pattern=".csv")
        cors <- rep(0, length(files.list))
        
        for(i in 1:length(files.list)){
                
                data<-read.csv(files.list[i], header = TRUE) 
                data.cor<-na.omit(data[,2:3])
                nobs<-nrow(data.cor)
                
                if(nobs > threshold){
                        
                        cors[i]<-cor(data.cor[,1], data.cor[,2])
                        
                }else{
                        cors[i] <- 0
                }
        }
        return(cors)
}
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
