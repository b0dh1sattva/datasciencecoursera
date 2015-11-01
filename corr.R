corr<-function(directory, threshold=0){
        cors<-c()
        x<-1
        tempNoNA<-data.frame()
        for(i in 1:332){
                i<-formatC(i,width=3,format="d",flag="0")
                filename<-paste("./",directory,"/",i,".csv",sep="")
                countComplete<-sum(complete.cases(read.csv(filename)))
                tempNoNA<-na.omit(read.csv(filename))
                x<-tempNoNA[["sulfate"]]
                y<-tempNoNA[["nitrate"]]
                if(countComplete > threshold){
                        cors<-c(cors,cor(x,y))
                }
        }
        return(cors)
}