pollutantmean<-function(directory, pollutant, id=1:332) {
        colData<-c(NA)
        ##loops through each file in directory getting the column data
        ##and combining them all into one vector
        for(i in id) {
                i<-formatC(i,width=3,format="d",flag="0")
                filename<-paste(directory,"/",i,".csv",sep="")
                colData<-c(colData, read.csv(filename)[[pollutant]])
        }
        ##taking the mean of the resulting vector from the for loop with the NAs removed
        means<-mean(colData,na.rm=TRUE)
        return(means)
}
