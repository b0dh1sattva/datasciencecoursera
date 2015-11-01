complete<-function(directory, id=1:332){
        nobs<-c()
        out<-data.frame()
        for(i in id){
                i<-formatC(i,width=3,format="d",flag="0")
                filename<-paste("./",directory,"/",i,".csv",sep="")
                nobs<-c(nobs, sum(complete.cases(read.csv(filename))))
        }
        out<-cbind.data.frame(id,nobs)
        print(class(out))
        return(out)
}