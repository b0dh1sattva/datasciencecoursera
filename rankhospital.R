rankhospital<-function(state, outcome, num="best"){
        ## Read outcome data
        out<-c(NA)
        dat<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        checkState<-unique(dat$State)
        checkOutcome<-c("heart attack", "heart failure", "pneumonia")
        
        if(!state %in% checkState) stop("invalid state")
        
        if(!outcome %in% checkOutcome) stop("invalid outcome")
        
        names(dat)[c(11,17,23)] <- checkOutcome
        
        dat <- dat[,c("State","Hospital.Name", outcome)]
        dat <- dat[dat$State == state, ]
        dat[,outcome] <- suppressWarnings(as.numeric(dat[,outcome]))
        dat <- dat[!is.na(dat[outcome]),]
        dat <- dat[order(dat$State, dat[outcome], dat$Hospital.Name),]
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        if (!is.numeric(num)){
                if (num == "best")
                        out<-dat[1,2]
                else if (num == "worst")
                        out<-tail(dat[, 2], 1)
        }else out<-dat[num,2]
        
        return(out)
}