rankall<-function(outcome, num="best"){
        ## Read outcome data
        out<-data.frame(NA)
        dat<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that outcome is valid
        checkOutcome<-c("heart attack", "heart failure", "pneumonia")
        
        if(!outcome %in% checkOutcome) stop("invalid outcome")
        names(dat)[c(11,17,23)] <- checkOutcome
        
        dat <- dat[,c("State","Hospital.Name", outcome)]
        dat[,outcome] <- suppressWarnings(as.numeric(dat[,outcome]))
        
        datSplit <- split(dat[, c("Hospital.Name", "State", outcome)], dat$State)
        ## Return data frame with the hospital names and the
        ## (abbreviated) state names
        rankH<-function(datSplit, num){
                orderedDatSplit <- order(datSplit[3], datSplit$Hospital.Name, na.last=NA)
                if (num == "best") {
                        datSplit$Hospital.Name[orderedDatSplit[1]]
                } else if (num == "worst") {
                        datSplit$Hospital.Name[orderedDatSplit[length(orderedDatSplit)]]
                } else if (is.numeric(num)) {
                        datSplit$Hospital.Name[orderedDatSplit[num]]
                } else {
                        stop("invalid num")
                }
        }
        
        pre_result <- lapply(datSplit, rankH, num)
        
        data.frame(hospital = unlist(pre_result), state = names(pre_result))
}