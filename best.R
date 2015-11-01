best<-function(state, outcome){
        ## Read outcome data
        out<-c(NA)
        dat<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        checkState<-unique(dat$State)
        checkOutcome<-c("heart attack", "heart failure", "pneumonia")
        
        if (!state %in% checkState) stop("invalid state")
        
        if(!outcome %in% checkOutcome) stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        dat2 <- dat[dat$State == state, ]
        dat2[, c(11, 17, 23)] <- sapply(dat2[, c(11, 17, 23)], as.numeric)
        dat2 <- dat2[order(dat2[, 2]), ]
        
        if (outcome == "heart attack") {
                out <- dat2[which.min(dat2[, 11]), "Hospital.Name"]
        }
        else if (outcome == "heart failure") {
                out <- dat2[which.min(dat2[, 17]), "Hospital.Name"]
        }
        else if (outcome == "pneumonia") {
                out <- dat2[which.min(dat2[, 23]), "Hospital.Name"]
        }
        
        out
}
