##Second assignment: GEt the best, worst or specific rank hospital in a state given
##specified cause of death.

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data <- outcome
data[, 11] <- as.numeric(outcome[, 11])
data[, 17] <- as.numeric(outcome[, 17])
data[, 23] <- as.numeric(outcome[, 23])

##Function (quite similar to the first) :
rankhospital <- function(state, outcome, num = "best") {
        if (outcome == "heart attack") {
                x <- 11
        } else if (outcome == "heart failure") {
                x <- 17
        } else if (outcome == "pneumonia") {
                x <- 23
        }else
                stop("invalid outcome")
        selected<-subset(data,data$State==state)
        if (length(selected$State)==0){
                stop(("invalid state"))}
        selected<-selected[order(selected[,2]),]
        selected<-selected[order(selected[,x]),]
        selected<-na.omit(selected)
        y<-num
        if (num=="best") {
                y<-1
        }
        if(num=="worst") {
                y<-as.numeric(nrow(selected))
        }
        selected[y,2]
}

##test it out
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("TX", "heart attack", 5000)