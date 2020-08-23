## First Assignment
## Get the best hospiotal in a state in for a specific cause of death

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

names(outcome)
str(outcome)
outcome$Hospital.Name
outcome$State

outcome[, 11] #average death rate due to Heart attack
outcome[, 17] #average death rate due to Heart.Failure
outcome[, 23] #average death rate due to Pneumonia
outcome[, 11] <- as.numeric(outcome[, 11])
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])

#histogram of death rate from heart attack
hist(outcome[, 11])

##managing data
data <- outcome ## I'll use a data.frame called data instread of the one called outcome
data[, 11] <- as.numeric(outcome[, 11])
data[, 17] <- as.numeric(outcome[, 17])
data[, 23] <- as.numeric(outcome[, 23])

##Function:
best <- function(state, outcome) {
        if (outcome == "heart attack") {
                x <- 11
        } else if (outcome == "heart failure") {
                x <- 17
        } else if (outcome == "pneumonia") {
                x <- 23
                        }else
                stop("invalid outcome")
        selected<-subset(data,data$State==state)
        if(length(selected$State)==0){
                stop(("invalid state"))}
        z<-min(selected[,x],na.rm = TRUE)
        selected<-subset(selected,selected[,x]==z)
        #selected<-selected[,x]
        selected<-selected[order(selected$Hospital.Name),]
        selected[1,2]
}

#try it out
best("NY", "heart attack")
best("MD", "heart failure")
best("MD", "pneumonia")

