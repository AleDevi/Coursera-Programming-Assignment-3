## Plot the 30-day mortality rates for heart attack

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

#histogram of death rate from heart attack
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

#returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
#in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
#be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.
str(outcome)
outcome$Hospital.Name
outcome$State
outcome$State
names(outcome)


outcome$Hospital.Name
outcome[, 11] #average death rate due to Heart attack
outcome[, 17] #average death rate due to Heart.Failure
outcome[, 23] #average death rate due to Pneumonia
outcome[, 11] <- as.numeric(outcome[, 11])
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])
data <- outcome

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

best("NY", "heart attack")
best("MD", "heart failure")
best("MD", "pneumonia")

selected<-selected[order(selected$Hospital.Name),]

