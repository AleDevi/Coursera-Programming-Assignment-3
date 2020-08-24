#Ranking hospitals in all states

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data <- outcome
data[, 11] <- as.numeric(outcome[, 11])
data[, 17] <- as.numeric(outcome[, 17])
data[, 23] <- as.numeric(outcome[, 23])

##Function (quite similar to the first) :
rankall <- function(outcome, num = "best") {
        if (outcome == "heart attack") { #defining where the outcomes should search (wich column)
                x <- 11
        } else if (outcome == "heart failure") {
                x <- 17
        } else if (outcome == "pneumonia") {
                x <- 23
        }else
                stop("invalid outcome")
        selected<-data[order(data[,2]),] #sorts everything by hospital name
        selected<-selected[order(selected[,x]),] #ranks by outcomes
        selected<-selected[order(selected[,7]),] #ranks by state
        selected<-selected[!is.na(selected[,x]),] #gets only the hospitals with a valid "outcome"
        selectedL<-split(selected,selected$State) #creates a list of data.frames. Each is a state (with inside hospitals ordered by rank in outcome and alpabetically)
              my.mat<-data.frame(row.names=names(selectedL), #creates a dataframe (my.mat) from the list applying a simple function that just get the outcome specified before
                                                             #the dataframe has specifie row names and 2 colums:
                                 sapply(selectedL,function(X){ #this is the first column of the data.frame:
                           y<-num
                           if (num=="best") {
                             y<-1
                           }
                           if(num=="worst") {
                             y<-as.numeric(nrow(X))
                           }
                           X[y,2]}), #the specified ranked (y) name of the hospital in each dataframe of the list (position 2)
                         names(selectedL)#this is the second coloumn: the state, wich corresponds to the dataframe name in the list
                         )
      colnames(my.mat)<-c("hospital","state") #this specifies the coloums names
     my.mat
            }

##try it out
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
