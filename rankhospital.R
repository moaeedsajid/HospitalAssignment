# rankhospital.R
# Moaeed Sajid
# V1 1/12/17

# Args (state, outcome, num = "best)
# Return hospital for a particular chosen state, outcome and position

rankhospital <- function(state, outcome, num = "best") {
        
        #install.packages('plyr')
        #library('plyr')
        
        ##Task - Read outcome data
        outcomef <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##Task - Check the state and outcome are valid
        #Columns are heart attack [,11], heart failure [,17], pneumonia [,23]
        
        #Check if state entered is in the state list
        statevalid <- state %in% c(unique(outcomef$State)) 
        if (statevalid == "FALSE") {
                stop ("Invalid State")
        }
        
        # IMPROVED THIS BELOW - Check outcome is one of three predefined ones
        #outcomevalid <- outcome %in% c("heart attack", "heart failure", "pneumonia")
        #if (outcomevalid == "FALSE") {
        #        stop ("Invalid Outcome")
        #}
        
        # Choosing the correct colum in table for outcome, else stop for invalid outcome
        outcomecol <- "NULL"
        
        if (outcome == "heart attack") {
                outcomecol <- 11
        }
        if (outcome == "heart failure") {
                outcomecol <- 17
        }
        if (outcome == "pneumonia") {
                outcomecol <- 23
        }
        if (outcomecol == "NULL") {
                stop ("Invalid Outcome")
        }
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        #Retrieve a subset of data with just the state
        substate <- subset(outcomef, (State == state))
        
        #Reduce data further with just the 2 rows that concern us
        filtrows <- (substate[c(2,outcomecol)])
        
        # Remove not available before converting to numeric
        totalavail <- subset(filtrows, (filtrows[,2] != 'Not Available'))
        totalavail[,2] <- as.numeric(totalavail[,2]) 
        
        # Sort by outcome and then name before counting the total results
        arrangeta <- arrange(totalavail,totalavail[,2],totalavail[,1])
        countta <- nrow (arrangeta)
        #print (countta)
        
        #Calculate ranking 
        
        if (num == "best") {
                num <- 1
        }
        if (num == "worst") {
                num <- countta
        }
        
        #Convert to numeric
        ranking <- as.numeric(num)
        
        #print(ranking)
        if (countta < ranking) {
                print ("NA")
        }
        else {
        print(arrangeta[ranking,1])
        }
        
}