# rankall.R
# Moaeed Sajid
#V1.1 1/12/17
#Updated so global num value is not being reset in the for loop

# V1 1/12/17
# Args (outcome, num = "best)
# Return dataframe of hospitals in each state for a particular outcome and position

rankall <- function(outcome, num = "best") {
        
        #install.packages('plyr')
        #library('plyr')
        
        ##Task - Read outcome data
        outcomef <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # Initialising Values
        ustate <- sort(c(unique(outcomef$State)))
        rankallrslt <- data.frame()
        cnames = c("hospital", "state")
        outcomecol <- "NULL"
        
        ##Task - Check the outcome is valid
        #Columns are heart attack [,11], heart failure [,17], pneumonia [,23]
        # Choosing the correct colum in table for outcome, else stop for invalid outcome
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
        
        
        ## For each state, find the hospital of the given rank
        
        for (n in ustate) {
                
                state <- n
                #Retrieve a subset of data with just the state
                substate <- subset(outcomef, (State == state))
                
                #Reduce data further with just the 3 rows that concern us
                filtrows <- (substate[c(2,outcomecol,7)])
                
                # Remove not available before converting to numeric
                totalavail <- subset(filtrows, (filtrows[,2] != 'Not Available'))
                totalavail[,2] <- as.numeric(totalavail[,2]) 
                
                # Sort by outcome and then name before counting the total results
                arrangeta <- arrange(totalavail,totalavail[,2],totalavail[,1])
                countta <- nrow (arrangeta)
                #print (countta)
                
                #Calculate ranking 
                
                if (num == "best") {
                        statenum <- 1
                }
                if (num == "worst") {
                        statenum <- countta
                }
                
                #Convert to numeric
                ranking <- as.numeric(statenum)
                #print (ranking)
                
                #Add hospital for this state to the rankallrst dataframe
                if (countta < ranking) {
                        staterslt <- data.frame ((arrangeta[ranking,1]),state)
                        rankallrslt <- rbind(rankallrslt,staterslt)
                }
                else {
                        staterslt <- data.frame ((arrangeta[ranking,1]),state)
                        rankallrslt <- rbind(rankallrslt,staterslt)
                }
                
        }
        
        colnames(rankallrslt) <- cnames
        print (rankallrslt) 
        
}