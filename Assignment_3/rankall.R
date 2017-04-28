rankall <- function(outcome, num = "best") {
    
    # list of all accepted outcome inputs
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    # Validate outcome parameter
    if ( !(outcome %in% outcomes) ) { stop("invalid outcome") } 

    # Extract the column indices
    index <- if (outcome == outcomes[1]) {
        11
    } else if (outcome == outcomes[2]) {
        17
    } else {
        23
    }
    
    # Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", header = T, sep = ",", colClasses="character", na.strings=c("Not Available", "NA"))
    
    # load the state data from the spreadsheet
    states <- unique(data[["State"]])
    
    # sort the state data
    states <- sort(states)
    
    # compute this once
    number_states <- length(states)
    
    # Initialize the matrix for our data
    m <- matrix(, nrow = number_states, ncol = 2)
    
    # Set column headers
    colnames(m) <- c("hospital", "state")
    
    # Build the results dataframe
    for (i in 1:number_states) {

        # Extract the hospital/outcome data for the current state
        state_data <- data[data$State == states[i], c(2, `index`)]   
    
        # Remove NAs
        noNAs <- complete.cases(state_data)
    
        # Dataframe without unwanted data 
        state_data <- state_data[noNAs, ]
        
        # Make more readable names
        colnames(state_data) <- c("Hospital", "Outcome")
        
        # Compute this once
        observations <- nrow(state_data)
        
        # Determine the values for number
        if (num == "best") {        rank <- 1 } 
        else if (num == "worst") {  rank <- observations } 
        else {                      rank <- num }
        
        # Check the boundaries
        if (rank < 1 | rank > observations) {
            nada <- c(NA, states[i])
            m[i, ] <- nada
            next
        }
        
        # Coerce values to be numerically sortable
        state_data[, 2] <- as.numeric(state_data[, 2])
 
        # Create an ordering vector
        ndx = order(state_data$Outcome, decreasing=F)
        
        # Put the vector in order
        state_data_sorted = state_data[ndx,]
        
        # Find the hospital that matches
        hospitals <- state_data_sorted[rank, ]
        
        # Break any ties    
        ties <- state_data_sorted[state_data_sorted$Outcome == hospitals[[2]], ]   
        names <- ties[, 1]
        winner <- sort(names, decreasing = F)        
        
        # Store the results
        finished <- c(winner[1], states[i])
        m[i, ] <- finished
    }
    
    # Return the results as a dataframe, per requirements
    as.data.frame(m)
}