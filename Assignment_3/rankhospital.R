rankhospital <- function(state, outcome, num = "best") {
    
    # Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", header = T, sep = ",", colClasses="character", na.strings=c("Not Available", "NA"))
    
    # Check that state and outcome are valid
    
    # list of all accepted outcome inputs
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if ( !(outcome %in% outcomes) ) { stop("invalid outcome") } 
    
    # load the state data from the spreadsheet
    states <- unique(data[["State"]])
    if ( !(state %in% states) ) { stop("invalid state") }
    
    # Extract the column indices
    index <- if (outcome == outcomes[1]) {
        11
    } else if (outcome == outcomes[2]) {
        17
    } else {
        23
    }
    
    # Filter by state, column of interest
    results <- data[data$State == state, c(2, `index`)]
    
    # Make more readable names
    colnames(results) <- c("Hospital", "Outcome")
    
    # Logical vector to remove unwanted data
    results_noNA <- complete.cases(results)
    
    # Dataframe without unwanted data 
    results <- results[results_noNA, ]
    
    # Determine the values for number
    if (num == "best") {
        rank <- 1
    } else if (num == "worst") {
        rank <- nrow(results)
    } else {
        rank <- num
    }
    
    # Check the boundaries
    if (rank < 1 | rank > nrow(results)) {
        return(as.vector(NA))
    }
    
    # Create an ordering vector
    v <- as.numeric(results[, 2])
    results[, 2] <- v
    ndx = order(results$Outcome, decreasing=F)
    
    # Put the vector in order
    results_sorted = results[ndx,]
    
    # Find the hospital that matches
    hospitals <- results_sorted[rank, ]
    
    ties <- results_sorted[results_sorted$Outcome == hospitals[[2]], ]   
    names <- ties[, 1]
    winner <- sort(names, decreasing = T)
    
    # Return it
    as.vector(winner[1], mode = "character")
}