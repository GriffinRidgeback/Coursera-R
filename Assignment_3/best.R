best <- function(state, outcome) {
    
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
    
    # Convert from character to double
    mort <- as.double(results[, 2])
    
    # Find minimum
    lowest <- which.min(mort)
    
    # Find the hospital that matches
    hospitals <- results[lowest, ]
    
    # Return it
    as.vector(hospitals[1], mode = "character")
}