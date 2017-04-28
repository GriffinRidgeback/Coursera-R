corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    
    # obtain a file listing to include the directory as prefix
    pollutant_data_files <- list.files(directory, full.names = T)
    
    # get the loop index
    file_count <- length(pollutant_data_files)
    
    # initialize the result vector
    vector <- integer(0)
    
    for (i in 1:file_count) {
        
        # load data for analysis
        file <- read.csv(pollutant_data_files[i])
        
        # create logical vector of completed cases
        good <- complete.cases(file)
        
        # find valid observations
        observations <- file[good, ]
        
        # get the count
        complete <- nrow(observations)
        
        if(complete > threshold) {
            x <- observations["sulfate"]
            y <- observations["nitrate"]
            z <- cor(x,y)
            w <- z[1]
            # this matched the results but failed
            # the Coursera submission tests
            #w <- round(z[1], 5)
            vector <- c(vector, w)
        } 
    }
    
    vector
}