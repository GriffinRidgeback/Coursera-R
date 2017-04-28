complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # load the "tools" library for file name manipulation
    library(tools)
    
    # obtain a file listing to include the directory as prefix
    pollutant_data_files <- list.files(directory, full.names = T)
    
    # initialize the target data holder
    pollutant_data_frame <- data.frame()
    
    # load the range of data files
    for (i in id) {
        # variable pointing to the data
        data <- pollutant_data_files[i]
        
        # extract file name as an integer
        data_file <- get_file_name(data)
        
        # load data for analysis
        file <- read.csv(data)
        
        # create logical vector of completed cases
        good <- complete.cases(file)
        
        # get the count
        complete <- nrow(file[good, ])
        
        # store results in vector
        results <- c(data_file, complete)
        
        # add to the result data frame
        pollutant_data_frame <- rbind(pollutant_data_frame, results)
    }
    
    names(pollutant_data_frame) <- c("id", "nobs")
    pollutant_data_frame
}

# simple function to extract the file name from the path of the data files
get_file_name <- function(file_path) {
    path <- file_path_sans_ext(file_path)
    path_elements <- strsplit(path, "\\/")[[1]]
    filename <- path_elements[2]
    return(as.numeric(filename))        
}