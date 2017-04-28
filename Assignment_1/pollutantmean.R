pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    if (!file.exists(directory)) {
        return("Invalid directory")
    }
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    if ( (pollutant != "sulfate") & (pollutant != "nitrate") ) {
        return("Invalid pollutant")
    }
    
    #improvements to the guard conditions
    # 1) if directory != "specdata"
    # 2) if (!file.info(directory)$isDir)
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    # Guard test TBD
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    # obtain a file listing to include the directory as prefix
    pollutant_data_files <- list.files(directory, full.names = T)
        
    # initialize the target data holder
    pollutant_data_frame <- data.frame()
    
    # load the range of data files
    for (i in id) {
        pollutant_data_frame <- rbind(pollutant_data_frame, read.csv(pollutant_data_files[i]))
    }

    # calculate the mean of the target pollutant
    result <- mean(pollutant_data_frame[, pollutant], na.rm = T)
    
    # format the result to 3 decimal places
    sprintf("%.3f", result)
}