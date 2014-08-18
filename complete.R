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
    
    # get number of complete cases
    nobs <- function(id) {
       
       # get file path
       path <- paste(c(getwd(),"/",directory),collapse="")
       fullpath <- file.path(path, paste(sprintf("%03d", as.numeric(id)), ".csv", sep=""))
    
        # read files into R and return number of complete cases
        return (sum(complete.cases(read.csv(fullpath))))
    
    }
    
    # return the love
    return (data.frame(id=id, nobs=sapply(id,nobs)))
      
}
