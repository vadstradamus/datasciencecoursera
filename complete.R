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
  

  nobs <- function(id) {
    
    # get filepath - concat working dir and 'subfolder'directory'
    path <-paste(c(getwd(),"/",directory),collapse="")
    
    # create subset of only needed files
    files <- list.files(path,full.names = TRUE)
    files <- files[id]
    
    # read files into R data frame
    df <- do.call(rbind,lapply(files,read.csv))
    
    # return sum of completed cases
    return (sum(complete.cases(read.csv(files))))
    
  }
  
  return (data.frame(id=id, nobs=sapply(id,nobs)))
  
}