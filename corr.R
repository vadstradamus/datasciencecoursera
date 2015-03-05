corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  

#   complete <- complete(directory)
  
  # establish path and files contained therein
  path <-paste(c(getwd(),"/",directory),collapse="")
  files <- list.files(path,full.names = TRUE)
  
  # populate data frame observations that meet threshold
  df <- subset(complete(directory), nobs > threshold)
  
  # get ids from that data frame
  ids <- df[,"id"]
  
  # initialize numeric vector
  output <- numeric(0)
  
  # initerate files
  for (i in df$id) {
    
    file <- files[i]
    
    filedf <- read.csv(file)
    
    output <- c(output, cor(filedf$sulfate, filedf$nitrate, use = "pairwise.complete.obs")) 

  }
  
  return (output)

  
}