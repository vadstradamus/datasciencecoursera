pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # get filepath - concat working dir and 'subfolder'directory'
  path <- paste(c(getwd(), "/", directory), collapse = "")
  
  # create subset of only needed files
  files <- list.files(path,full.names = TRUE)
  files <- files[id]
  
  # read files into R data frame
  df <- do.call(rbind,lapply(files,read.csv))
  
  # calculate mean if sulfate
  if (pollutant=="sulfate") {
    mean <- (mean(df$sulfate, na.rm=TRUE))  
  }
  
  # calculate mean if nitrate
  if (pollutant=="nitrate"){
    mean <- (mean(df$nitrate, na.rm=TRUE))   
  }

  # don't be...  
  mean

}

