corr <- function(directory, threshold = 0) {
    
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
# create folder path - concat wd and sub-folder
path <- paste(c(getwd(),"/",directory),collapse="")

# read csv files into dataframe
files <- dir(path, pattern = "*.csv", full.names = TRUE)

# call complete function
comp <- subset(complete(directory, 1:332), nobs > threshold)

# initialize vector
output <- numeric(0)

# iterate ids
for (i in comp$id){

    # create full filename
    fullpath <- paste(path, sprintf("%03d.csv", as.numeric(i)), sep="/",collapse="")
    
    # load csv, excluding NA
    df <- na.omit(read.csv(fullpath, colClasses=c("Date","numeric","numeric","integer")))
    
    # execute cor function
    output <- c(output, cor(df$sulfate, df$nitrate, use="na.or.complete"))
    #output <- round(output,digits=5)
}

# spit some knowledge
output

}
