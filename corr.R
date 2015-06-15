corr <- function(directory, threshold = 0){
  ##perform a pearson correlation on all complete observations
  ##if the total number of complete observations is
  ##above the provided threshold
  
  x <- vector("numeric", length = 332)

  #initialize an empty vector
  corrResults <-vector("numeric")

  #define the directory path
  dir <- paste(getwd(),"/",directory,"/",sep="")
  
  #get all the files in the directory into a character vextor
  dirFiles <- list.files(dir,pattern="*csv",full.names=TRUE)
  
  ##process the vector of id's passed to the function
  for (i in seq_along(dirFiles)){
    ##read in the entire file contents
    tempFile <- read.csv(dirFiles[i],sep=",",header=TRUE)
    
    ##now, subset the entire data file based on the complete
    ##cases. This will process each column at a time and get
    ##rid of any rows that have a NA for any column value.
    cmpCases <- tempFile[complete.cases(tempFile),]
    
    if(nrow(cmpCases) >= threshold){
      ##call correlation function using subsets of the completed cases vector
      corrResults <- c(corrResults,cor(cmpCases$sulfate, cmpCases$nitrate))
      
    }
  }
  corrResults
}